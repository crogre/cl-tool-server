(in-package #:grit)

;;; AI autopilot for rebase conflicts (r R): each conflicted file goes through
;;; a configured command (prompt and file on stdin, resolution on stdout),
;;; then an optional verify command runs, then the rebase continues, until it
;;; is done or something fails.
;;;
;;;   ~/.config/grit/config.toml   user defaults ($XDG_CONFIG_HOME respected)
;;;   ~/.config/grit/resolve.md    user prompt
;;;   <repo>/.grit/config.toml     project overrides, per field
;;;   <repo>/.grit/resolve.md      project prompt
;;;
;;;   [ai]
;;;   command = "claude -p"
;;;   verify_command = "cargo check --quiet"
;;;
;;; GRIT_AI_RESOLVE_COMMAND and GRIT_AI_VERIFY_COMMAND override the files.
;;; Environment variables are the server's: the client does not send its own.

(defparameter *max-resolve-bytes* (* 256 1024)
  "Larger conflicted files are left to the user: they tend to be lock files or
generated, where a guessed resolution does more harm than good.")

(defparameter *autopilot-iterations* 20)

(defparameter *default-prompt* "You are resolving a Git merge conflict. The file content below contains conflict markers (`<<<<<<<`, `=======`, `>>>>>>>`). Output ONLY the resolved file content — no commentary, no markdown fences, no preamble. Preserve the original file's formatting, indentation, and trailing newline. Choose the merge that best reflects the intent of both sides; if it is ambiguous, prefer the side that does not introduce new behavior.

Conflicted file content follows:")

(defun getenv* (name)
  (let ((value (sb-posix:getenv name)))
    (and value (plusp (length value)) value)))

(defun parse-toml-string (text start)
  "The TOML basic or literal string starting at START in TEXT, or NIL."
  (let ((quote (char text start)))
    (case quote
      (#\' (let ((end (position #\' text :start (1+ start))))
             (and end (subseq text (1+ start) end))))
      (#\" (with-output-to-string (out)
             (loop with i = (1+ start)
                   while (< i (length text))
                   do (let ((char (char text i)))
                        (cond ((char= char #\") (return))
                              ((and (char= char #\\) (< (1+ i) (length text)))
                               (incf i)
                               (write-char (case (char text i) (#\n #\Newline) (#\t #\Tab)
                                             (t (char text i)))
                                           out))
                              (t (write-char char out)))
                        (incf i))
                   finally (return-from parse-toml-string nil)))))))

(defun parse-ai-config (text)
  "An alist of the string keys in TEXT's [ai] table. Lines it cannot read are
skipped, so a broken file costs only its broken lines."
  (let ((section nil) (result '()))
    (dolist (line (split text #\Newline) (nreverse result))
      (let ((line (trim line)))
        (cond ((or (zerop (length line)) (char= (char line 0) #\#)))
              ((char= (char line 0) #\[)
               (setf section (trim (string-trim "[]" line))))
              ((equal section "ai")
               (let ((equals (position #\= line)))
                 (when equals
                   (let* ((key (trim (subseq line 0 equals)))
                          (rest (string-left-trim '(#\Space #\Tab) (subseq line (1+ equals))))
                          (value (and (plusp (length rest)) (parse-toml-string rest 0))))
                     (when value (push (cons key value) result)))))))))))

(defun user-config-dir ()
  (let ((xdg (getenv* "XDG_CONFIG_HOME"))
        (home (getenv* "HOME")))
    (cond (xdg (concatenate 'string xdg "/grit"))
          (home (concatenate 'string home "/.config/grit")))))

(defun ai-config (workdir)
  "The resolve command, verify command (or NIL) and prompt for WORKDIR."
  (let* ((project (concatenate 'string workdir "/.grit"))
         (user (user-config-dir))
         (files (remove nil (list project user)))
         (tables (mapcar (lambda (dir)
                           (let ((text (read-file (concatenate 'string dir "/config.toml"))))
                             (and text (parse-ai-config text))))
                         files)))
    (flet ((field (env key)
             (or (getenv* env)
                 (some (lambda (table) (cdr (assoc key table :test #'string=))) tables))))
      (values (or (field "GRIT_AI_RESOLVE_COMMAND" "command")
                  (git-fail "no AI resolve command configured. Add `[ai]\\ncommand = ...` to ~A/config.toml or to .grit/config.toml in your repo, or set GRIT_AI_RESOLVE_COMMAND."
                            (or user "~/.config/grit")))
              (field "GRIT_AI_VERIFY_COMMAND" "verify_command")
              (or (some (lambda (dir) (read-file (concatenate 'string dir "/resolve.md"))) files)
                  *default-prompt*)))))

(defun conflict-markers-p (octets)
  (let ((text (sb-ext:octets-to-string octets :external-format :latin-1)))
    (some (lambda (line)
            (or (uiop:string-prefix-p "<<<<<<<" line) (uiop:string-prefix-p ">>>>>>>" line)
                (string= (string-right-trim '(#\Return) line) "=======")))
          (split text #\Newline))))

(defun read-octets (path)
  (with-open-file (in (uiop:parse-native-namestring path) :element-type '(unsigned-byte 8)
                                                          :if-does-not-exist nil)
    (when in
      (let ((buffer (make-array (file-length in) :element-type '(unsigned-byte 8))))
        (subseq buffer 0 (read-sequence buffer in))))))

(defun write-octets (path &rest chunks)
  (with-open-file (out (uiop:parse-native-namestring path) :direction :output
                                                           :element-type '(unsigned-byte 8)
                                                           :if-exists :supersede)
    (dolist (chunk chunks) (write-sequence chunk out))))

(defun run-with-progress (app command input description)
  "Run shell COMMAND with the file INPUT on stdin while the footer shows
DESCRIPTION; Esc kills it. Returns its exit status, stdout and stderr octets,
or :CANCELLED."
  (let* ((repo (app-repo app))
         (out-file (git-dir-file repo "grit-ai.out"))
         (err-file (git-dir-file repo "grit-ai.err"))
         (process (sb-ext:run-program "/bin/sh" (list "-c" command)
                                      :wait nil :directory (repo-workdir repo)
                                      :input (uiop:parse-native-namestring input)
                                      :output (uiop:parse-native-namestring out-file)
                                      :if-output-exists :supersede
                                      :error (uiop:parse-native-namestring err-file)
                                      :if-error-exists :supersede
                                      :environment (sb-ext:posix-environ))))
    (setf (app-progress app) (list description (get-internal-real-time) t))
    (unwind-protect
         (loop while (sb-ext:process-alive-p process)
               do (render app)
                  (when (eq (read-key (app-terminal app) 200) :escape)
                    (sb-ext:process-kill process sb-posix:sigterm)
                    (sb-ext:process-wait process)
                    (return-from run-with-progress :cancelled)))
      (setf (app-progress app) nil)
      (sb-ext:process-close process))
    (multiple-value-prog1 (values (sb-ext:process-exit-code process) (read-octets out-file) (read-octets err-file))
      (ignore-errors (delete-file (uiop:parse-native-namestring out-file)))
      (ignore-errors (delete-file (uiop:parse-native-namestring err-file))))))

(defun run-verify (app command)
  "Run COMMAND where its output can be watched; true if it exited 0."
  (prog1 (zerop (run-on-terminal (app-terminal app) "/bin/sh" (list "-c" command)
                                 :directory (repo-workdir (app-repo app))
                                 :environment (editor-environment)
                                 :banner (format nil "~%--- grit autopilot · verify: ~A ---" command)
                                 :pause-on-failure t))
    (setf (app-last-screen app) nil)))

(defun resolve-file (app command prompt path index count)
  "Pipe conflicted PATH through COMMAND and stage the result; signals GIT-ERROR."
  (let* ((repo (app-repo app))
         (absolute (repo-path repo path))
         (input (git-dir-file repo "grit-ai.in")))
    (write-octets input (sb-ext:string-to-octets prompt :external-format :utf-8)
                  (sb-ext:string-to-octets (format nil "~%~%") :external-format :utf-8)
                  (or (read-octets absolute) (git-fail "read ~A: missing" path)))
    (multiple-value-bind (code out err)
        (unwind-protect
             (run-with-progress app command input
                                (format nil "resolving ~A (~D/~D)" path (1+ index) count))
          (ignore-errors (delete-file (uiop:parse-native-namestring input))))
      (cond ((eq code :cancelled) (git-fail "resolve ~A: cancelled by user" path))
            ((/= code 0)
             (git-fail "resolve ~A: exit ~D; ~A" path code
                       (flatten-message (sb-ext:octets-to-string err :external-format
                                                                 '(:utf-8 :replacement #\?))
                                        200)))
            ((conflict-markers-p out)
             (git-fail "resolve ~A: output still contains conflict markers" path)))
      (write-octets absolute out)
      (stage-file repo path))))

(defun autopilot (app)
  "Resolve conflicts with the configured AI command and continue the rebase,
over and over, until it finishes or a step fails."
  (handler-case
      (multiple-value-bind (command verify prompt) (ai-config (repo-workdir (app-repo app)))
        (let ((resolved 0)
              (repo (app-repo app)))
          (dotimes (iteration *autopilot-iterations*
                              (err app "autopilot bailed after ~D iterations (still conflicts?)"
                                   *autopilot-iterations*))
            (refresh app)
            (let* ((status (app-status app))
                   (paths (mapcar #'change-path (status-conflicted status))))
              (cond
                ((and (null paths) (null (status-rebase status)))
                 (return (info app "autopilot done~:[~*~*~; (~D file(s) across ~D iter(s))~]"
                               (plusp resolved) resolved iteration)))
                ((null paths)
                 (when (and verify (not (run-verify app verify)))
                   (git-fail "verify failed: ~A exited non-zero" verify))
                 (handler-case (busy (app "continuing rebase") (rebase-continue repo))
                   (git-error (condition) (git-fail "rebase continue: ~A" condition))))
                (t
                 (dolist (path paths)
                   (let ((size (length (or (read-octets (repo-path repo path)) #()))))
                     (when (> size *max-resolve-bytes*)
                       (git-fail "~A: ~D bytes exceeds the ~D-byte autopilot limit; resolve this file manually"
                                 path size *max-resolve-bytes*))))
                 (loop for path in paths
                       for index from 0
                       do (resolve-file app command prompt path index (length paths))
                          (incf resolved))))))))
    (error (condition)
      (ignore-errors (refresh app))
      (err app "~A" condition))))
