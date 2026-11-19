(in-package #:tool-server-user)

;;; trigger-ci: drop the CI-skip markers from a branch's commit messages and
;;; force-push, so that CI picks the branch up.
;;;
;;; Git runs in the caller's cwd with the caller's environment, so hooks, SKIP,
;;; GIT_* variables, PATH and the ssh agent are the caller's, not the server's.
;;; A rebase needs non-interactive editors; both are this image, reached
;;; through toolc as the hidden trigger-ci-edit command, so the message is
;;; stripped by the same code as everywhere else and no perl or sed -i
;;; (whose syntax differs between BSD and GNU) is needed.

(defparameter *ci-skip-markers*
  '("[skip ci]" "[ci skip]" "[no ci]" "[skip actions]")
  "Markers that keep CI from running on a push, matched case-insensitively.")

(defparameter *no-ci-marker* "***NO_CI***")

(defparameter *host-broken-hooks*
  "scripts-should-be-executable,no-mixed-line-ending,no-trailing-whitespace"
  "Appended to SKIP. Repos provision these pre-commit console scripts
themselves, so a Homebrew pre-commit cannot run them; --no-verify is not an
option, so every other hook still runs.")

(defparameter *trigger-ci-usage* "usage: trigger-ci [--base <ref>] [--no-push] [-n|--dry-run] [-h]

Rewrites the commits on this branch that carry a CI-skip marker ([skip ci],
[ci skip], [no ci], [skip actions], ***NO_CI***) to drop it, then force-pushes
with --force-with-lease so the pipeline picks the branch up.

The first commit of a DRAFT PR is created with [skip ci] by convention; this
is the counterpart that turns CI back on once the branch is worth building.

  --base <ref>   range to scan (default: origin/main, else origin/master)
  --no-push      rewrite locally, do not push
  -n, --dry-run  report what would change, touch nothing

Only commit messages change. Refuses to run with staged changes, since those
would be folded into the amended commit.
")

;;; Messages

(defun perl-space-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return #\Page #.(code-char 11))))

(defun remove-markers (line markers)
  "LINE without each case-insensitive occurrence of MARKERS and the whitespace
just before it, as perl's s/\\s*(?:MARKERS)//gi does."
  (with-output-to-string (out)
    (let ((pos 0))
      (loop
        (let ((start nil) (marker nil))
          (dolist (m markers)
            (let ((i (search m line :start2 pos :test #'char-equal)))
              (when (and i (or (null start) (< i start)))
                (setf start i marker m))))
          (unless start
            (write-string line out :start pos)
            (return))
          (let ((cut (position-if-not #'perl-space-p line :start pos :end start :from-end t)))
            (write-string line out :start pos :end (if cut (1+ cut) pos)))
          (setf pos (+ start (length marker))))))))

(defun strip-markers (message)
  "MESSAGE without CI-skip markers. Trailing blanks are trimmed from every
line too, since a dropped trailing \" [skip ci]\" would otherwise leave some,
which pre-commit flags."
  (format nil "~{~A~^~%~}"
          (mapcar (lambda (line)
                    (string-right-trim
                     '(#\Space #\Tab)
                     (remove-markers (remove-markers line *ci-skip-markers*)
                                     (list *no-ci-marker*))))
                  (uiop:split-string message :separator '(#\Newline)))))

(defun marked-p (message)
  (some (lambda (m) (search m message :test #'char-equal))
        (cons *no-ci-marker* *ci-skip-markers*)))

;;; Git

(defun override-environment (env &rest overrides)
  "ENV with OVERRIDES, alternating names and values; a NIL value is skipped."
  (loop for (name value) on overrides by #'cddr
        for prefix = (concatenate 'string name "=")
        when value
          do (setf env (cons (concatenate 'string prefix value)
                             (remove-if (lambda (e) (uiop:string-prefix-p prefix e)) env))))
  env)

(defun git-out (env &rest args)
  "Stdout of git ARGS without the final newline, or NIL if git failed."
  (multiple-value-bind (out ok) (capture-output (list* "git" args) :environment env)
    (and ok (string-right-trim '(#\Newline) out))))

(defun git-run (env &rest args)
  "Run git ARGS, relaying its output to the caller's stderr; return its status."
  (run-streaming (list* "git" args) :environment env))

(defun shell-quote (string)
  (format nil "'~A'" (uiop:frob-substrings string '("'") "'\\''")))

(defun marked-commits (env range)
  "(SHA SUBJECT MESSAGE) of each commit in RANGE whose message has a marker,
newest first."
  (let ((out (or (git-out env "log" "-z" "--format=%H%x01%s%x01%B" range)
                 (fail "git log ~A failed" range))))
    (loop for record in (uiop:split-string out :separator '(#\Nul))
          for i = (position (code-char 1) record)
          for j = (and i (position (code-char 1) record :start (1+ i)))
          for message = (and j (subseq record (1+ j)))
          when (and message (marked-p message))
            collect (list (subseq record 0 i) (subseq record (1+ i) j) message))))

(defun reword-all (env merge-base)
  "Rebase onto MERGE-BASE rewording every commit. The strip leaves a message
without a marker as it was, and a rebase renews every SHA after the first
rewritten commit anyway, so this costs nothing over rewording only the marked
ones."
  (let ((toolc (uiop:native-namestring
                (asdf:system-relative-pathname "tool-server" "bin/toolc"))))
    (unless (probe-file toolc)
      (fail "~A not found; build it with make" toolc))
    (flet ((editor (mode) (format nil "~A trigger-ci-edit ~A" (shell-quote toolc) mode)))
      (let ((env (override-environment env
                                       "GIT_SEQUENCE_EDITOR" (editor "todo")
                                       "GIT_EDITOR" (editor "message"))))
        (unless (zerop (git-run env "rebase" "--interactive" merge-base))
          (fail "rebase stopped; finish it with git rebase --continue or undo it with ~
                 git rebase --abort"))))))

(defun read-latin-1 (path)
  (with-open-file (in path :external-format :latin-1)
    (let* ((buffer (make-string (file-length in)))
           (end (read-sequence buffer in)))
      (subseq buffer 0 end))))

(defun write-latin-1 (path string)
  (with-open-file (out path :direction :output :if-exists :supersede :external-format :latin-1)
    (write-string string out)))

(defun reword-todo (todo)
  "TODO with every pick turned into a reword."
  (format nil "~{~A~^~%~}"
          (mapcar (lambda (line)
                    (let ((word (subseq line 0 (position #\Space line))))
                      (if (member word '("pick" "p") :test #'string=)
                          (concatenate 'string "reword" (subseq line (length word)))
                          line)))
                  (uiop:split-string todo :separator '(#\Newline)))))

(defcommand (trigger-ci-edit :shim nil) (mode file)
  "Rebase editor for trigger-ci: rewrite FILE as MODE, todo or message, says.
MODE todo rewords every pick; MODE message strips the CI-skip markers.
Latin-1 passes any bytes through."
  (let* ((path (arg-pathname file))
         (text (read-latin-1 path)))
    (write-latin-1 path (cond ((string= mode "todo") (reword-todo text))
                              ((string= mode "message") (strip-markers text))
                              (t (fail "unknown mode: ~A" mode))))))

;;; Command

(defun trigger-ci-usage ()
  (write-string *trigger-ci-usage* *error-output*)
  (exit-with 2))

(defcommand trigger-ci (&rest args)
  "Drop the [skip ci] marker from a branch's commits and force-push so CI runs.
Run trigger-ci -h for the options."
  (let ((base nil) (push t) (dry nil))
    (loop while args
          do (let ((arg (pop args)))
               (cond ((string= arg "--base")
                      (unless args (trigger-ci-usage))
                      (setf base (pop args)))
                     ((string= arg "--no-push") (setf push nil))
                     ((member arg '("-n" "--dry-run") :test #'string=) (setf dry t))
                     (t (trigger-ci-usage)))))
    ;; NIL only when called from a REPL rather than through toolc.
    (let* ((env (or *client-environment* (sb-ext:posix-environ)))
           (branch (progn
                     (unless (equal (git-out env "rev-parse" "--is-inside-work-tree") "true")
                       (fail "not inside a git repository"))
                     (git-out env "rev-parse" "--abbrev-ref" "HEAD"))))
      (when (equal branch "HEAD")
        (fail "detached HEAD; check out a branch first"))
      (unless base
        (setf base (or (find-if (lambda (ref) (git-out env "rev-parse" "--verify" "--quiet" ref))
                                '("origin/main" "origin/master"))
                       (fail "no origin/main or origin/master; pass --base"))))
      (let* ((merge-base (or (git-out env "merge-base" base "HEAD")
                             (fail "no common ancestor with ~A" base)))
             (marked (marked-commits env (format nil "~A..HEAD" merge-base))))
        (unless marked
          (format *error-output* "no CI-skip marker on ~A (vs ~A); nothing to do~%" branch base)
          (return-from trigger-ci))
        (format *error-output* "branch ~A, ~D commit(s) with a CI-skip marker:~%"
                branch (length marked))
        (loop for (sha subject) in marked
              do (format *error-output* "  ~A  ~A~%             ~A~%"
                         (subseq sha 0 9) subject (strip-markers subject)))
        (when dry
          (format *error-output* "dry run; nothing changed~%")
          (return-from trigger-ci))
        (unless (git-out env "diff" "--cached" "--quiet")
          (fail "staged changes present; commit or reset them first"))
        (finish-output *error-output*)
        (let* ((skip (client-getenv "SKIP"))
               (env (override-environment
                     env
                     "SKIP" (if (plusp (length skip))
                                (format nil "~A,~A" skip *host-broken-hooks*)
                                *host-broken-hooks*)
                     ;; The rebase editors call back into this server.
                     "TOOL_SERVER_SOCKET" (server-socket-path))))
          (if (and (= (length marked) 1)
                   (equal (first (first marked)) (git-out env "rev-parse" "HEAD")))
              ;; The common case, a single-commit draft PR: amend, no rebase.
              (uiop:with-temporary-file (:pathname message-file)
                (with-open-file (out message-file :direction :output :if-exists :supersede
                                                  :external-format :utf-8)
                  (write-string (strip-markers (third (first marked))) out))
                (unless (zerop (git-run env "commit" "--amend" "--quiet" "--file"
                                        (uiop:native-namestring message-file)))
                  (fail "git commit --amend failed")))
              (reword-all env merge-base))
          (format *error-output* "rewrote: ~A~%" (git-out env "log" "-1" "--format=%h %s"))
          (unless push
            (format *error-output* "--no-push given; force-push with: git push --force-with-lease~%")
            (return-from trigger-ci))
          (finish-output *error-output*)
          (unless (zerop (git-run env "push" "--force-with-lease"))
            (fail "git push --force-with-lease failed"))
          (format *error-output* "pushed ~A; CI should start (check with: gh pr checks)~%"
                  branch))))))
