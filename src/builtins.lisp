(in-package #:tool-server)

(defun first-line (string)
  (subseq string 0 (position #\Newline string)))

(defcommand (help :shim nil) (&optional command)
  "List the commands, or describe COMMAND."
  (if command
      (let ((command (or (find-command command) (fail "unknown command: ~A" command))))
        (format t "usage: ~A~%~@[~%~A~%~]" (command-usage command)
                (documentation (command-symbol command) 'function)))
      (let* ((commands (all-commands))
             (width (reduce #'max commands :key (lambda (c) (length (command-usage c))))))
        (dolist (command commands)
          (format t "~vA  ~A~%" width (command-usage command)
                  (first-line (or (documentation (command-symbol command) 'function) "")))))))

(defcommand (reload :shim nil) ()
  "Recompile changed source files and load them into the running image."
  (asdf:load-system "tool-server"))

(defcommand (lisp-eval :name "eval" :shim nil) (&rest forms)
  "Evaluate each FORM in the image and print its values."
  (dolist (form forms)
    (format t "~{~S~^~%~}~%" (multiple-value-list (eval (read-from-string form))))))

(defcommand (cache-stats :shim nil) ()
  "Show size, hits and misses of every cache."
  (format t "~20A ~10@A ~10@A ~10@A~%" "cache" "entries" "hits" "misses")
  (dolist (cache (all-caches))
    (format t "~20A ~10D ~10D ~10D~%" (cache-name cache)
            (hash-table-count (cache-table cache)) (cache-hits cache) (cache-misses cache))))

(defcommand (cache-clear :shim nil) (&rest names)
  "Empty the caches called NAMES, or all caches."
  (dolist (cache (all-caches))
    (when (or (null names) (member (cache-name cache) names :test #'string=))
      (clear-cache cache)
      (format t "cleared ~A~%" (cache-name cache)))))

(defun toolc-symlink-p (path)
  (let ((stat (handler-case (sb-posix:lstat path) (sb-posix:syscall-error () nil))))
    (and stat
         (sb-posix:s-islnk (sb-posix:stat-mode stat))
         (equal (pathname-name (sb-posix:readlink path)) "toolc"))))

(defcommand (install-shims :shim nil) (directory)
  "Symlink every command into DIRECTORY (put it on PATH) as the toolc client.
Existing toolc symlinks are updated; other files are left alone."
  (let ((client (uiop:native-namestring
                 (asdf:system-relative-pathname "tool-server" "bin/toolc")))
        (directory (arg-directory directory)))
    (unless (probe-file client)
      (fail "~A not found; build it with make" client))
    (ensure-directories-exist directory)
    (dolist (command (all-commands))
      (when (command-shim command)
        (let ((link (concatenate 'string (uiop:native-namestring directory)
                                 (command-name command))))
          (cond ((toolc-symlink-p link)
                 (unless (equal (sb-posix:readlink link) client)
                   (sb-posix:unlink link)
                   (sb-posix:symlink client link)
                   (format t "updated ~A~%" link)))
                ((probe-file link)
                 (format *error-output* "skipped ~A: exists and is not a toolc symlink~%" link))
                (t
                 (sb-posix:symlink client link)
                 (format t "created ~A~%" link))))))))

(defcommand (slynk :shim nil) (&optional (port "4005"))
  "Start a slynk server on localhost PORT for M-x sly-connect.
If one is already running, report its port instead."
  (format t "slynk listening on localhost:~D~%"
          (start-slynk :port (or (parse-integer port :junk-allowed t)
                                 (fail "not a port number: ~A" port)))))

(defcommand (shutdown :shim nil) ()
  "Stop the server once running commands have finished."
  (stop))
