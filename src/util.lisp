(in-package #:tool-server)

(defun log-message (control &rest args)
  "Write a line to the server's stderr."
  (format sb-sys:*stderr* "~&tool-server: ~?~%" control args)
  (force-output sb-sys:*stderr*))

(defun in-directory (command directory)
  "COMMAND wrapped in a shell that first changes to native DIRECTORY (default:
the current one). SBCL's posix_spawn path ignores :DIRECTORY."
  (list* "/bin/sh" "-c" "cd -- \"$1\" && shift && exec \"$@\"" "sh"
         (or directory (uiop:native-namestring *default-pathname-defaults*))
         command))

(defun capture-output (command &key directory (environment (sb-ext:posix-environ)))
  "Run COMMAND, a list of strings, in native DIRECTORY (default: the current one)
with ENVIRONMENT (default: the server's). Return its stdout, whether it exited 0,
and its stderr."
  (handler-case
      (multiple-value-bind (out err code)
          ;; fork() in this image, then closing every fd up to the open-files
          ;; limit, costs 20-70 ms; posix_spawn ~2. SBCL's posix_spawn path
          ;; passes an empty environment unless given one.
          (uiop:run-program (in-directory command directory)
                            :output :string :error-output :string :ignore-error-status t
                            :use-posix-spawn t :environment environment)
        (values out (eql code 0) err))
    (error (condition)
      (values "" nil (princ-to-string condition)))))

(defun run-streaming (command &key directory (environment (sb-ext:posix-environ))
                                   (stream *error-output*))
  "Run COMMAND like CAPTURE-OUTPUT, but copy its stdout and stderr, merged, to
STREAM line by line as they arrive. Return its exit status."
  (let ((process (destructuring-bind (program &rest args) (in-directory command directory)
                   (sb-ext:run-program program args
                                       :input nil :output :stream :error :output :wait nil
                                       :use-posix-spawn t :environment environment
                                       :external-format '(:utf-8 :replacement #\?)))))
    (unwind-protect
         (let ((out (sb-ext:process-output process)))
           (loop for line = (read-line out nil)
                 while line
                 do (write-line line stream)
                    (finish-output stream))
           (sb-ext:process-wait process)
           (sb-ext:process-exit-code process))
      (sb-ext:process-close process))))

(defun pmap (function list &key (workers 8))
  "Map FUNCTION over LIST on up to WORKERS threads, keeping LIST's order.
Workers see the caller's *DEFAULT-PATHNAME-DEFAULTS* but not its output
streams. The first error stops the remaining work and is re-signalled here."
  (let* ((items (coerce list 'vector))
         (results (make-array (length items)))
         (next 0)
         (failure nil)
         (lock (sb-thread:make-mutex :name "pmap"))
         (cwd *default-pathname-defaults*))
    (flet ((work ()
             (let ((*default-pathname-defaults* cwd))
               (loop
                 (let ((i (sb-thread:with-mutex (lock)
                            (when (and (< next (length items)) (not failure))
                              (prog1 next (incf next))))))
                   (unless i (return))
                   (handler-case (setf (aref results i) (funcall function (aref items i)))
                     (error (condition)
                       (sb-thread:with-mutex (lock)
                         (setf failure (or failure condition))))))))))
      (mapc #'sb-thread:join-thread
            (loop repeat (min workers (length items))
                  collect (sb-thread:make-thread #'work :name "pmap worker")))
      (when failure
        (error failure))
      (coerce results 'list))))

(defun run-in-background (name function)
  "Call FUNCTION in a thread that outlives the current command. Its output goes
to the server's stdout; errors are logged rather than reaching any client."
  (sb-thread:make-thread
   (lambda ()
     (handler-case (funcall function)
       (serious-condition (condition)
         (log-message "~A: ~A" name condition))))
   :name name))
