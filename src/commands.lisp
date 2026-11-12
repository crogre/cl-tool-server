(in-package #:tool-server)

(defstruct (command (:constructor make-command (name symbol lambda-list shim)))
  name symbol lambda-list shim)

(defvar *commands* (make-hash-table :test 'equal :synchronized t)
  "Command name -> COMMAND. Dispatch goes through the symbol, so redefining the
function in the live image takes effect immediately.")

(defvar *backtrace-on-error* nil
  "When true, unexpected errors print a backtrace to the client's stderr.")

(defmacro defcommand (name-and-options lambda-list &body body)
  "Define function NAME and register it as a command. NAME-AND-OPTIONS is NAME
or (NAME &key name shim): :NAME overrides the command name (default: NAME
downcased), :SHIM NIL keeps install-shims from linking it. Arguments arrive as
strings, so LAMBDA-LIST may only use required, &optional and &rest parameters."
  (destructuring-bind (symbol &key name (shim t))
      (if (listp name-and-options) name-and-options (list name-and-options))
    `(progn
       (defun ,symbol ,lambda-list ,@body)
       (register-command ',symbol ',lambda-list :name ,name :shim ,shim)
       ',symbol)))

(defun register-command (symbol lambda-list &key name (shim t))
  (let ((name (or name (string-downcase (symbol-name symbol)))))
    (setf (gethash name *commands*) (make-command name symbol lambda-list shim))))

(defun find-command (name)
  (gethash name *commands*))

(defun all-commands ()
  (sort (sb-ext:with-locked-hash-table (*commands*)
          (loop for command being the hash-values of *commands* collect command))
        #'string< :key #'command-name))

(defun command-usage (command)
  (let ((mode '&required))
    (format nil "~A~{ ~A~}" (command-name command)
            (loop for param in (command-lambda-list command)
                  if (member param lambda-list-keywords)
                    do (setf mode param)
                  else
                    collect (let ((name (symbol-name (if (consp param) (first param) param))))
                              (case mode
                                (&optional (format nil "[~A]" name))
                                (&rest (format nil "[~A...]" name))
                                (t name)))))))

(defun arity (lambda-list)
  "Return the minimum and maximum (NIL: unbounded) number of arguments."
  (let ((min 0) (max 0) (mode '&required))
    (dolist (param lambda-list (values min max))
      (case param
        (&optional (setf mode param))
        (&rest (setf mode param max nil))
        (&aux (return (values min max)))
        (t (case mode
             (&required (incf min) (incf max))
             (&optional (incf max))))))))

(define-condition command-failed (error)
  ((message :initarg :message :reader command-failed-message)
   (code :initarg :code :initform 1 :reader command-failed-code))
  (:report (lambda (condition stream)
             (write-string (command-failed-message condition) stream))))

(defun fail (control &rest args)
  "Abort the current command: print the message to stderr and exit with status 1."
  (error 'command-failed :message (apply #'format nil control args)))

(defun exit-with (code)
  "End the current command with exit status CODE, printing nothing."
  (throw 'exit-command code))

(defun arg-pathname (string)
  "Parse a path argument relative to the client's cwd, without wildcard syntax."
  (merge-pathnames (uiop:parse-native-namestring string)))

(defun arg-directory (string)
  "Like ARG-PATHNAME, but always a directory pathname."
  (merge-pathnames (uiop:parse-native-namestring string :ensure-directory t)))

(defun run-command (argv)
  "Run the command named by the first element of ARGV on the remaining ones and
return its exit status."
  (destructuring-bind (name &rest args) argv
    (let ((command (find-command name)))
      (unless command
        (format *error-output* "toolc: unknown command: ~A (try: toolc help)~%" name)
        (return-from run-command 127))
      (multiple-value-bind (min max) (arity (command-lambda-list command))
        (unless (and (<= min (length args)) (or (null max) (<= (length args) max)))
          (format *error-output* "usage: ~A~%" (command-usage command))
          (return-from run-command 2)))
      (catch 'exit-command
        (handler-case
            (handler-bind ((serious-condition
                             (lambda (condition)
                               (when (and *backtrace-on-error*
                                          (not (typep condition 'command-failed)))
                                 (sb-debug:print-backtrace :count 40 :stream *error-output*)))))
              (apply (command-symbol command) args)
              0)
          (command-failed (condition)
            (format *error-output* "~A: ~A~%" name condition)
            (command-failed-code condition))
          (serious-condition (condition)
            (format *error-output* "~A: ~A~%" name condition)
            1))))))
