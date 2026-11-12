(in-package #:tool-server)

;;; Slynk is loaded from the user's SLY checkout rather than declared as a
;;; dependency: its version must match the Emacs side, and nothing else needs it.

(defparameter *slynk-search-paths*
  '(".config/emacs/.local/straight/repos/sly/slynk/slynk.asd"
    ".emacs.d/.local/straight/repos/sly/slynk/slynk.asd"
    ".emacs.d/straight/repos/sly/slynk/slynk.asd"
    ".emacs.d/elpa/sly-*/slynk/slynk.asd")
  "Where to look for slynk.asd, relative to the home directory, unless SLYNK_DIR is set.")

(defvar *slynk-port* nil)
(defvar *slynk-lock* (sb-thread:make-mutex :name "tool-server slynk"))

(defun find-slynk-asd ()
  (let ((dir (uiop:getenv "SLYNK_DIR")))
    (if dir
        (probe-file (merge-pathnames "slynk.asd"
                                     (uiop:parse-native-namestring dir :ensure-directory t)))
        (loop for pattern in *slynk-search-paths*
                thereis (first (directory (merge-pathnames pattern (user-homedir-pathname))))))))

(defun %start-slynk (port)
  (unless (find-package '#:slynk)
    (let ((asd (or (find-slynk-asd)
                   (error "slynk not found; set SLYNK_DIR to the slynk/ directory of SLY"))))
      (asdf:load-asd asd)
      (asdf:load-system "slynk")))
  (uiop:symbol-call '#:slynk '#:create-server :port port :dont-close t))

(defun start-slynk (&key (port 4005))
  "Start a slynk server on localhost PORT for M-x sly-connect; return the port.
Does nothing if one is already running."
  (sb-thread:with-mutex (*slynk-lock*)
    (or *slynk-port*
        ;; A fresh thread sees the global streams; slynk keeps *error-output*
        ;; for logging, which must not be a command's client connection.
        (let ((result (sb-thread:join-thread
                       (sb-thread:make-thread
                        (lambda () (handler-case (%start-slynk port) (error (c) c)))
                        :name "slynk start"))))
          (when (typep result 'error)
            (error result))
          (log-message "slynk listening on localhost:~D" result)
          (setf *slynk-port* result)))))

(defun stop-slynk ()
  (sb-thread:with-mutex (*slynk-lock*)
    (when *slynk-port*
      (uiop:symbol-call '#:slynk '#:stop-server *slynk-port*)
      (log-message "slynk stopped")
      (setf *slynk-port* nil)
      t)))
