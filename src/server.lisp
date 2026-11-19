(in-package #:tool-server)

;;; Protocol, one request per connection.
;;;
;;; Client -> server: NUL-terminated UTF-8 fields
;;;   "3" (protocol version), cwd, tty, argc, argv[0] .. argv[argc-1],
;;;   envc, env[0] .. env[envc-1]
;;; where tty is the client's controlling terminal ("" if none), argv[0] is
;;; the command name and each env entry is NAME=value. The client sends
;;; nothing more; the socket becoming readable means it went away.
;;;
;;; Server -> client: frames of a tag byte, a 4-byte big-endian length and the payload
;;;   #\O  stdout bytes
;;;   #\E  stderr bytes
;;;   #\X  exit status as decimal ASCII; always the last frame

(defparameter *protocol-version* "3")
(defparameter *flush-threshold* 16384)

(defun default-socket-path ()
  (or (sb-posix:getenv "TOOL_SERVER_SOCKET")
      (format nil "~A/tool-server.sock"
              (string-right-trim "/" (or (sb-posix:getenv "TMPDIR") "/tmp")))))

;;; Output frames

(defstruct (connection (:constructor make-connection (stream)))
  stream
  (lock (sb-thread:make-mutex :name "tool-server connection")))

(defun send-frame (connection tag octets)
  (let ((out (connection-stream connection))
        (length (length octets)))
    (sb-thread:with-mutex ((connection-lock connection))
      (write-byte (char-code tag) out)
      (loop for shift from 24 downto 0 by 8
            do (write-byte (ldb (byte 8 shift) length) out))
      (write-sequence octets out)
      (force-output out))))

(defun send-exit (connection status)
  (send-frame connection #\X (sb-ext:string-to-octets (princ-to-string status))))

(defclass frame-stream (sb-gray:fundamental-character-output-stream)
  ((connection :initarg :connection :reader connection)
   (tag :initarg :tag :reader tag)
   (buffer :initform (make-array 1024 :element-type 'character :adjustable t :fill-pointer 0)
           :reader buffer)
   (at-line-start :initform t :accessor at-line-start))
  (:documentation "Character stream sent to the client as frames with TAG.
Fully buffered: output reaches the client on FINISH-OUTPUT or when the buffer fills."))

(defun flush-frame-stream (stream)
  (let ((buffer (buffer stream)))
    (when (plusp (fill-pointer buffer))
      (send-frame (connection stream) (tag stream)
                  (sb-ext:string-to-octets buffer :external-format :utf-8))
      (setf (fill-pointer buffer) 0))))

(defmethod sb-gray:stream-write-char ((stream frame-stream) char)
  (vector-push-extend char (buffer stream))
  (setf (at-line-start stream) (char= char #\Newline))
  (when (>= (fill-pointer (buffer stream)) *flush-threshold*)
    (flush-frame-stream stream))
  char)

(defmethod sb-gray:stream-write-string ((stream frame-stream) string &optional (start 0) end)
  (loop for i from start below (or end (length string))
        do (sb-gray:stream-write-char stream (char string i)))
  string)

(defmethod sb-gray:stream-line-column ((stream frame-stream))
  (if (at-line-start stream) 0 nil))

(defmethod sb-gray:stream-start-line-p ((stream frame-stream))
  (at-line-start stream))

(defmethod sb-gray:stream-force-output ((stream frame-stream))
  (flush-frame-stream stream))

(defmethod sb-gray:stream-finish-output ((stream frame-stream))
  (flush-frame-stream stream))

;;; Requests

(defun read-field (in)
  "Read one NUL-terminated UTF-8 field, or return NIL at end of input."
  (let ((octets (make-array 128 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte in nil nil)
          do (case byte
               ((nil) (return nil))
               (0 (return (sb-ext:octets-to-string
                           (coerce octets '(simple-array (unsigned-byte 8) (*)))
                           :external-format '(:utf-8 :replacement #\?))))
               (t (vector-push-extend byte octets))))))

(defun read-request (in)
  "Return the client's cwd, argv, tty and environment, or NIL if the client
sent nothing."
  (flet ((field () (or (read-field in) (error "truncated request"))))
    (let ((version (read-field in)))
      (when version
        (unless (equal version *protocol-version*)
          (error "client speaks protocol ~S, server ~S: rebuild toolc (make) ~
                  or restart the server" version *protocol-version*))
        (let* ((cwd (field))
               (tty (field))
               (argc (parse-integer (field)))
               (argv (loop repeat argc collect (field)))
               (env (loop repeat (parse-integer (field)) collect (field))))
          (when (null argv)
            (error "empty argv"))
          (values cwd argv (and (plusp (length tty)) tty) env))))))

(defun serve-request (connection cwd argv tty env socket-fd)
  (let* ((out (make-instance 'frame-stream :connection connection :tag #\O))
         (err (make-instance 'frame-stream :connection connection :tag #\E))
         (status (let ((*standard-output* out)
                       (*error-output* err)
                       (*standard-input* (make-concatenated-stream))
                       (*package* (find-package '#:tool-server-user))
                       (*client-tty* tty)
                       (*client-environment* env)
                       (*client-socket-fd* socket-fd)
                       (*default-pathname-defaults*
                         (uiop:parse-native-namestring cwd :ensure-directory t)))
                   (run-command argv))))
    (finish-output out)
    (finish-output err)
    status))

(defun handle-client (socket)
  (let* ((stream (sb-bsd-sockets:socket-make-stream
                  socket :input t :output t :element-type '(unsigned-byte 8) :buffering :full))
         (connection (make-connection stream)))
    (unwind-protect
         (handler-case
             (multiple-value-bind (cwd argv tty env) (read-request stream)
               (when cwd
                 (send-exit connection
                            (serve-request connection cwd argv tty env
                                           (sb-bsd-sockets:socket-file-descriptor socket)))))
           (serious-condition (condition)
             (log-message "~A" condition)
             (ignore-errors
              (send-frame connection #\E (sb-ext:string-to-octets
                                          (format nil "tool-server: ~A~%" condition)))
              (send-exit connection 255))))
      (ignore-errors (close stream))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))

;;; Listener

(defstruct server
  path
  socket
  thread
  (running t)
  (clients '())
  (lock (sb-thread:make-mutex :name "tool-server")))

(defvar *server* nil)

(defun socket-alive-p (path)
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (unwind-protect
         (handler-case (progn (sb-bsd-sockets:socket-connect socket path) t)
           (sb-bsd-sockets:socket-error () nil))
      (sb-bsd-sockets:socket-close socket))))

(defun remove-stale-socket (path)
  (let ((stat (handler-case (sb-posix:lstat path)
                (sb-posix:syscall-error () nil))))
    (when stat
      (unless (sb-posix:s-issock (sb-posix:stat-mode stat))
        (error "~A exists and is not a socket" path))
      (when (socket-alive-p path)
        (error "another tool-server is already listening on ~A" path))
      (sb-posix:unlink path))))

(defun spawn-client (server socket)
  ;; Holding the lock across MAKE-THREAD makes the thread's own removal wait
  ;; until it has been added.
  (sb-thread:with-mutex ((server-lock server))
    (push (sb-thread:make-thread
           (lambda ()
             (unwind-protect (handle-client socket)
               (sb-thread:with-mutex ((server-lock server))
                 (setf (server-clients server)
                       (remove sb-thread:*current-thread* (server-clients server))))))
           :name "tool-server client")
          (server-clients server))))

(defun accept-loop (server)
  (unwind-protect
       (loop
         (let ((client (handler-case (sb-bsd-sockets:socket-accept (server-socket server))
                         (sb-bsd-sockets:socket-error (condition)
                           (log-message "accept: ~A" condition)
                           (sleep 0.1)
                           nil))))
           (cond ((not (server-running server))
                  (when client (sb-bsd-sockets:socket-close client))
                  (return))
                 (client
                  (spawn-client server client)))))
    (sb-bsd-sockets:socket-close (server-socket server))
    (ignore-errors (sb-posix:unlink (server-path server)))))

(defun start (&key (path (default-socket-path)))
  "Listen on the unix socket at PATH, serving each connection in its own thread."
  (when *server*
    (error "tool-server is already running on ~A" (server-path *server*)))
  ;; sun_path is 104 bytes on macOS (108 on Linux); SBCL silently truncates.
  (when (> (length (sb-ext:string-to-octets path :external-format :utf-8)) 103)
    (error "socket path too long (max 103 bytes): ~A" path))
  (remove-stale-socket path)
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream))
        (old-umask (sb-posix:umask #o077)))
    (unwind-protect (sb-bsd-sockets:socket-bind socket path)
      (sb-posix:umask old-umask))
    (sb-bsd-sockets:socket-listen socket 64)
    (let ((server (make-server :path path :socket socket)))
      (setf (server-thread server)
            (sb-thread:make-thread #'accept-loop :name "tool-server accept"
                                                 :arguments (list server))
            *server* server)
      (log-message "listening on ~A" path)
      server)))

(defun server-socket-path ()
  "Path of the socket this image is listening on, or NIL."
  (let ((server *server*))
    (and server (server-path server))))

(defun stop ()
  "Stop accepting connections. Commands already running are left to finish."
  (let ((server *server*))
    (when server
      (setf *server* nil
            (server-running server) nil)
      ;; Wake the accept loop so it sees RUNNING is false.
      (socket-alive-p (server-path server))
      (unless (eq sb-thread:*current-thread* (server-thread server))
        (sb-thread:join-thread (server-thread server) :default nil))
      (log-message "stopped")
      t)))

(defun wait-for-clients (server)
  (dolist (thread (sb-thread:with-mutex ((server-lock server))
                    (copy-list (server-clients server))))
    (unless (eq thread sb-thread:*current-thread*)
      (sb-thread:join-thread thread :default nil))))

(defun serve (&key (path (default-socket-path)))
  "Entry point for bin/tool-server: start the server and block until it is
stopped (by the shutdown command or ^C). Exits the process if it cannot start."
  (let ((server (handler-case (start :path path)
                  (error (condition)
                    (log-message "~A" condition)
                    (sb-ext:exit :code 1 :abort t)))))
    (handler-case (sb-thread:join-thread (server-thread server) :default nil)
      (sb-sys:interactive-interrupt ()
        (log-message "interrupted")))
    (stop)
    (wait-for-clients server)))
