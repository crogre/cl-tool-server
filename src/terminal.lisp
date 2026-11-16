(in-package #:tool-server)

;;; Interactive commands draw on the client's terminal directly: the client
;;; sends the path of its controlling terminal, and since server and client run
;;; as the same user the server can open it. Stdout stays a normal frame
;;; stream, so `dir=$(tool -i)' captures a result while the UI shows on the tty.

(defvar *client-tty* nil "Path of the client's controlling terminal, or NIL.")
(defvar *client-socket-fd* nil "Socket fd of the current client, to notice it leaving.")

(define-condition terminal-closed (error) ()
  (:report "the terminal or the client went away"))

(defstruct (terminal (:constructor %make-terminal (fd output saved)))
  fd output saved (pending '()))

(defun display-width (string)
  "Terminal columns STRING takes; emoji and other wide characters take two."
  (loop for char across string sum (if (>= (char-code char) #x2600) 2 1)))

(defun fit-to-width (string width)
  "STRING cut or padded with spaces to exactly WIDTH columns."
  (let ((used 0))
    (with-output-to-string (out)
      (loop for char across string
            for w = (if (>= (char-code char) #x2600) 2 1)
            while (<= (+ used w) width)
            do (write-char char out) (incf used w))
      (loop repeat (- width used) do (write-char #\Space out)))))

(defun raw-mode (fd)
  "Put the terminal at FD in raw mode; return its previous settings."
  (let ((saved (sb-posix:tcgetattr fd))
        (raw (sb-posix:tcgetattr fd)))
    (setf (sb-posix:termios-lflag raw)
          (logandc2 (sb-posix:termios-lflag raw)
                    (logior sb-posix:icanon sb-posix:echo sb-posix:isig sb-posix:iexten))
          (sb-posix:termios-iflag raw)
          (logandc2 (sb-posix:termios-iflag raw)
                    (logior sb-posix:ixon sb-posix:icrnl sb-posix:brkint
                            sb-posix:inpck sb-posix:istrip)))
    (let ((cc (sb-posix:termios-cc raw)))
      (setf (aref cc sb-posix:vmin) 1
            (aref cc sb-posix:vtime) 0
            (sb-posix:termios-cc raw) cc))
    (sb-posix:tcsetattr fd sb-posix:tcsanow raw)
    saved))

(defun call-with-terminal (function)
  (unless *client-tty*
    (fail "this needs a terminal"))
  (let ((fd (sb-posix:open *client-tty* (logior sb-posix:o-rdwr sb-posix:o-noctty))))
    (unless (sb-posix:s-ischr (sb-posix:stat-mode (sb-posix:fstat fd)))
      (sb-posix:close fd)
      (fail "~A is not a terminal" *client-tty*))
    (let* ((saved (raw-mode fd))
           (output (sb-sys:make-fd-stream fd :output t :external-format :utf-8
                                             :buffering :full))
           (terminal (%make-terminal fd output saved)))
      (unwind-protect
           (progn
             ;; Alternate screen, hidden cursor.
             (write-string (format nil "~C[?1049h~C[?25l" #\Esc #\Esc) output)
             (finish-output output)
             (funcall function terminal))
        (ignore-errors
         (write-string (format nil "~C[?25h~C[?1049l" #\Esc #\Esc) output)
         (finish-output output))
        (ignore-errors (sb-posix:tcsetattr fd sb-posix:tcsanow saved))
        ;; Closes FD too.
        (close output)))))

(defmacro with-terminal ((var) &body body)
  "Run BODY with VAR bound to the client's terminal in raw mode on the
alternate screen, restoring both afterwards. Fails without a terminal."
  `(call-with-terminal (lambda (,var) ,@body)))

(defun terminal-size (terminal)
  "Rows and columns of TERMINAL."
  (sb-alien:with-alien ((size (array (sb-alien:unsigned 16) 4)))
    (if (zerop (sb-alien:alien-funcall
                ;; ioctl is variadic: &optional makes SBCL pass what follows
                ;; the way varargs go, which on arm64 macOS is on the stack.
                (sb-alien:extern-alien "ioctl" (function sb-alien:int sb-alien:int
                                                         sb-alien:unsigned-long
                                                         &optional
                                                         (* (array (sb-alien:unsigned 16) 4))))
                (terminal-fd terminal)
                #+darwin #x40087468 #+linux #x5413 ; TIOCGWINSZ
                (sb-alien:addr size)))
        (values (max 1 (sb-alien:deref size 0)) (max 1 (sb-alien:deref size 1)))
        (values 24 80))))

(defun terminal-write (terminal string)
  (write-string string (terminal-output terminal)))

(defun terminal-flush (terminal)
  (finish-output (terminal-output terminal)))

(defconstant +pollin+ 1)
(defconstant +poll-closed+ (logior #x8 #x10 #x20)) ; POLLERR POLLHUP POLLNVAL

(sb-alien:define-alien-type nil
    (sb-alien:struct pollfd
                     (fd sb-alien:int)
                     (events sb-alien:short)
                     (revents sb-alien:short)))

(defun wait-for-input (terminal timeout-ms)
  "True when TERMINAL has input within TIMEOUT-MS. Signals TERMINAL-CLOSED
when the terminal hangs up or the client disconnects."
  (sb-alien:with-alien ((fds (array (sb-alien:struct pollfd) 2)))
    (let ((count (if *client-socket-fd* 2 1)))
      (setf (sb-alien:slot (sb-alien:deref fds 0) 'fd) (terminal-fd terminal)
            (sb-alien:slot (sb-alien:deref fds 0) 'events) +pollin+
            (sb-alien:slot (sb-alien:deref fds 0) 'revents) 0)
      (when *client-socket-fd*
        ;; The client sends nothing after its request, so input means it left.
        (setf (sb-alien:slot (sb-alien:deref fds 1) 'fd) *client-socket-fd*
              (sb-alien:slot (sb-alien:deref fds 1) 'events) +pollin+
              (sb-alien:slot (sb-alien:deref fds 1) 'revents) 0))
      (let ((ready (sb-alien:alien-funcall
                    (sb-alien:extern-alien "poll" (function sb-alien:int sb-alien:system-area-pointer
                                                            #+darwin sb-alien:unsigned
                                                            #-darwin sb-alien:unsigned-long
                                                            sb-alien:int))
                    (sb-alien:alien-sap fds) count timeout-ms)))
        (cond ((<= ready 0) nil)
              ((and *client-socket-fd*
                    (plusp (sb-alien:slot (sb-alien:deref fds 1) 'revents)))
               (error 'terminal-closed))
              ((plusp (logand (sb-alien:slot (sb-alien:deref fds 0) 'revents) +poll-closed+))
               (error 'terminal-closed))
              (t t))))))

(defun decode-keys (octets)
  "Keys in OCTETS: characters, or keywords for special keys."
  (let ((keys '())
        (i 0)
        (n (length octets)))
    (flet ((next () (and (< i n) (aref octets i))))
      (loop while (< i n)
            do (let ((byte (aref octets i)))
                 (incf i)
                 (cond
                   ((and (= byte 27) (member (next) '(91 79))) ; ESC [ or ESC O
                    (incf i)
                    (let ((start i))
                      (loop while (and (< i n) (<= 48 (aref octets i) 63)) do (incf i))
                      (let ((params (map 'string #'code-char (subseq octets start i)))
                            (final (and (< i n) (code-char (aref octets i)))))
                        (when final (incf i))
                        (push (case final
                                (#\A :up) (#\B :down) (#\C :right) (#\D :left)
                                (#\H :home) (#\F :end)
                                (#\~ (cond ((string= params "1") :home)
                                           ((string= params "4") :end)
                                           ((string= params "3") :delete)
                                           ((string= params "5") :page-up)
                                           ((string= params "6") :page-down)
                                           (t :unknown)))
                                (t :unknown))
                              keys))))
                   ((= byte 27) (push :escape keys))
                   ((member byte '(13 10)) (push :enter keys))
                   ((member byte '(127 8)) (push :backspace keys))
                   ((= byte 9) (push :tab keys))
                   ((< byte 128) (push (code-char byte) keys))
                   (t ; UTF-8 sequence
                    (let* ((length (cond ((>= byte #xF0) 4) ((>= byte #xE0) 3) (t 2)))
                           (end (min n (+ i (1- length)))))
                      (push (char (sb-ext:octets-to-string
                                   (coerce (subseq octets (1- i) end) '(vector (unsigned-byte 8)))
                                   :external-format '(:utf-8 :replacement #\?))
                                  0)
                            keys)
                      (setf i end)))))))
    (nreverse keys)))

(defun read-key (terminal &optional (timeout-ms 100))
  "The next key pressed on TERMINAL, or NIL if none came within TIMEOUT-MS.
Keys are characters (control keys as their control characters) or one of
:UP :DOWN :LEFT :RIGHT :HOME :END :PAGE-UP :PAGE-DOWN :DELETE :ENTER
:ESCAPE :BACKSPACE :TAB."
  (or (pop (terminal-pending terminal))
      (when (wait-for-input terminal timeout-ms)
        (let ((buffer (make-array 256 :element-type '(unsigned-byte 8))))
          (let ((count (sb-sys:with-pinned-objects (buffer)
                         (sb-unix:unix-read (terminal-fd terminal)
                                            (sb-sys:vector-sap buffer) (length buffer)))))
            (unless (and count (plusp count))
              (error 'terminal-closed))
            (setf (terminal-pending terminal) (decode-keys (subseq buffer 0 count)))
            (pop (terminal-pending terminal)))))))
