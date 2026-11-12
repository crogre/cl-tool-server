(in-package #:tool-server-user)

(defcache *directory-listings*)

(defun base-name (pathname)
  (let ((native (string-right-trim "/" (uiop:native-namestring pathname))))
    (subseq native (1+ (position #\/ native :from-end t)))))

(defun directory-entries (directory)
  "Sorted entry names of DIRECTORY, subdirectories with a trailing slash."
  (sort (nconc (mapcar (lambda (dir) (concatenate 'string (base-name dir) "/"))
                       (uiop:subdirectories directory))
               (mapcar #'base-name (uiop:directory-files directory)))
        #'string<))

(defcommand list-dir (&rest directories)
  "List each of DIRECTORIES (default: the current directory).
Listings are cached until the directory's mtime changes."
  (loop for (name . more) on (or directories '("."))
        for directory = (or (uiop:directory-exists-p (arg-directory name))
                            (fail "~A: not a directory" name))
        for mtime = (file-write-date directory)
        do (when (rest directories)
             (format t "~A:~%" name))
           (dolist (entry (if (< mtime (get-universal-time))
                              (cached (*directory-listings* (uiop:native-namestring directory)
                                       :stamp mtime)
                                (directory-entries directory))
                              ;; mtime has 1s resolution: a change later in this
                              ;; same second would not alter it.
                              (directory-entries directory)))
             (write-line entry))
           (when more
             (terpri))))

(defcommand show-args (&rest args)
  "Print the working directory and each argument, to check what arrives."
  (format t "cwd: ~A~%" (uiop:native-namestring *default-pathname-defaults*))
  (loop for arg in args
        for i from 1
        do (format t "~D: [~A]~%" i arg)))
