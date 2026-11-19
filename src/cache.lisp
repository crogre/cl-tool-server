(in-package #:tool-server)

(defstruct (cache (:constructor make-cache (name)))
  name
  (table (make-hash-table :test 'equal :synchronized t))
  (hits 0 :type sb-ext:word)
  (misses 0 :type sb-ext:word))

(defvar *caches* (make-hash-table :test 'equal :synchronized t))

(defun register-cache (cache)
  (setf (gethash (cache-name cache) *caches*) cache))

(defun all-caches ()
  (sort (sb-ext:with-locked-hash-table (*caches*)
          (loop for cache being the hash-values of *caches* collect cache))
        #'string< :key #'cache-name))

(defmacro defcache (var &optional documentation)
  "Define VAR as a cache. Being a DEFVAR, it keeps its contents across reloads."
  `(defvar ,var (register-cache (make-cache ,(string-trim "*" (string-downcase var))))
     ,@(when documentation (list documentation))))

(defun call-with-cache (cache key stamp thunk &optional refresh)
  (let ((entry (gethash key (cache-table cache))))
    (cond ((and entry (not refresh) (equal (car entry) stamp))
           (sb-ext:atomic-incf (cache-hits cache))
           (cdr entry))
          (t
           (sb-ext:atomic-incf (cache-misses cache))
           (let ((value (funcall thunk)))
             (setf (gethash key (cache-table cache)) (cons stamp value))
             value)))))

(defmacro cached ((cache key &key stamp refresh) &body body)
  "Return the value stored in CACHE under KEY (compared with EQUAL). BODY computes
it when missing, when STAMP (e.g. a file's mtime) differs from the stored one,
or when REFRESH is true. Concurrent misses on one key may both run BODY; the
last one wins."
  `(call-with-cache ,cache ,key ,stamp (lambda () ,@body) ,refresh))

(defun cache-get (cache key)
  "The value stored in CACHE under KEY, and whether there was one."
  (let ((entry (gethash key (cache-table cache))))
    (if entry
        (progn (sb-ext:atomic-incf (cache-hits cache))
               (values (cdr entry) t))
        (progn (sb-ext:atomic-incf (cache-misses cache))
               (values nil nil)))))

(defun cache-put (cache key value)
  "Store VALUE in CACHE under KEY, for entries whose freshness the caller judges."
  (setf (gethash key (cache-table cache)) (cons nil value))
  value)

(defun cache-remove (cache key)
  "Forget what CACHE holds under KEY."
  (remhash key (cache-table cache)))

(defun clear-cache (cache)
  (clrhash (cache-table cache))
  (setf (cache-hits cache) 0
        (cache-misses cache) 0)
  cache)
