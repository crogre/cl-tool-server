(defpackage #:tool-server
  (:use #:cl)
  (:export
   ;; Defining commands
   #:defcommand #:fail #:exit-with #:command-failed
   #:arg-pathname #:arg-directory
   #:*backtrace-on-error*
   ;; Caches
   #:defcache #:cached #:clear-cache
   ;; Server
   #:start #:stop #:serve #:default-socket-path
   #:start-slynk #:stop-slynk))

(defpackage #:tool-server-user
  (:use #:cl #:tool-server)
  (:documentation "Home package for tools; commands run with *PACKAGE* bound to it."))
