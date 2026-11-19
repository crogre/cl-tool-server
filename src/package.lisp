(defpackage #:tool-server
  (:use #:cl)
  (:export
   ;; Defining commands
   #:defcommand #:fail #:exit-with #:command-failed
   #:arg-pathname #:arg-directory #:*client-environment* #:client-getenv
   #:*backtrace-on-error*
   ;; Caches
   #:defcache #:cached #:cache-get #:cache-put #:cache-remove #:clear-cache
   ;; Utilities
   #:capture-output #:run-streaming #:pmap #:run-in-background #:log-message
   #:parse-json #:jget #:json-string
   ;; Terminal
   #:with-terminal #:terminal-size #:terminal-write #:terminal-flush #:read-key
   #:terminal-closed #:display-width #:fit-to-width #:fit-spans-to-width
   #:terminal-suspend #:terminal-resume #:run-on-terminal
   ;; Server
   #:start #:stop #:serve #:default-socket-path #:server-socket-path
   #:start-slynk #:stop-slynk))

(defpackage #:tool-server-user
  (:use #:cl #:tool-server)
  (:documentation "Home package for tools; commands run with *PACKAGE* bound to it."))
