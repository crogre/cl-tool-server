(defsystem "tool-server"
  :description "Long-running SBCL image that serves CLI commands over a unix socket."
  :depends-on ((:require "sb-bsd-sockets") (:require "sb-posix"))
  :serial t
  :in-order-to ((test-op (test-op "tool-server/tests")))
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "commands")
                             (:file "cache")
                             (:file "util")
                             (:file "json")
                             (:file "terminal")
                             (:file "server")
                             (:file "slynk")
                             (:file "builtins")))
               (:module "tools"
                :serial t
                :components ((:file "files")
                             (:file "git-worktrees")
                             (:file "git-worktrees-ui")
                             (:file "grit-git")
                             (:file "grit-log")
                             (:file "grit-ui")
                             (:file "grit-ai")))))

(defsystem "tool-server/tests"
  :depends-on ("tool-server")
  :components ((:module "tests" :components ((:file "grit-tests"))))
  :perform (test-op (op system)
             (unless (uiop:symbol-call '#:grit-tests '#:run-tests)
               (error "tests failed"))))
