(defsystem "tool-server"
  :description "Long-running SBCL image that serves CLI commands over a unix socket."
  :depends-on ((:require "sb-bsd-sockets") (:require "sb-posix"))
  :serial t
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
                             (:file "git-worktrees-ui")))))
