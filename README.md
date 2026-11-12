# tool-server

A long-running SBCL image holding compiled tools and their caches. Shell
commands are thin symlinks to a small C client (`toolc`) that forwards argv and
cwd over a unix socket and relays stdout, stderr and the exit status. A call
costs about 3 ms, with no Lisp startup, and caches stay warm between calls.

## Quick start

```sh
make                          # builds bin/toolc
bin/tool-server               # foreground server (or: bin/tool-server --repl)
bin/toolc help                # in another terminal
bin/toolc install-shims ~/bin # symlink each command into a dir on PATH
list-dir src                  # runs (list-dir "src") in the image
```

The socket is `$TOOL_SERVER_SOCKET`, or `$TMPDIR/tool-server.sock` by default,
created with mode 0600. Unix socket paths are limited to 103 bytes.

## Connecting from Emacs (SLY)

```sh
bin/tool-server --slynk          # or --slynk=PORT; default 4005
bin/toolc slynk [PORT]           # or start it in a server that is already running
```

Then run `M-x sly-connect RET localhost RET 4005`. slynk is loaded from your
SLY checkout so that its version matches Emacs. It looks for `slynk.asd` in
straight and elpa locations under `~/.config/emacs` and `~/.emacs.d`, or in
`$SLYNK_DIR`. It listens on localhost only, where any local user can connect.

## Writing a tool

Put it in `tools/` (and add new files to `tool-server.asd`):

```lisp
(in-package #:tool-server-user)

(defcache *word-counts*)

(defcommand count-words (file)
  "Count the words in FILE."
  (let ((path (arg-pathname file)))   ; relative to the caller's cwd
    (unless (probe-file path) (fail "~A: no such file" file))
    (print (cached (*word-counts* (namestring path) :stamp (file-write-date path))
             (count-words-in path)))))
```

- Arguments arrive as strings. The lambda list may use required, `&optional`
  and `&rest` parameters. A wrong argument count prints the usage line and exits 2.
- Write to `*standard-output*` / `*error-output*`. Output is buffered and sent
  at the end or on `(finish-output)`.
- `(fail fmt args...)` prints `cmd: message` to stderr and exits 1, and
  `(exit-with n)` exits n silently. Any other error is reported the same way
  as `fail`. To get backtraces, set `*backtrace-on-error*` to true.
- `(defcommand (name :name "cli-name" :shim nil) ...)` sets the command's
  CLI name, or keeps it out of `install-shims`.
- Commands run in their own thread with `*default-pathname-defaults*` set to
  the client's cwd. Keep shared state thread-safe (caches already are).
- Never call `sb-ext:exit`, because it exits the whole server.

After editing, run `toolc reload`, which recompiles changed files into the
running image. Caches survive the reload. With `--repl` or SLY, you can
also redefine functions directly. Other built-ins are `eval`, `cache-stats`,
`cache-clear`, `slynk` and `shutdown`.

## Protocol

There is one request per connection. The client sends NUL-terminated UTF-8
fields: `"1"` (version), cwd, argc, then argv (argv[0] is the command name).
The server replies with frames, each a tag byte, a 4-byte big-endian length
and the payload: `O` for stdout, `E` for stderr, and finally `X` with the
decimal exit status.

## Not yet supported

- stdin is not forwarded, and commands see an empty stream.
- Environment variables are not forwarded.
- Interrupting the client does not interrupt the command. The command runs
  until it next writes output.
