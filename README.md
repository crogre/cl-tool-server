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

## Tools

- `git-worktree-status [-r|--refresh] [REPOSITORY...]` shows each worktree's
  path, branch, PR and commit age, newest first, with 💩 marking dirty ones.
  It works like the Python and Emacs versions. A worktree is rescanned only
  when its index or HEAD ref changes. PRs come from one batched GraphQL call
  per host. Stale PR answers print at once and refresh in the background.
  `--refresh` waits for everything. Invoked as a shim on PATH, it also works
  as `git worktree-status`.

  `-i` browses the list on your terminal. Use j/k to move, space to mark,
  1–4 to filter or sort by a column, / to filter, d to delete worktrees along
  with their Bazel output bases, and ? for the rest. Deletes run in the
  background while you keep browsing; leaving waits for them, and a second q
  leaves them to finish in the server. Enter prints the chosen path, so a
  shell function can cd to it:

  ```sh
  wt() { local d; d=$(git-worktree-status -i) && cd "$d"; }    # bash/zsh
  function wt; set -l d (git-worktree-status -i); and cd $d; end  # fish
  ```
- `list-dir [DIRECTORY...]` and `show-args [ARGS...]` are small examples.

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
- Helpers: `(capture-output '("git" "status") :directory dir)` returns
  stdout, success and stderr. It uses posix_spawn, which takes about 2 ms,
  where SBCL's default fork takes 20–70 ms. `(pmap fn list)` maps in
  parallel. `(run-in-background name fn)` runs work that outlives the
  command. `parse-json`, `jget` and `json-string` read and write JSON.
  `cache-get` / `cache-put` are for entries that expire by TTL rather than
  by stamp.
- Interactive commands use `(with-terminal (term) ...)` to draw on the
  client's terminal, which is in raw mode on the alternate screen, and
  `read-key` to read keys. Stdout still goes back to the caller as usual.
- Never call `sb-ext:exit`, because it exits the whole server.

After editing, run `toolc reload`, which recompiles changed files into the
running image. Caches survive the reload. With `--repl` or SLY, you can
also redefine functions directly. Other built-ins are `eval`, `cache-stats`,
`cache-clear`, `slynk` and `shutdown`.

## Protocol

There is one request per connection. The client sends NUL-terminated UTF-8
fields: `"2"` (version), cwd, tty, argc, then argv (argv[0] is the command
name). The tty field is the path of the client's terminal, or empty.
Interactive commands open that path themselves, since the server runs as the
same user. If the server leaves the terminal changed, the client restores it
on exit and on SIGINT, SIGTERM or SIGHUP.
The server replies with frames, each a tag byte, a 4-byte big-endian length
and the payload: `O` for stdout, `E` for stderr, and finally `X` with the
decimal exit status.

## Not yet supported

- stdin is not forwarded, so commands see an empty stream. Interactive
  commands read the terminal directly instead.
- Environment variables are not forwarded.
- Interrupting the client does not interrupt the command. The command runs
  until it next writes output.
