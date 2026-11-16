(in-package #:tool-server-user)

;;; git-worktree-status: path, branch, pull request and commit age of every
;;; worktree of a repository.
;;;
;;; `git status' costs the better part of a second in a large repository, so a
;;; worktree is only re-scanned when its git state has moved. The fingerprint
;;; is the index and the ref HEAD points at, read without spawning anything.
;;; Dirtiness is keyed to the index, so an edit that never touches the index
;;; can leave a stale flag until git does.
;;;
;;; Pull requests come from one GraphQL request per GitHub host: aliased fields
;;; answer for every branch at once, for one rate-limit point. Which PR a
;;; branch holds rarely changes, so that is re-asked only when the branch's
;;; remote-tracking ref moves (a push) or after *PR-MAPPING-TTL*; the PR's state
;;; is refreshed by number once older than *PR-STATE-TTL*. Stale answers are
;;; printed at once and refreshed in the background, so a call only waits for
;;; GitHub when it meets a branch it has never asked about.

(defparameter *remote* "origin"
  "Remote whose URL names the GitHub repository and whose HEAD is the default branch.")

(defparameter *pr-state-ttl* 60
  "Seconds before a PR's state is refreshed. Also how long a branch without a
PR is believed, since one can appear at any moment. Merged PRs are final.")

(defparameter *pr-mapping-ttl* 86400
  "Seconds before a branch is asked again which PR it holds; a push asks sooner.")

(defparameter *pr-query-batch* 100 "Branches asked about per GraphQL request.")

(defparameter *git-workers* 8 "How many worktrees are scanned at once.")

(defcache *worktree-scans*
  "Worktree path -> (COMMIT-TIME DIRTY), stamped with the worktree's git fingerprint.")

(defcache *pull-requests*
  "(FORGE-KEY . BRANCH) -> PR, what GitHub last said about the branch.")

(defstruct (worktree (:conc-name wt-))
  "PR is the PR cell, NIL until known; SCANNED is set once AGE and DIRTY are."
  path repo forge branch note age dirty scanned pr)

(defstruct (repo (:constructor %make-repo))
  dir worktrees default-branch heads)

(defstruct pr
  "NUMBER is NIL when the branch has no PR. SHA is the remote-tracking ref when
the mapping was taken; MAPPED and CHECKED are when the mapping and the state
were last confirmed."
  number state draft sha mapped checked)

;;; Files and processes

(defun unix-now ()
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

(defun join (directory name)
  (concatenate 'string directory "/" name))

(defun absolute (path base)
  (if (uiop:string-prefix-p "/" path) path (join base path)))

(defun trim (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun remove-prefix (prefix string)
  (if (uiop:string-prefix-p prefix string) (subseq string (length prefix)) string))

(defun lines (string)
  (remove "" (uiop:split-string string :separator '(#\Newline)) :test #'string=))

(defun read-trimmed (path)
  "Contents of the file at native PATH without surrounding whitespace, or NIL."
  (ignore-errors
   (with-open-file (in (uiop:parse-native-namestring path) :if-does-not-exist nil)
     (when in
       (let* ((buffer (make-string (file-length in)))
              (end (read-sequence buffer in)))
         (trim (subseq buffer 0 end)))))))

(defun file-stamp (path)
  "Inode, mtime and size of PATH, or NIL. Git replaces the index and refs by
renaming a new file over them, so the inode changes on every write even when
the mtime, at one-second resolution, does not."
  (handler-case (let ((stat (sb-posix:stat path)))
                  (list (sb-posix:stat-ino stat) (sb-posix:stat-mtime stat)
                        (sb-posix:stat-size stat)))
    (sb-posix:syscall-error () nil)))

(defun directory-p (path)
  (handler-case (sb-posix:s-isdir (sb-posix:stat-mode (sb-posix:stat path)))
    (sb-posix:syscall-error () nil)))

(defun git (directory &rest args)
  "Stdout of git ARGS run in DIRECTORY, or NIL if git failed."
  (multiple-value-bind (out ok) (capture-output (list* "git" args) :directory directory)
    (and ok out)))

;;; Reading worktrees

(defun git-dir (path)
  "Git directory of the worktree at PATH, resolving a .git file."
  (let ((dotgit (join path ".git")))
    (if (directory-p dotgit)
        dotgit
        (let ((ref (read-trimmed dotgit)))
          (when (and ref (uiop:string-prefix-p "gitdir: " ref))
            (absolute (subseq ref (length "gitdir: ")) path))))))

(defun common-dir (gitdir)
  "Repository-wide git directory for GITDIR, which may be a worktree's own."
  (let ((relative (read-trimmed (join gitdir "commondir"))))
    (if relative (absolute relative gitdir) gitdir)))

(defun scan-key (path)
  "Fingerprint of the git state of the worktree at PATH, or NIL."
  (let* ((gitdir (git-dir path))
         (index (and gitdir (file-stamp (join gitdir "index")))))
    (when index
      (let* ((head (read-trimmed (join gitdir "HEAD")))
             (ref (and head (uiop:string-prefix-p "ref: " head)
                       (trim (subseq head (length "ref: ")))))
             (ref-stamp (or (and ref (or (file-stamp (join gitdir ref))
                                         (file-stamp (join (common-dir gitdir) ref))))
                            ;; Detached, or a packed ref.
                            (file-stamp (join gitdir "HEAD")))))
        (list index ref-stamp head)))))

(defun detached-state (path)
  "(BRANCH . NOTE) for a worktree whose HEAD a rebase or bisect detached, or NIL.
`git worktree list' reports no branch for it, but git records the branch."
  (let ((gitdir (git-dir path)))
    (when gitdir
      (loop for (file . note) in '(("rebase-merge/head-name" . "rebasing")
                                   ("rebase-apply/head-name" . "rebasing")
                                   ("BISECT_START" . "bisecting"))
            for ref = (read-trimmed (join gitdir file))
            when (and ref (plusp (length ref)))
              return (cons (remove-prefix "refs/heads/" ref) note)))))

(defun parse-worktrees (porcelain repo forge)
  (let ((result '()) (path nil) (branch nil))
    (flet ((flush ()
             (when path
               (let ((detached (unless branch (detached-state path))))
                 (push (make-worktree :path path :repo repo :forge forge
                                      :branch (or branch (car detached))
                                      :note (cdr detached))
                       result)))
             (setf path nil branch nil)))
      (dolist (line (lines porcelain))
        (cond ((uiop:string-prefix-p "worktree " line)
               (flush)
               (setf path (subseq line (length "worktree "))))
              ((uiop:string-prefix-p "branch " line)
               (setf branch (remove-prefix "refs/heads/" (subseq line (length "branch ")))))))
      (flush))
    (nreverse result)))

(defun parse-forge (url)
  "(HOST OWNER NAME) named by a git remote URL, or NIL if it is not forge-shaped.
Parsed locally rather than asked of gh, which would spend an API call."
  (let* ((rest (let ((scheme (search "://" url)))
                 (if scheme (subseq url (+ scheme 3)) url)))
         (at (position #\@ rest))
         (slash (position #\/ rest))
         (rest (if (and at (or (null slash) (< at slash))) (subseq rest (1+ at)) rest))
         (separator (position-if (lambda (c) (member c '(#\: #\/))) rest)))
    (when (and separator (plusp separator))
      (let* ((host (subseq rest 0 separator))
             (path (subseq rest (1+ separator)))
             (port-end (position #\/ path))
             (path (if (and (char= (char rest separator) #\:) port-end (plusp port-end)
                            (every #'digit-char-p (subseq path 0 port-end)))
                       (subseq path (1+ port-end))
                       path))
             (path (string-right-trim "/" path))
             (path (if (uiop:string-suffix-p path ".git")
                       (subseq path 0 (- (length path) 4))
                       path))
             (owner-end (position #\/ path)))
        (when (and owner-end (plusp owner-end) (< (1+ owner-end) (length path)))
          (list host (subseq path 0 owner-end) (subseq path (1+ owner-end))))))))

(defun forge-key (forge)
  "Cache key for FORGE; GitHub ignores case in owner and repository names."
  (string-downcase (format nil "~{~A~^/~}" forge)))

(defun remote-heads (dir)
  "Branch -> commit of the remote-tracking refs, the trace a push leaves."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (line (lines (or (git dir "for-each-ref" "--format=%(refname:strip=3) %(objectname)"
                                  (format nil "refs/remotes/~A/" *remote*))
                             "")))
      (let ((space (position #\Space line :from-end t)))
        (when space
          (setf (gethash (subseq line 0 space) table) (subseq line (1+ space))))))
    table))

(defun load-repo (directory)
  "Read the worktrees of the repository checked out at DIRECTORY."
  (let ((dir (string-right-trim "/" (uiop:native-namestring (arg-directory directory)))))
    (destructuring-bind (porcelain url default heads)
        (pmap #'funcall
              (list (lambda () (git dir "worktree" "list" "--porcelain"))
                    (lambda () (git dir "config" "--get" (format nil "remote.~A.url" *remote*)))
                    (lambda () (git dir "symbolic-ref" "--short"
                                    (format nil "refs/remotes/~A/HEAD" *remote*)))
                    (lambda () (remote-heads dir))))
      (unless porcelain
        (fail "~A: not a git repository" directory))
      (%make-repo :dir dir
                  :worktrees (parse-worktrees porcelain dir (and url (parse-forge (trim url))))
                  :default-branch (and default (remove-prefix (format nil "~A/" *remote*)
                                                              (trim default)))
                  :heads heads))))

;;; Commit age and dirtiness

(defparameter *scan-script*
  "git log -1 --format=%ct 2>/dev/null; echo ---; git status --porcelain 2>/dev/null | head -n 1"
  "One process per worktree: commit time, a marker, and whether anything is dirty.")

(defun scan-worktree (path refresh)
  "(COMMIT-TIME DIRTY) of the worktree at PATH; COMMIT-TIME is NIL if unknown."
  (flet ((scan ()
           (let* ((output (lines (or (capture-output (list "sh" "-c" *scan-script*)
                                                     :directory path)
                                     "")))
                  (marker (position "---" output :test #'string=)))
             (list (and marker (plusp marker)
                        (parse-integer (first output) :junk-allowed t))
                   (and marker (< (1+ marker) (length output)))))))
    (let ((key (scan-key path)))
      (cond ((not (directory-p path)) (list nil nil))
            ((null key) (scan))
            (t (cached (*worktree-scans* path :stamp key :refresh refresh)
                 (scan)))))))

;;; Pull requests

(defun pr-key (forge branch)
  (cons (forge-key forge) branch))

(defun pr-need (pr sha now refresh)
  "What to ask GitHub about a branch: :MAPPING, :STATE or NIL. PR is its cached
answer and SHA its remote-tracking commit now."
  (cond ((or refresh (null pr)
             (not (equal (pr-sha pr) sha))
             (> (- now (pr-mapped pr)) *pr-mapping-ttl*)
             ;; A branch without a PR is one `gh pr create' away from one.
             (and (null (pr-number pr)) (> (- now (pr-checked pr)) *pr-state-ttl*)))
         :mapping)
        ((and (pr-number pr)
              (not (equal (pr-state pr) "MERGED"))
              (> (- now (pr-checked pr)) *pr-state-ttl*))
         :state)))

(defun pr-questions (repos now refresh)
  "The (FORGE BRANCH KIND NUMBER SHA) questions to ask GitHub, and whether any
concerns a branch never asked about before."
  (let ((questions '()) (unknown nil))
    (dolist (repo repos)
      (dolist (wt (repo-worktrees repo))
        (let ((forge (wt-forge wt))
              (branch (wt-branch wt)))
          (when (and forge branch (not (equal branch (repo-default-branch repo))))
            (let* ((pr (cache-get *pull-requests* (pr-key forge branch)))
                   (sha (gethash branch (repo-heads repo)))
                   (need (pr-need pr sha now refresh)))
              (when need
                (unless pr (setf unknown t))
                (pushnew (list forge branch need (and pr (pr-number pr)) sha)
                         questions :test #'equal)))))))
    (values (nreverse questions) unknown)))

(defun group-by (key list)
  "LIST as ((KEY . ITEMS) ...), in order of first appearance."
  (let ((groups '()))
    (dolist (item list)
      (let ((group (assoc (funcall key item) groups :test #'equal)))
        (if group
            (push item (cdr group))
            (push (list (funcall key item) item) groups))))
    (nreverse (mapcar (lambda (group) (cons (car group) (reverse (cdr group)))) groups))))

(defun batches (list size)
  (loop while list
        collect (loop repeat size while list collect (pop list))))

(defun pr-query (groups)
  "GraphQL asking about GROUPS, ((FORGE . ITEMS) ...) with ITEMS (BRANCH KIND
NUMBER SHA). Every repository and item gets an alias, r0 b0 ..., so one request
answers for all of them. A :MAPPING item asks which PRs its branch holds; a
:STATE item asks after one PR by number."
  (with-output-to-string (out)
    (write-string "query { " out)
    (loop for ((nil owner name) . items) in groups
          for i from 0
          do (format out "r~D: repository(owner: ~A, name: ~A) { "
                     i (json-string owner) (json-string name))
             (loop for (branch kind number) in items
                   for j from 0
                   do (if (eq kind :state)
                          (format out "b~D: pullRequest(number: ~D) { number state isDraft } "
                                  j number)
                          (format out "b~D: pullRequests(headRefName: ~A, first: 5, ~
                                   orderBy: {field: UPDATED_AT, direction: DESC}) ~
                                   { nodes { number state isDraft } } "
                                  j (json-string branch))))
             (write-string "} " out))
    (write-string "}" out)))

(defun pr-rank (state draft)
  "Preference between several PRs on one branch: open, draft, merged, closed."
  (cond ((and draft (equal state "OPEN")) 3)
        ((equal state "OPEN") 4)
        ((equal state "MERGED") 2)
        ((equal state "CLOSED") 1)
        (t 0)))

(defun best-pr (nodes)
  "The PR of NODES most worth showing; ties go to the most recently updated."
  (let ((best nil))
    (dolist (node nodes best)
      (when (or (null best)
                (> (pr-rank (jget node "state") (jget node "isDraft"))
                   (pr-rank (jget best "state") (jget best "isDraft"))))
        (setf best node)))))

(defun ask-github (host groups)
  "Ask HOST about GROUPS in one request and cache the answers.
Returns NIL, or a message saying why GitHub could not answer."
  (let ((now (unix-now)))
    (multiple-value-bind (out ok err)
        (capture-output (list "gh" "api" "graphql" "--hostname" host
                              "-f" (concatenate 'string "query=" (pr-query groups))))
      (let* ((reply (and (plusp (length out)) (ignore-errors (parse-json out))))
             (data (jget reply "data")))
        (when data
          (loop for (forge . items) in groups
                for i from 0
                ;; A repository gh cannot see answers null; its branches keep
                ;; whatever the cache had.
                for repo = (jget data (format nil "r~D" i))
                when repo
                  do (loop for (branch kind nil sha) in items
                           for j from 0
                           for answer = (jget repo (format nil "b~D" j))
                           for node = (if (eq kind :state) answer (best-pr (jget answer "nodes")))
                           for old = (cache-get *pull-requests* (pr-key forge branch))
                           when answer
                             do (cache-put *pull-requests* (pr-key forge branch)
                                           (make-pr :number (jget node "number")
                                                    :state (jget node "state")
                                                    :draft (jget node "isDraft")
                                                    :sha (if (eq kind :state) (pr-sha old) sha)
                                                    :mapped (if (eq kind :state) (pr-mapped old) now)
                                                    :checked now)))))
        (unless (and ok data)
          (format nil "~A: ~A" host (trim (or (jget reply "errors" 0 "message") err ""))))))))

(defun ask-github-all (questions)
  "Ask about QUESTIONS, one request per host and batch; return the failures."
  (loop for (host . host-questions) in (group-by #'caar questions)
        nconc (loop for batch in (batches host-questions *pr-query-batch*)
                    for groups = (mapcar (lambda (group) (cons (car group) (mapcar #'cdr (cdr group))))
                                         (group-by #'first batch))
                    for failure = (ask-github host groups)
                    when failure collect failure)))

(defvar *pr-refresh-lock* (sb-thread:make-mutex :name "pull request refresh"))
(defvar *pr-refresh-running* nil)

(defun ask-github-in-background (questions)
  "Refresh QUESTIONS for the next call, unless a refresh is already running."
  (sb-thread:with-mutex (*pr-refresh-lock*)
    (unless *pr-refresh-running*
      (setf *pr-refresh-running* t)
      (run-in-background
       "pull request refresh"
       (lambda ()
         (unwind-protect
              (dolist (failure (ask-github-all questions))
                (log-message "pull request refresh: ~A" failure))
           (sb-thread:with-mutex (*pr-refresh-lock*)
             (setf *pr-refresh-running* nil))))))))

;;; Output

(defun human-age (seconds)
  (let* ((minutes (/ seconds 60))
         (hours (/ minutes 60))
         (days (/ hours 24)))
    (cond ((< minutes 60) (format nil "~Dm" (floor minutes)))
          ((< hours 24) (format nil "~Dh" (floor hours)))
          ((< days 30) (format nil "~Dd" (floor days)))
          ((< days 365) (format nil "~Dmo" (floor days 30)))
          (t (format nil "~,1Fy" (/ days 365))))))

(defun pr-emoji (state draft)
  (cond ((and draft (equal state "OPEN")) "📝")
        ((equal state "OPEN") "  ")
        ((equal state "MERGED") "✅")
        ((equal state "CLOSED") "❌")
        (t "❔")))

(defun pr-cell (wt repo)
  "PR cell for WT: \"\" when the branch cannot have a PR or has none, NIL when
nothing is known about it yet."
  (if (and (wt-forge wt) (wt-branch wt)
           (not (equal (wt-branch wt) (repo-default-branch repo))))
      (let ((pr (cache-get *pull-requests* (pr-key (wt-forge wt) (wt-branch wt)))))
        (cond ((null pr) nil)
              ((pr-number pr)
               (format nil "~A #~D" (pr-emoji (pr-state pr) (pr-draft pr)) (pr-number pr)))
              (t "")))
      ""))

(defun update-pr-cells (repos &key (unknown nil))
  "Fill each worktree's PR cell from the cache; UNKNOWN for branches it lacks."
  (dolist (repo repos)
    (dolist (wt (repo-worktrees repo))
      (setf (wt-pr wt) (or (pr-cell wt repo) unknown)))))

(defun apply-scan (wt scan now)
  (destructuring-bind (commit dirty) scan
    (setf (wt-age wt) (and commit (max 0 (- now commit)))
          (wt-dirty wt) dirty
          (wt-scanned wt) t)))

(defun row-cells (wt)
  "Path, branch, PR and age cells of WT; \"…\" for what is still loading."
  (list (abbreviate-home (wt-path wt))
        (format nil "~A~@[ (~A)~]" (or (wt-branch wt) "(detached)") (wt-note wt))
        (or (wt-pr wt) "…")
        (if (wt-scanned wt)
            (format nil "~A~:[~; 💩~]" (if (wt-age wt) (human-age (wt-age wt)) "?") (wt-dirty wt))
            "…")))

(defun newer-p (a b)
  "Whether worktree A has the newer commit; unknown ages sort last."
  (and (wt-age a) (or (null (wt-age b)) (< (wt-age a) (wt-age b)))))

(defun abbreviate-home (path)
  (let ((home (string-right-trim "/" (uiop:native-namestring (user-homedir-pathname)))))
    (if (or (string= path home) (uiop:string-prefix-p (concatenate 'string home "/") path))
        (concatenate 'string "~" (subseq path (length home)))
        path)))

(defun print-table (headers rows)
  (let ((widths (apply #'mapcar (lambda (&rest cells) (reduce #'max cells :key #'display-width))
                       headers rows)))
    (flet ((print-row (cells)
             (loop for (cell . more) on cells
                   for width in widths
                   do (write-string cell)
                      (when more
                        (format t "~v@T" (+ 2 (- width (display-width cell))))))
             (terpri)))
      (print-row headers)
      (print-row (mapcar (lambda (width) (make-string width :initial-element #\-)) widths))
      (mapc #'print-row rows))))

(declaim (ftype function worktree-ui))

(defparameter *usage* "usage: git-worktree-status [-i|--interactive] [-r|--refresh] [REPOSITORY...]")

(defcommand git-worktree-status (&rest args)
  "Show the worktrees of a repository: path, branch, pull request and commit age.
usage: git-worktree-status [-i|--interactive] [-r|--refresh] [REPOSITORY...]

REPOSITORY is any checkout of a repository; the default is the current
directory. Worktrees are rescanned only when their git state has changed.
Cached pull requests print at once and are refreshed in the background
when stale. --refresh rescans everything and waits for GitHub.

--interactive browses the list on the terminal and prints the worktree
chosen with enter, e.g. for: cd \"$(git-worktree-status -i)\". Press ? there
for its keys."
  (let ((refresh nil) (interactive nil) (directories '()))
    (dolist (arg args)
      (cond ((member arg '("-r" "--refresh") :test #'string=) (setf refresh t))
            ((member arg '("-i" "--interactive") :test #'string=) (setf interactive t))
            ((uiop:string-prefix-p "-" arg)
             (error 'command-failed :code 2
                                    :message (format nil "unknown option ~A~%~A" arg *usage*)))
            (t (push arg directories))))
    (let ((directories (or (nreverse directories) '("."))))
      (if interactive
          (worktree-ui directories refresh)
          (print-worktrees (mapcar #'load-repo directories) refresh)))))

(defun print-worktrees (repos refresh)
  (let ((now (unix-now))
        (worktrees (loop for repo in repos append (repo-worktrees repo))))
    (multiple-value-bind (questions unknown) (pr-questions repos now refresh)
      (let* ((wait (and questions (or refresh unknown)))
             (pr-thread (when wait
                          (sb-thread:make-thread
                           (lambda ()
                             (handler-case (ask-github-all questions)
                               (error (condition) (list (princ-to-string condition)))))
                           :name "pull request lookup")))
             (scans (pmap (lambda (wt) (scan-worktree (wt-path wt) refresh))
                          worktrees :workers *git-workers*))
             (failures (and pr-thread (sb-thread:join-thread pr-thread))))
        (when (and questions (not wait))
          (ask-github-in-background questions))
        (loop for wt in worktrees
              for scan in scans
              do (apply-scan wt scan now))
        (update-pr-cells repos :unknown "")
        (print-table '("Path" "Branch" "PR" "Age")
                     (mapcar #'row-cells (stable-sort (copy-list worktrees) #'newer-p)))
        (dolist (failure failures)
          (format *error-output* "git-worktree-status: pull requests: ~A~%" failure))))))
