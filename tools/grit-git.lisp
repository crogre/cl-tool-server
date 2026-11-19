(defpackage #:grit
  (:use #:cl #:tool-server)
  (:documentation "grit: a Magit-like Git client for the terminal."))

(in-package #:grit)

;;; The git side of grit: status, diffs, staging, commits, rebase, branches,
;;; remotes and worktrees, as plain data for the UI in grit-ui.lisp.
;;;
;;; The original grit links libgit2. This port runs the git CLI instead, on
;;; machine-readable output (porcelain v2, -z, --format, unified diffs), and
;;; stages hunks and lines by handing patches to `git apply', so the image
;;; needs no foreign library. It follows the installed git's behaviour and
;;; config, hooks included, rather than being independent of them.
;;;
;;; Diff text is read as Latin-1, which keeps every byte, so a patch built
;;; from it applies to files in any encoding; display text is decoded as
;;; UTF-8 separately.

(define-condition git-error (error)
  ((message :initarg :message :reader git-error-message))
  (:report (lambda (condition stream) (write-string (git-error-message condition) stream))))

(define-condition not-a-repository (error)
  ((directory :initarg :directory :reader not-a-repository-directory)))

(defun git-fail (control &rest args)
  (error 'git-error :message (apply #'format nil control args)))

;;; Running git

(defparameter *git-options*
  '("-c" "core.quotepath=false" "-c" "color.ui=false" "-c" "diff.noprefix=false"
    "-c" "diff.mnemonicprefix=false" "-c" "diff.relative=false"
    "-c" "diff.suppressBlankEmpty=false")
  "Options that keep git's output in the shape the parsers expect.")

(defparameter *git-environment*
  '("GIT_TERMINAL_PROMPT=0" "GIT_EDITOR=true" "GIT_SEQUENCE_EDITOR=true" "GIT_PAGER=cat")
  "Nothing git runs on its own may wait on a terminal it does not have.")

(defun environment (extra)
  "The server's environment with EXTRA, \"NAME=value\" strings, taking precedence."
  (flet ((name (entry) (subseq entry 0 (or (position #\= entry) (length entry)))))
    (let ((names (mapcar #'name extra)))
      (append extra (remove-if (lambda (entry) (member (name entry) names :test #'string=))
                               (sb-ext:posix-environ))))))

(defun run (command directory &key input raw environment plain)
  "Run COMMAND, a list of strings, in native DIRECTORY. Return its stdout, whether
it exited 0, and its stderr. INPUT is a file for stdin. RAW reads stdout as
Latin-1, byte for byte. ENVIRONMENT adds to *GIT-ENVIRONMENT*, or with PLAIN
to the server's own."
  ;; posix_spawn is far cheaper than fork in this image but ignores
  ;; :DIRECTORY, so a shell changes directory first.
  (multiple-value-bind (out err code)
      (uiop:run-program (list* "/bin/sh" "-c" "cd -- \"$1\" && shift && exec \"$@\"" "sh"
                               directory command)
                        :input input :output :string :error-output :string
                        :ignore-error-status t :use-posix-spawn t
                        :external-format (if raw :latin-1 '(:utf-8 :replacement #\?))
                        :environment (environment (if plain
                                                      environment
                                                      (append environment *git-environment*))))
    (values out (eql code 0) err)))

(defun flatten-message (text &optional (limit 300))
  "TEXT on one line, keeping its last LIMIT characters."
  (let* ((lines (remove "" (mapcar (lambda (line) (string-trim '(#\Space #\Tab #\Return) line))
                                   (uiop:split-string text :separator '(#\Newline)))
                        :test #'string=))
         (flat (format nil "~{~A~^ ~}" lines)))
    (if (> (length flat) limit)
        (concatenate 'string "…" (subseq flat (- (length flat) limit)))
        flat)))

(defstruct repo
  "WORKDIR is the top of the worktree; GIT-DIR its own git directory, which for
a linked worktree is not the shared one. Both are native, without a trailing /."
  workdir git-dir (empty-tree nil))

(defun git* (repo args &key raw input environment)
  "Run git with ARGS in REPO's worktree; return stdout, success and stderr."
  (run (append (list "git") *git-options* args) (repo-workdir repo)
       :raw raw :input input :environment environment))

(defun git (repo &rest args)
  "Run git with ARGS in REPO's worktree and return its stdout; signal GIT-ERROR
with git's message if it fails."
  (multiple-value-bind (out ok err) (git* repo args)
    (unless ok
      (git-fail "~A" (flatten-message (if (plusp (length (string-trim '(#\Space #\Newline) err)))
                                          err out))))
    out))

(defun git-ok (repo &rest args)
  "Stdout of git ARGS, or NIL if it fails."
  (multiple-value-bind (out ok) (git* repo args)
    (and ok out)))

(defun trim (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun split (string separator)
  (uiop:split-string string :separator (list separator)))

(defun short-oid (oid)
  (subseq oid 0 (min 7 (length oid))))

(defun open-repo (directory)
  "The repository whose worktree holds native DIRECTORY. Signals NOT-A-REPOSITORY."
  (multiple-value-bind (out ok)
      (run (list "git" "rev-parse" "--show-toplevel" "--absolute-git-dir") directory)
    (let ((lines (and ok (remove "" (split out #\Newline) :test #'string=))))
      (unless (= (length lines) 2)
        (error 'not-a-repository :directory directory))
      (make-repo :workdir (first lines) :git-dir (second lines)))))

(defun repo-path (repo relative)
  "Native path of RELATIVE, a path from the top of REPO's worktree."
  (concatenate 'string (repo-workdir repo) "/" relative))

(defun git-dir-file (repo name)
  (concatenate 'string (repo-git-dir repo) "/" name))

(defun read-file (path &key (external-format :utf-8))
  "Contents of the file at native PATH, or NIL."
  (with-open-file (in (uiop:parse-native-namestring path) :if-does-not-exist nil
                                                          :external-format external-format)
    (when in
      (let* ((buffer (make-string (file-length in)))
             (end (read-sequence buffer in)))
        (subseq buffer 0 end)))))

(defun write-file (path text &key (external-format :utf-8))
  (with-open-file (out (uiop:parse-native-namestring path) :direction :output
                                                           :if-exists :supersede
                                                           :external-format external-format)
    (write-string text out)))

(defun directory-exists-p (path)
  (let ((stat (handler-case (sb-posix:stat path) (sb-posix:syscall-error () nil))))
    (and stat (sb-posix:s-isdir (sb-posix:stat-mode stat)))))

(defun file-exists-p (path)
  (handler-case (progn (sb-posix:lstat path) t)
    (sb-posix:syscall-error () nil)))

;;; Status

(defstruct entry
  "A commit in a log. REFS are (NAME . KIND), KIND one of :HEAD :BRANCH :REMOTE :TAG."
  oid short parents author time summary refs)

(defstruct change
  "KIND is one of :ADDED :MODIFIED :DELETED :RENAMED :TYPECHANGE."
  path old-path kind)

(defstruct upstream name short-oid summary (ahead 0) (behind 0))

(defstruct rebase-state head-name current total conflicts)

(defstruct status
  "BRANCH is NIL when HEAD is detached or unborn; OID is NIL when unborn."
  branch detached oid summary upstream tag rebase
  untracked unstaged staged conflicted recent-commits)

(defun kind-label (kind)
  (ecase kind
    ((nil :added) "new file")
    (:modified "modified")
    (:deleted "deleted")
    (:renamed "renamed")
    (:typechange "typechange")))

(defun index-kind (char)
  (case char (#\A :added) (#\C :added) (#\D :deleted) (#\R :renamed) (#\T :typechange)
    (t :modified)))

(defun worktree-kind (char)
  (case char (#\A :added) (#\D :deleted) (#\R :renamed) (#\T :typechange) (t :modified)))

(defun split-fields (record count)
  "The first COUNT space-separated fields of RECORD and the rest of it."
  (let ((fields '()) (start 0))
    (dotimes (i count)
      (let ((space (position #\Space record :start start)))
        (unless space (return))
        (push (subseq record start space) fields)
        (setf start (1+ space))))
    (values (nreverse fields) (subseq record start))))

(defun parse-status (text)
  "Parse `git status --porcelain=v2 --branch -z' output into a STATUS with
the branch, upstream name and counts, and the four file sections filled in."
  (let ((status (make-status))
        (records (split text (code-char 0)))
        (untracked '()) (unstaged '()) (staged '()) (conflicted '()))
    (loop while records
          do (let ((record (pop records)))
               (cond
                 ((zerop (length record)))
                 ((uiop:string-prefix-p "# branch.oid " record)
                  (let ((oid (subseq record 13)))
                    (setf (status-oid status) (and (string/= oid "(initial)") oid))))
                 ((uiop:string-prefix-p "# branch.head " record)
                  (let ((head (subseq record 14)))
                    (if (string= head "(detached)")
                        (setf (status-detached status) t)
                        (setf (status-branch status) head))))
                 ((uiop:string-prefix-p "# branch.upstream " record)
                  (setf (status-upstream status) (make-upstream :name (subseq record 18))))
                 ((uiop:string-prefix-p "# branch.ab " record)
                  (destructuring-bind (ahead behind) (split (subseq record 12) #\Space)
                    (when (status-upstream status)
                      (setf (upstream-ahead (status-upstream status))
                            (parse-integer ahead :start 1)
                            (upstream-behind (status-upstream status))
                            (parse-integer behind :start 1)))))
                 ((uiop:string-prefix-p "? " record)
                  (push (subseq record 2) untracked))
                 ((uiop:string-prefix-p "u " record)
                  (push (make-change :path (nth-value 1 (split-fields record 10)) :kind :modified)
                        conflicted))
                 ((or (uiop:string-prefix-p "1 " record) (uiop:string-prefix-p "2 " record))
                  (let ((renamed (char= (char record 0) #\2)))
                    (multiple-value-bind (fields path) (split-fields record (if renamed 9 8))
                      (let ((x (char (second fields) 0))
                            (y (char (second fields) 1))
                            (old-path (and renamed (pop records))))
                        (unless (char= x #\.)
                          (push (make-change :path path :kind (index-kind x)
                                             :old-path (and (member x '(#\R #\C)) old-path))
                                staged))
                        (unless (char= y #\.)
                          (push (make-change :path path :kind (worktree-kind y)
                                             :old-path (and (member y '(#\R #\C)) old-path))
                                unstaged)))))))))
    (flet ((sorted (changes) (sort changes #'string< :key #'change-path)))
      (setf (status-untracked status) (sort untracked #'string<)
            (status-unstaged status) (sorted unstaged)
            (status-staged status) (sorted staged)
            (status-conflicted status) (sorted conflicted)))
    status))

(defun read-rebase-state (repo)
  "The rebase in progress in REPO's worktree, or NIL."
  (flet ((field (dir name)
           (let ((text (read-file (concatenate 'string dir "/" name))))
             (and text (trim text)))))
    (loop for (name current-file total-file) in '(("rebase-merge" "msgnum" "end")
                                                  ("rebase-apply" "next" "last"))
          for dir = (git-dir-file repo name)
          when (directory-exists-p dir)
            return (let ((current (parse-integer (or (field dir current-file) "") :junk-allowed t))
                         (total (parse-integer (or (field dir total-file) "") :junk-allowed t)))
                     (make-rebase-state :head-name (field dir "head-name")
                                        :current current :total (or total 0))))))

(defun rebasing-p (repo)
  (and (read-rebase-state repo) t))

(defparameter *recent-commits* 10)

(defun load-status (repo)
  "The full status of REPO: HEAD, upstream, nearest tag, rebase, files and
the most recent commits."
  (destructuring-bind (status-text upstream-text tag recent)
      (pmap #'funcall
            (list (lambda () (git repo "status" "--porcelain=v2" "--branch" "-z"
                                  "--untracked-files=normal"))
                  (lambda () (git-ok repo "log" "-1" "--format=%H%x1f%s" "@{upstream}"))
                  (lambda () (git-ok repo "describe" "--tags" "--abbrev=7" "HEAD"))
                  (lambda () (load-log repo :head *recent-commits*))))
    (let ((status (parse-status status-text)))
      (setf (status-tag status) (and tag (trim tag))
            (status-recent-commits status) recent
            (status-summary status) (and recent (status-oid status)
                                         (string= (entry-oid (first recent)) (status-oid status))
                                         (entry-summary (first recent))))
      (let ((upstream (status-upstream status))
            (fields (and upstream-text (split (trim upstream-text) (code-char #x1f)))))
        (if (and upstream fields)
            (setf (upstream-short-oid upstream) (short-oid (first fields))
                  (upstream-summary upstream) (second fields))
            (setf (status-upstream status) nil)))
      (let ((rebase (read-rebase-state repo)))
        (when rebase
          (setf (rebase-state-conflicts rebase) (and (status-conflicted status) t)
                (status-rebase status) rebase)))
      status)))

(defun untracked-p (status path)
  (member path (status-untracked status) :test #'string=))

;;; Log

(defun parse-decorations (text)
  "(NAME . KIND) decorations from %D with --decorate=full, as grit shows them."
  (loop for item in (mapcar #'trim (split text #\,))
        for head-arrow = (uiop:string-prefix-p "HEAD -> " item)
        for name = (cond (head-arrow (subseq item 8))
                         ((uiop:string-prefix-p "tag: " item) (subseq item 5))
                         (t item))
        for decoration = (cond ((string= item "HEAD") (cons "HEAD" :head))
                               ((uiop:string-prefix-p "refs/heads/" name)
                                (cons (format nil "~:[~;HEAD -> ~]~A" head-arrow (subseq name 11))
                                      :branch))
                               ((uiop:string-prefix-p "refs/remotes/" name)
                                (cons (subseq name 13) :remote))
                               ((uiop:string-prefix-p "refs/tags/" name)
                                (cons (subseq name 10) :tag)))
        when decoration collect decoration))

(defun parse-log (text)
  (loop for record in (split text (code-char #x1e))
        for fields = (split (string-left-trim '(#\Newline) record) (code-char #x1f))
        when (= (length fields) 6)
          collect (destructuring-bind (oid parents author time summary refs) fields
                    (make-entry :oid oid :short (short-oid oid)
                                :parents (remove "" (split parents #\Space) :test #'string=)
                                :author author
                                :time (parse-integer time :junk-allowed t)
                                :summary (trim summary)
                                :refs (parse-decorations refs)))))

(defun load-log (repo scope limit)
  "Up to LIMIT commits, newest first, reachable from SCOPE: :HEAD, :LOCAL
(local branches) or :ALL (local and remote-tracking branches)."
  (let ((out (apply #'git-ok repo "log" "--date-order" "--decorate=full"
                    (format nil "-n~D" limit)
                    "--format=%H%x1f%P%x1f%an%x1f%ct%x1f%s%x1f%D%x1e"
                    (ecase scope
                      (:head '("HEAD"))
                      (:local '("--branches"))
                      (:all '("--branches" "--remotes"))))))
    (if out (parse-log out) '())))

;;; Diffs

(defstruct diff-line
  "KIND is :CONTEXT, :ADD or :DEL. RAW is the line as git wrote it, in
Latin-1; TEXT is for display. NO-EOL marks a line without a final newline."
  kind raw text no-eol)

(defstruct hunk header old-start old-count new-start new-count lines)

(defstruct file-patch
  "HEADER holds git's lines before the first hunk. MINUS and PLUS are the
labels of the --- and +++ lines, \"/dev/null\" for a missing side."
  path header minus plus mode binary hunks)

(defun file-patch-new-p (patch) (equal (file-patch-minus patch) "/dev/null"))
(defun file-patch-deleted-p (patch) (equal (file-patch-plus patch) "/dev/null"))

(defun display-text (raw &optional (indent 0))
  "RAW, Latin-1 bytes of UTF-8 text, made fit for one terminal row: decoded,
tabs expanded as if starting at column INDENT, control characters shown as ?."
  (let ((text (sb-ext:octets-to-string (sb-ext:string-to-octets raw :external-format :latin-1)
                                       :external-format '(:utf-8 :replacement #\?)))
        (column indent))
    (with-output-to-string (out)
      (loop for char across text
            do (cond ((char= char #\Tab)
                      (loop do (write-char #\Space out) (incf column)
                            until (zerop (mod column 8))))
                     ((< (char-code char) 32) (write-char #\? out) (incf column))
                     (t (write-char char out) (incf column)))))))

(defun unquote-path (label)
  "A ---/+++ label or diff --git half without its a/ or b/ prefix, C quotes undone."
  (let ((text (if (and (> (length label) 1) (char= (char label 0) #\"))
                  (with-output-to-string (out)
                    (loop with i = 1
                          while (< i (1- (length label)))
                          do (let ((char (char label i)))
                               (if (char/= char #\\)
                                   (progn (write-char char out) (incf i))
                                   (let ((next (char label (1+ i))))
                                     (cond ((digit-char-p next 8)
                                            (write-char (code-char (parse-integer label :start (1+ i)
                                                                                        :end (+ i 4)
                                                                                        :radix 8))
                                                        out)
                                            (incf i 4))
                                           (t (write-char (case next (#\n #\Newline) (#\t #\Tab)
                                                            (t next))
                                                          out)
                                              (incf i 2))))))))
                  label)))
    (let ((path (if (and (> (length text) 2) (member (char text 0) '(#\a #\b))
                         (char= (char text 1) #\/))
                    (subseq text 2)
                    text)))
      ;; Paths come in as Latin-1 bytes; octal escapes made them bytes too.
      (sb-ext:octets-to-string (sb-ext:string-to-octets path :external-format :latin-1)
                               :external-format '(:utf-8 :replacement #\?)))))

(defun git-header-path (line)
  "The path of a `diff --git a/P b/P' line, for patches with no ---/+++ lines."
  (let* ((rest (subseq line 11))
         (half (floor (1- (length rest)) 2)))
    (unquote-path (if (and (> (length rest) 2) (char= (char rest half) #\Space))
                      (subseq rest (1+ half))
                      rest))))

(defun parse-range (text)
  "Start and count of \"-12,3\" or \"+7\"."
  (let ((comma (position #\, text)))
    (values (parse-integer text :start 1 :end comma)
            (if comma (parse-integer text :start (1+ comma)) 1))))

(defun parse-hunk-header (line)
  (let* ((parts (split line #\Space)))
    (multiple-value-bind (old-start old-count) (parse-range (second parts))
      (multiple-value-bind (new-start new-count) (parse-range (third parts))
        (make-hunk :header line :old-start old-start :old-count old-count
                   :new-start new-start :new-count new-count :lines '())))))

(defun parse-diff (text)
  "FILE-PATCHes in TEXT, unified diff output of git read as Latin-1."
  (let ((patches '()) (patch nil) (hunk nil) (lines '())
        (old-left 0) (new-left 0))
    (labels ((finish-hunk ()
               (when hunk
                 (setf (hunk-lines hunk) (coerce (nreverse lines) 'vector))
                 (push hunk (file-patch-hunks patch))
                 (setf hunk nil lines '())))
             (finish-file ()
               (finish-hunk)
               (when patch
                 (setf (file-patch-header patch) (nreverse (file-patch-header patch))
                       (file-patch-hunks patch) (nreverse (file-patch-hunks patch)))
                 (unless (file-patch-path patch)
                   (setf (file-patch-path patch)
                         (git-header-path (first (file-patch-header patch)))))
                 (push patch patches)
                 (setf patch nil))))
      (dolist (line (split text #\Newline))
        (cond
          ((uiop:string-prefix-p "diff --git " line)
           (finish-file)
           (setf patch (make-file-patch :header (list line))))
          ((null patch))
          ((uiop:string-prefix-p "@@ " line)
           (finish-hunk)
           (setf hunk (parse-hunk-header line)
                 old-left (hunk-old-count hunk)
                 new-left (hunk-new-count hunk)))
          ((null hunk)
           (push line (file-patch-header patch))
           (flet ((label () (let ((label (subseq line 4)))
                              ;; git adds a tab after labels containing spaces.
                              (string-right-trim '(#\Tab) label))))
             (cond ((uiop:string-prefix-p "--- " line)
                    (setf (file-patch-minus patch) (label))
                    (unless (string= (label) "/dev/null")
                      (setf (file-patch-path patch) (unquote-path (label)))))
                   ((uiop:string-prefix-p "+++ " line)
                    (setf (file-patch-plus patch) (label))
                    (unless (string= (label) "/dev/null")
                      (setf (file-patch-path patch) (unquote-path (label)))))
                   ((or (uiop:string-prefix-p "new file mode " line)
                        (uiop:string-prefix-p "deleted file mode " line))
                    (setf (file-patch-mode patch) (subseq line (1+ (position #\Space line :from-end t)))))
                   ((uiop:string-prefix-p "Binary files " line)
                    (setf (file-patch-binary patch) t)))))
          ((uiop:string-prefix-p "\\" line)
           (when lines (setf (diff-line-no-eol (first lines)) t)))
          ;; The header's counts end the hunk: an empty line can be blank
          ;; context, or what follows the diff.
          ((and (zerop old-left) (zerop new-left))
           (finish-hunk))
          (t
           (let ((kind (case (if (zerop (length line)) #\Space (char line 0))
                         (#\+ :add) (#\- :del) (#\Space :context))))
             (if kind
                 (let ((raw (if (zerop (length line)) "" (subseq line 1))))
                   (unless (eq kind :add) (decf old-left))
                   (unless (eq kind :del) (decf new-left))
                   (push (make-diff-line :kind kind :raw raw :text (display-text raw 5)) lines))
                 (finish-hunk))))))
      (finish-file)
      (nreverse patches))))

(defparameter *diff-options*
  '("--no-color" "--no-ext-diff" "--no-textconv" "--no-renames" "-U3"
    "--src-prefix=a/" "--dst-prefix=b/"))

(defun diff (repo &rest args)
  "FILE-PATCHes of `git diff ARGS'."
  (multiple-value-bind (out ok err) (git* repo (append '("diff") *diff-options* args) :raw t)
    ;; --no-index exits 1 when the files differ.
    (when (and (not ok) (plusp (length (trim err))))
      (git-fail "~A" (flatten-message err)))
    (parse-diff out)))

(defun file-patch (repo side path &key untracked conflicted)
  "PATH's patch on SIDE: :UNSTAGED (worktree against index, or against nothing
when UNTRACKED; against our side when CONFLICTED) or :STAGED (index against
HEAD). NIL when there is no textual patch, as for an untracked directory."
  (let ((patches (cond ((eq side :staged) (diff repo "--cached" "--" path))
                       (untracked
                        (if (directory-exists-p (repo-path repo path))
                            '()
                            (diff repo "--no-index" "--" "/dev/null" path)))
                       (conflicted (diff repo "-2" "--" path))
                       (t (diff repo "--" path)))))
    (find path patches :key #'file-patch-path :test #'string=)))

(defun empty-tree (repo)
  (or (repo-empty-tree repo)
      (setf (repo-empty-tree repo) (trim (git repo "hash-object" "-t" "tree" "/dev/null")))))

;;; Patches for hunks and lines

(defun hunk-text (hunk)
  "HUNK as git wrote it."
  (with-output-to-string (out)
    (format out "~A~%" (hunk-header hunk))
    (loop for line across (hunk-lines hunk)
          do (format out "~C~A~%" (ecase (diff-line-kind line) (:context #\Space) (:add #\+) (:del #\-))
                     (diff-line-raw line))
             (when (diff-line-no-eol line)
               (format out "\\ No newline at end of file~%")))))

(defun hunk-patch (patch hunk)
  "A patch of PATCH's header and HUNK alone."
  (format nil "~{~A~%~}~A" (file-patch-header patch) (hunk-text hunk)))

(defun swap-prefix (label from to)
  "LABEL, a/P or \"a/P\" style, with prefix FROM replaced by TO."
  (let ((quoted (and (plusp (length label)) (char= (char label 0) #\"))))
    (if quoted
        (concatenate 'string "\"" to (subseq label (+ 1 (length from))))
        (concatenate 'string to (subseq label (length from))))))

(defun partial-patch (patch hunk selected reverse)
  "A patch with only the SELECTED (indices into HUNK's lines) + and - lines of
HUNK, or NIL when nothing selected is a change. Forward it takes the old side
of PATCH to a partial new side, for staging; with REVERSE it takes the new
side back towards the old, for unstaging and discarding:

  line         forward    reverse
  context      context    context
  + selected   +          -
  + not        dropped    context
  - selected   -          +
  - not        context    dropped"
  (let ((body (make-string-output-stream))
        (old-count 0) (new-count 0) (changed nil))
    (loop for line across (hunk-lines hunk)
          for index from 0
          for chosen = (member index selected)
          for kind = (diff-line-kind line)
          for prefix = (ecase kind
                         (:context #\Space)
                         (:add (cond ((and chosen reverse) #\-) (chosen #\+) (reverse #\Space)))
                         (:del (cond ((and chosen reverse) #\+) (chosen #\-) (reverse nil)
                                     (t #\Space))))
          do (when (and chosen (not (eq kind :context)))
               (setf changed t))
             (when prefix
               (format body "~C~A~%" prefix (diff-line-raw line))
               (when (diff-line-no-eol line)
                 (format body "\\ No newline at end of file~%"))
               (unless (char= prefix #\+) (incf old-count))
               (unless (char= prefix #\-) (incf new-count))))
    (when changed
      (let* ((old-start (if reverse (hunk-new-start hunk) (hunk-old-start hunk)))
             ;; In a unified diff an empty side names the line before it.
             (new-start (cond ((zerop old-count) (1+ old-start))
                              ((zerop new-count) (1- old-start))
                              (t old-start)))
             (minus (file-patch-minus patch))
             (plus (file-patch-plus patch))
             (a (if (equal minus "/dev/null") (swap-prefix plus "b/" "a/") minus))
             (b (if (equal plus "/dev/null") (swap-prefix minus "a/" "b/") plus))
             ;; The file this patch applies to is missing when it is the
             ;; side the source patch creates from or deletes to.
             (creates (if reverse (file-patch-deleted-p patch) (file-patch-new-p patch))))
        (format nil "diff --git ~A ~A~%~:[--- ~A~%~;new file mode ~A~%--- /dev/null~%~]+++ ~A~%@@ -~D,~D +~D,~D @@~%~A"
                a b creates (if creates (or (file-patch-mode patch) "100644") a) b
                old-start old-count new-start new-count
                (get-output-stream-string body))))))

(defun apply-patch (repo text &rest options)
  "Run `git apply OPTIONS' on patch TEXT."
  (let ((file (git-dir-file repo "grit.patch")))
    (write-file file text :external-format :latin-1)
    (unwind-protect
         (multiple-value-bind (out ok err)
             (git* repo (append '("apply" "--whitespace=nowarn") options (list file)))
           (unless ok
             (git-fail "~A" (flatten-message (if (plusp (length (trim err))) err out)))))
      (ignore-errors (delete-file (uiop:parse-native-namestring file))))))

(defun nth-hunk (patch index path)
  (unless patch
    (git-fail "no patch available for ~A" path))
  (or (nth index (file-patch-hunks patch))
      (git-fail "hunk index ~D out of range (file has ~D hunks)"
                index (length (file-patch-hunks patch)))))

(defun stage-hunk (repo path index &key untracked)
  (let ((patch (file-patch repo :unstaged path :untracked untracked)))
    (apply-patch repo (hunk-patch patch (nth-hunk patch index path)) "--cached")))

(defun unstage-hunk (repo path index)
  (let ((patch (file-patch repo :staged path)))
    (apply-patch repo (hunk-patch patch (nth-hunk patch index path)) "--cached" "-R")))

(defun discard-hunk (repo path index)
  (let ((patch (file-patch repo :unstaged path)))
    (apply-patch repo (hunk-patch patch (nth-hunk patch index path)) "-R")))

(defun apply-lines (repo side path index selected reverse options &key untracked)
  (let* ((patch (file-patch repo side path :untracked untracked))
         (text (partial-patch patch (nth-hunk patch index path) selected reverse)))
    ;; A selection of context lines alone changes nothing.
    (when text
      (apply #'apply-patch repo text options))))

(defun stage-lines (repo path index selected &key untracked)
  (apply-lines repo :unstaged path index selected nil '("--cached") :untracked untracked))

(defun unstage-lines (repo path index selected)
  (apply-lines repo :staged path index selected t '("--cached")))

(defun discard-lines (repo path index selected)
  (apply-lines repo :unstaged path index selected t '()))

;;; Whole files

(defun stage-file (repo path)
  "Stage PATH: new, modified, deleted, or an untracked directory and all in it."
  (git repo "add" "-A" "--" path))

(defun unstage-file (repo path)
  (git repo "reset" "-q" "--" path))

(defun in-index-p (repo path)
  (plusp (length (git repo "ls-files" "--" path))))

(defun discard-file (repo path)
  "Throw away PATH's unstaged changes: delete it if untracked, else restore
it from the index."
  (if (in-index-p repo path)
      (git repo "checkout" "-q" "--" path)
      (run (list "rm" "-rf" "--" path) (repo-workdir repo))))

;;; Commits

(defun commit-message-path (repo)
  (git-dir-file repo "COMMIT_EDITMSG"))

(defun head-commit-message (repo)
  "HEAD's message, or NIL when HEAD is unborn."
  (let ((out (git-ok repo "log" "-1" "--format=%B" "HEAD")))
    (and out (string-right-trim '(#\Newline) out))))

(defun commit-template (status prior)
  "The text the editor opens on: PRIOR's message for an amend, then a comment
block summarising the index, worktree and untracked files."
  (with-output-to-string (out)
    (when prior
      (write-string prior out)
      (unless (and (plusp (length prior)) (char= (char prior (1- (length prior))) #\Newline))
        (terpri out)))
    (format out "~%# Please enter the commit message for your changes. Lines starting~%~
                 # with '#' will be ignored, and an empty message aborts the commit.~%#~%")
    (cond ((status-branch status) (format out "# On branch ~A~%" (status-branch status)))
          ((status-detached status)
           (format out "# HEAD detached at ~A~%" (short-oid (or (status-oid status) ""))))
          (t (format out "# On no branch (unborn)~%")))
    (format out "#~%# Changes to be committed:~%")
    (if (status-staged status)
        (dolist (change (status-staged status))
          (format out "#   ~A: ~A~%" (kind-label (change-kind change)) (change-path change)))
        (format out "#   (no staged changes)~%"))
    (when (status-unstaged status)
      (format out "#~%# Changes not staged for commit:~%")
      (dolist (change (status-unstaged status))
        (format out "#   ~A: ~A~%" (kind-label (change-kind change)) (change-path change))))
    (when (status-untracked status)
      (format out "#~%# Untracked files:~%")
      (dolist (path (status-untracked status))
        (format out "#   ~A~%" path)))))

(defun strip-comments (text)
  "TEXT without its #-comment lines, trimmed."
  (trim (format nil "~{~A~^~%~}"
                (remove-if (lambda (line) (uiop:string-prefix-p "#" (string-left-trim '(#\Space #\Tab) line)))
                           (split text #\Newline)))))

(defun commit (repo message &key amend)
  "Commit the index with MESSAGE, or amend HEAD with it. Returns the short oid."
  (let ((file (git-dir-file repo "GRIT_COMMIT_MSG")))
    (write-file file message)
    (unwind-protect
         (apply #'git repo "commit" "-q" "--cleanup=whitespace" "-F" file
                (and amend '("--amend" "--allow-empty")))
      (ignore-errors (delete-file (uiop:parse-native-namestring file))))
    (short-oid (trim (git repo "rev-parse" "HEAD")))))

;;; Rebase

(defun drive-rebase (repo &rest args)
  "Run git rebase ARGS. Stopping at a conflict is not an error: the rebase
state stays for the user, or for continuing."
  (multiple-value-bind (out ok err) (git* repo (cons "rebase" args))
    (unless (or ok (and (rebasing-p repo) (not (member "--abort" args :test #'string=))))
      (git-fail "~A" (flatten-message (if (plusp (length (trim err))) err out))))))

(defun rebase-onto (repo onto)
  (drive-rebase repo onto))

(defun conflicts-p (repo)
  (plusp (length (git repo "diff" "--name-only" "--diff-filter=U"))))

(defun rebase-continue (repo)
  (unless (rebasing-p repo) (git-fail "no rebase in progress"))
  (when (conflicts-p repo)
    (git-fail "unresolved conflicts remain; resolve and stage them first"))
  (drive-rebase repo "--continue"))

(defun rebase-abort (repo)
  (unless (rebasing-p repo) (git-fail "no rebase in progress"))
  (drive-rebase repo "--abort"))

(defun rebase-skip (repo)
  (unless (rebasing-p repo) (git-fail "no rebase in progress"))
  (drive-rebase repo "--skip"))

;;; Branches

(defun head-oid (repo)
  "HEAD's commit, or NIL when HEAD is unborn."
  (let ((out (git-ok repo "rev-parse" "-q" "--verify" "HEAD^{commit}")))
    (and out (trim out))))

(defun current-branch (repo)
  (let ((out (git-ok repo "symbolic-ref" "-q" "--short" "HEAD")))
    (and out (trim out))))

(defun checkout-branch (repo name)
  (git repo "switch" "-q" "--no-guess" name))

(defun create-and-checkout-branch (repo name at)
  (git repo "switch" "-q" "-c" name at))

(defun delete-branch (repo name)
  (git repo "branch" "-q" "-D" name))

(defun reset-mixed (repo oid)
  (git repo "reset" "-q" "--mixed" oid))

;;; Remotes

(defstruct push-spec remote local-branch remote-branch set-upstream)

(defun config (repo key)
  (let ((out (git-ok repo "config" "--get" key)))
    (and out (trim out))))

(defun remote-names (repo)
  (remove "" (split (git repo "remote") #\Newline) :test #'string=))

(defun push-target-upstream (repo)
  "Where `p u' pushes: the current branch's upstream."
  (let ((branch (or (current-branch repo) (git-fail "HEAD is detached; no upstream to push to"))))
    (make-push-spec
     :remote (or (config repo (format nil "branch.~A.remote" branch))
                 (git-fail "no upstream configured for this branch (try `p p` or `p e`)"))
     :local-branch branch
     :remote-branch (let ((merge (config repo (format nil "branch.~A.merge" branch))))
                      (cond ((null merge) branch)
                            ((uiop:string-prefix-p "refs/heads/" merge) (subseq merge 11))
                            (t merge))))))

(defun push-target-push-remote (repo)
  "Where `p p' pushes: branch.<b>.pushRemote, remote.pushDefault, the upstream's
remote or the only remote. Sets the upstream when the branch has none."
  (let ((branch (or (current-branch repo) (git-fail "HEAD is detached; nothing to push"))))
    (make-push-spec
     :remote (or (config repo (format nil "branch.~A.pushRemote" branch))
                 (config repo "remote.pushDefault")
                 (config repo (format nil "branch.~A.remote" branch))
                 (let ((remotes (remote-names repo)))
                   (and (= (length remotes) 1) (first remotes)))
                 (git-fail "no push-remote configured and not exactly one remote (try `p e`)"))
     :local-branch branch :remote-branch branch
     :set-upstream (not (config repo (format nil "branch.~A.merge" branch))))))

(defun push-target-elsewhere (repo input)
  "Parse \"REMOTE [BRANCH]\"; BRANCH defaults to the current branch's name."
  (let ((branch (or (current-branch repo) "HEAD"))
        (words (remove "" (split (trim input) #\Space) :test #'string=)))
    (unless words (git-fail "expected: <remote> [branch]"))
    (make-push-spec :remote (first words) :local-branch branch
                    :remote-branch (or (second words) branch)
                    :set-upstream (not (config repo (format nil "branch.~A.merge" branch))))))

(defun push-branch (repo spec force)
  "Push SPEC. FORCE is force-with-lease against our remote-tracking ref: it
fails if the remote branch moved since the last fetch."
  (let* ((remote-ref (format nil "refs/heads/~A" (push-spec-remote-branch spec)))
         (tracking (git-ok repo "rev-parse" "-q" "--verify"
                           (format nil "refs/remotes/~A/~A" (push-spec-remote spec)
                                   (push-spec-remote-branch spec)))))
    (apply #'git repo "push" "-q"
           (append (and force (list (format nil "--force-with-lease=~A:~A" remote-ref
                                            (if tracking (trim tracking) ""))))
                   (and (push-spec-set-upstream spec) '("-u"))
                   (list (push-spec-remote spec)
                         (format nil "refs/heads/~A:~A" (push-spec-local-branch spec) remote-ref))))))

(defun fetch (repo remote)
  (git repo "fetch" "-q" "--prune" remote))

(defun pull (repo spec)
  "Fetch SPEC's remote and bring its branch into HEAD: :UP-TO-DATE, a
fast-forward (returning :FAST-FORWARDED and the new short oid), or else a
rebase onto it, which may stop at a conflict (:REBASED)."
  (fetch repo (push-spec-remote spec))
  (let* ((tracking (format nil "refs/remotes/~A/~A" (push-spec-remote spec)
                           (push-spec-remote-branch spec)))
         (oid (let ((out (git-ok repo "rev-parse" "-q" "--verify" tracking)))
                (if out (trim out)
                    (git-fail "fetched, but no remote-tracking ref to integrate (nothing to pull)"))))
         (head (head-oid repo)))
    (cond ((and head (nth-value 1 (git* repo (list "merge-base" "--is-ancestor" oid head))))
           :up-to-date)
          ((or (null head) (nth-value 1 (git* repo (list "merge-base" "--is-ancestor" head oid))))
           (git repo "merge" "-q" "--ff-only" oid)
           (values :fast-forwarded (short-oid oid)))
          (t (rebase-onto repo tracking)
             :rebased))))

;;; Worktrees

(defstruct worktree path branch head-short current main locked)

(defun same-directory-p (a b)
  (flet ((true (path) (ignore-errors (namestring (truename (uiop:parse-native-namestring
                                                           path :ensure-directory t))))))
    (equal (or (true a) a) (or (true b) b))))

(defun worktrees (repo)
  "The main worktree and every linked one."
  (let ((worktrees '()) (current nil))
    (dolist (line (split (git repo "worktree" "list" "--porcelain") #\Newline))
      (cond ((uiop:string-prefix-p "worktree " line)
             (setf current (make-worktree :path (subseq line 9) :main (null worktrees)))
             (setf (worktree-current current) (same-directory-p (worktree-path current)
                                                                (repo-workdir repo)))
             (push current worktrees))
            ((null current))
            ((uiop:string-prefix-p "HEAD " line)
             (setf (worktree-head-short current) (short-oid (subseq line 5))))
            ((uiop:string-prefix-p "branch refs/heads/" line)
             (setf (worktree-branch current) (subseq line 18)))
            ((or (string= line "locked") (uiop:string-prefix-p "locked " line))
             (setf (worktree-locked current) t))
            ((string= line "bare") (setf worktrees (rest worktrees) current nil))))
    (nreverse worktrees)))

(defun create-worktree (repo path branch)
  "Add a worktree at native PATH with BRANCH (default: PATH's last component)
checked out, creating the branch at HEAD if it does not exist."
  (let* ((path (string-right-trim "/" path))
         (branch (or branch (subseq path (1+ (or (position #\/ path :from-end t) -1))))))
    (when (zerop (length branch)) (git-fail "invalid worktree path"))
    (if (git-ok repo "show-ref" "--verify" "-q" (format nil "refs/heads/~A" branch))
        (git repo "worktree" "add" "-q" path branch)
        (git repo "worktree" "add" "-q" "-b" branch path "HEAD"))
    path))

;;; Commit detail and diffs between commits

(defstruct commit-detail oid author email time message parents patches)

(defun commit-detail (repo oid)
  "OID's header, message and its patches against its first parent (against
nothing for a root commit)."
  (let* ((fields (split (git repo "log" "-1" "--format=%H%x1f%an%x1f%ae%x1f%ct%x1f%P%x1f%B" oid)
                        (code-char #x1f)))
         (parents (remove "" (split (fifth fields) #\Space) :test #'string=)))
    (make-commit-detail :oid (first fields) :author (second fields) :email (third fields)
                        :time (parse-integer (fourth fields) :junk-allowed t)
                        :message (string-right-trim '(#\Newline) (sixth fields))
                        :parents parents
                        :patches (diff repo (or (first parents) (empty-tree repo)) (first fields)))))

(defun diff-commit-range (repo oldest newest)
  "The combined patches of OLDEST through NEWEST: parent(OLDEST) to NEWEST."
  (let ((parent (git-ok repo "rev-parse" "-q" "--verify" (format nil "~A^" oldest))))
    (diff repo (if parent (trim parent) (empty-tree repo)) newest)))

(defun diff-vs-head (repo oid)
  "What OID has that HEAD lacks."
  (diff repo (or (head-oid repo) (git-fail "HEAD is unborn; nothing to compare to")) oid))
