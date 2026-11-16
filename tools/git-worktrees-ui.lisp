(in-package #:tool-server-user)

;;; git-worktree-status -i: browse worktrees on the client's terminal.
;;;
;;; Rows show at once with whatever the caches hold, and a loader thread fills
;;; in what has changed. Each column has a number key (1-4) that walks it
;;; through: picked for filtering (◆), sorted ascending (▲), descending (▼),
;;; off. The PR column takes two more steps that sort by state, then number
;;; (▲▲ ▼▼). One column filters and one sorts at a time; / filters the picked
;;; column as you type.

(defparameter *headers* '("Path" "Branch" "PR" "Age"))
(defparameter *path-max-width* 70)
(defparameter *spinner* "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏")

(defparameter *help*
  '("j k ↑ ↓         move"
    "PgUp PgDn       move a page; Home End to the first or last row"
    "space           mark and move down;  U  unmark all"
    "enter           print the worktree and leave, e.g. cd \"$(git-worktree-status -i)\""
    "d               force-remove the marked worktrees, or the one under the cursor,"
    "                with their Bazel output bases"
    "1 2 3 4         cycle the column: pick for filtering ◆, sort ▲, sort ▼, off"
    "                (the PR column also sorts by state, then number: ▲▲ ▼▼)"
    "/               filter the picked column as you type; enter keeps it, esc drops it"
    "r  R            reload;  R also rescans every worktree and re-asks GitHub"
    "q  esc  ^C      leave without choosing (esc clears a filter first)"
    ""
    "Press any key to return."))

(defparameter *column-cycle* '(nil :pick :asc :desc))
(defparameter *pr-column-cycle* '(nil :pick :asc :desc :state-asc :state-desc))
(defparameter *sort-states* '(:asc :desc :state-asc :state-desc))
(defparameter *column-marks*
  '((:pick . " ◆") (:asc . " ▲") (:desc . " ▼") (:state-asc . " ▲▲") (:state-desc . " ▼▼")))

(defstruct (ui (:conc-name ui-))
  terminal directories repos
  (cursor-path nil)
  (top 0)
  (marked '())
  (columns (vector nil nil nil :asc))
  (filter "")
  (editing nil)            ; filter to restore if the prompt is cancelled, while it is open
  (message nil)            ; shown in the status bar until the next key
  (busy nil)               ; status bar text while checking or deleting
  (loading nil)
  (scan-done 0 :type sb-ext:word)
  (scan-total 0)
  (pr-note nil)
  (help nil)
  (chosen nil)
  (closed nil)             ; set when the UI is gone; the loader stops scanning
  (last-screen nil))

(defun ui-worktrees (ui)
  (loop for repo in (ui-repos ui) append (repo-worktrees repo)))

;;; Loading

(defun ui-prefill (ui)
  "Show what the caches hold, however stale, until the loader has checked it."
  (let ((now (unix-now)))
    (dolist (wt (ui-worktrees ui))
      (multiple-value-bind (scan found) (cache-get *worktree-scans* (wt-path wt))
        (when found (apply-scan wt scan now)))))
  (update-pr-cells (ui-repos ui)))

(defun ui-start-load (ui refresh)
  (let ((now (unix-now))
        (repos (ui-repos ui))
        (worktrees (ui-worktrees ui)))
    (setf (ui-loading ui) t
          (ui-scan-done ui) 0
          (ui-scan-total ui) (length worktrees))
    (sb-thread:make-thread
     (lambda ()
       (handler-case
           (let* ((questions (pr-questions repos now refresh))
                  (pr-thread
                    (when questions
                      (setf (ui-pr-note ui)
                            (format nil "asking GitHub about ~D branch(es)" (length questions)))
                      (sb-thread:make-thread
                       (lambda ()
                         (let ((failures (handler-case (ask-github-all questions)
                                           (error (condition)
                                             (list (princ-to-string condition))))))
                           (update-pr-cells repos :unknown "")
                           (setf (ui-pr-note ui) nil)
                           failures))
                       :name "worktree ui pull requests"))))
             (unless questions
               (update-pr-cells repos :unknown ""))
             (pmap (lambda (wt)
                     (unless (ui-closed ui)
                       (apply-scan wt (scan-worktree (wt-path wt) refresh) now))
                     (sb-ext:atomic-incf (ui-scan-done ui)))
                   worktrees :workers *git-workers*)
             (let ((failures (and pr-thread (sb-thread:join-thread pr-thread))))
               (when failures
                 (setf (ui-message ui) (format nil "Pull requests: ~{~A~^; ~}" failures)))))
         (error (condition)
           (setf (ui-message ui) (format nil "Load failed: ~A" condition))))
       (setf (ui-loading ui) nil))
     :name "worktree ui loader")))

(defun ui-reload (ui refresh)
  (if (ui-loading ui)
      (setf (ui-message ui) "Still loading")
      (handler-case
          (progn
            (setf (ui-repos ui) (mapcar #'load-repo (ui-directories ui)))
            (setf (ui-marked ui) (intersection (ui-marked ui)
                                               (mapcar #'wt-path (ui-worktrees ui))
                                               :test #'string=))
            (ui-prefill ui)
            (ui-start-load ui refresh))
        (error (condition)
          (setf (ui-message ui) (format nil "Reload failed: ~A" condition))))))

;;; Columns, filtering and sorting

(defun column-in-state (ui states)
  (position-if (lambda (state) (member state states)) (ui-columns ui)))

(defun cycle-column (ui column)
  "Advance COLUMN to its next state, evicting whichever column held that role."
  (let* ((columns (ui-columns ui))
         (cycle (if (= column 2) *pr-column-cycle* *column-cycle*))
         (state (nth (mod (1+ (position (aref columns column) cycle)) (length cycle)) cycle))
         (picked (column-in-state ui '(:pick))))
    (dotimes (i (length columns))
      (unless (= i column)
        (when (or (and (eq state :pick) (eq (aref columns i) :pick))
                  (and (member state *sort-states*) (member (aref columns i) *sort-states*)))
          (setf (aref columns i) nil))))
    (setf (aref columns column) state)
    ;; The filter belongs to the picked column; when the pick moves, it goes.
    (unless (eql picked (column-in-state ui '(:pick)))
      (setf (ui-filter ui) ""))))

(defun pr-sort-value (cell by-state)
  "PR number in CELL, ranked first by state (closed, merged, draft, open) if BY-STATE."
  (let ((hash (position #\# cell)))
    (when hash
      (let ((number (parse-integer cell :start (1+ hash) :junk-allowed t)))
        (if by-state
            (+ (* 10000000 (case (char cell 0) (#\📝 3) (#\✅ 2) (#\❌ 1) (#\❔ 0) (t 4)))
               number)
            number)))))

(defun sort-value (column state entry)
  "What ENTRY, (WT . CELLS), sorts by in COLUMN; NIL sorts last either way."
  (destructuring-bind (wt . cells) entry
    (case column
      ((0 1) (string-downcase (nth column cells)))
      (2 (pr-sort-value (third cells) (member state '(:state-asc :state-desc))))
      (3 (wt-age wt)))))

(defun ui-view (ui)
  "The (WT . CELLS) entries to show: filtered by the picked column, sorted by
the sorted one, newest commit first when none is."
  (let* ((entries (mapcar (lambda (wt) (cons wt (row-cells wt))) (ui-worktrees ui)))
         (pick (column-in-state ui '(:pick)))
         (needle (string-downcase (ui-filter ui)))
         (entries (if (and pick (plusp (length needle)))
                      (remove-if-not (lambda (entry)
                                       (search needle (string-downcase (nth pick (cdr entry)))))
                                     entries)
                      entries))
         (sorted (column-in-state ui *sort-states*))
         (column (or sorted 3))
         (state (and sorted (aref (ui-columns ui) sorted)))
         (key (lambda (entry) (sort-value column state entry)))
         (known (stable-sort (remove-if-not key entries)
                             (if (< column 2) #'string< #'<) :key key)))
    (append (if (member state '(:desc :state-desc)) (reverse known) known)
            (remove-if key entries))))

;;; Drawing

(defun progress-bar (done total width)
  (let ((filled (if (plusp total) (min width (floor (* width done) total)) width)))
    (concatenate 'string (make-string filled :initial-element #\█)
                 (make-string (- width filled) :initial-element #\░))))

(defun spinner ()
  (char *spinner* (mod (floor (get-internal-real-time) (floor internal-time-units-per-second 12))
                       (length *spinner*))))

(defun ui-cursor (ui view)
  "Index of the cursor in VIEW, keeping it on the same worktree as rows move."
  (or (position (ui-cursor-path ui) view :key (lambda (entry) (wt-path (car entry)))
                                         :test #'equal)
      0))

(defun ui-status (ui view)
  (let* ((pick (column-in-state ui '(:pick)))
         (filter-note (format nil "filter[~A]: ~A" (nth (or pick 0) *headers*) (ui-filter ui))))
    (cond
      ((ui-editing ui) (format nil "~A▏  (enter keep, esc drop)" filter-note))
      ((ui-busy ui) (format nil "~A ~C" (ui-busy ui) (spinner)))
      ((ui-message ui))
      ((ui-loading ui)
       (format nil "Loading ~C [~A] ~D/~D~@[  ·  ~A~]" (spinner)
               (progress-bar (ui-scan-done ui) (ui-scan-total ui) 16)
               (ui-scan-done ui) (ui-scan-total ui) (ui-pr-note ui)))
      (t
       (let* ((paths (mapcar (lambda (entry) (wt-path (car entry))) view))
              (marked (length (ui-marked ui)))
              (hidden (count-if-not (lambda (path) (member path paths :test #'string=))
                                    (ui-marked ui))))
         (with-output-to-string (out)
           (when (plusp (length (ui-filter ui)))
             (format out "~A  ~D/~D shown  " filter-note (length view) (length (ui-worktrees ui))))
           (when (plusp marked)
             (format out "~D marked~:[~; (~D hidden)~]  " marked (plusp hidden) hidden))
           (write-string "j/k move  space mark  enter choose  d delete  1-4 column  / filter  ? help  q quit"
                         out)))))))

(defun ui-draw (ui view)
  (let ((terminal (ui-terminal ui)))
    (multiple-value-bind (rows cols) (terminal-size terminal)
      (let* ((esc (string #\Esc))
             (lines '()))
        (flet ((line (text &optional (attributes ""))
                 (push (format nil "~A~A~A[0m" attributes (fit-to-width text cols) esc) lines)))
          (if (ui-help ui)
              (progn (line "")
                     (dolist (text *help*) (line (concatenate 'string "  " text))))
              (let* ((visible (max 1 (- rows 3)))
                     (cursor (ui-cursor ui view))
                     (headers (loop for header in *headers*
                                    for i from 0
                                    collect (format nil "~D ~A~A" (1+ i) header
                                                    (or (cdr (assoc (aref (ui-columns ui) i)
                                                                    *column-marks*))
                                                        ""))))
                     (widths (loop for header in headers
                                   for i from 0
                                   collect (min (if (= i 0) *path-max-width* most-positive-fixnum)
                                                (reduce #'max view
                                                        :key (lambda (entry)
                                                               (display-width (nth i (cdr entry))))
                                                        :initial-value (display-width header))))))
                (flet ((cells (cells)
                         (format nil "~{~A~^  ~}" (mapcar #'fit-to-width cells widths))))
                  (setf (ui-top ui) (max 0 (min (ui-top ui) cursor)
                                         (- cursor visible -1)))
                  (when (> (ui-top ui) cursor) (setf (ui-top ui) cursor))
                  (line (concatenate 'string "    " (cells headers)))
                  (line (concatenate 'string "    "
                                     (cells (mapcar (lambda (w) (make-string w :initial-element #\-))
                                                    widths))))
                  (if (null view)
                      (line "    (no worktree matches the filter)")
                      (loop for index from (ui-top ui) below (min (length view) (+ (ui-top ui) visible))
                            for (wt . row) = (nth index view)
                            for marked = (member (wt-path wt) (ui-marked ui) :test #'string=)
                            do (line (format nil "~:[ ~;▶~]~:[ ~;*~]  ~A" (= index cursor) marked
                                             (cells row))
                                     (concatenate 'string
                                                  (if (= index cursor) (format nil "~A[7m" esc) "")
                                                  (if marked (format nil "~A[1m" esc) ""))))))))
          (let ((body (nreverse lines)))
            (with-output-to-string (out)
              (loop for text in body
                    for row from 1 below rows
                    do (format out "~A[~D;1H~A" esc row text))
              (loop for row from (1+ (length body)) below rows
                    do (format out "~A[~D;1H~A[K" esc row esc))
              (format out "~A[~D;1H~A[7m~A~A[0m" esc rows esc
                      (fit-to-width (ui-status ui view) cols) esc))))))))

(defun ui-render (ui view)
  (let ((screen (ui-draw ui view)))
    (unless (equal screen (ui-last-screen ui))
      (setf (ui-last-screen ui) screen)
      (terminal-write (ui-terminal ui) screen)
      (terminal-flush (ui-terminal ui)))))

;;; Deleting

(defun list-directory (directory)
  "Names of the entries in native DIRECTORY, or NIL if it cannot be read."
  (let ((dir (handler-case (sb-posix:opendir directory)
               (sb-posix:syscall-error () nil))))
    (when dir
      (unwind-protect
           (loop for entry = (sb-posix:readdir dir)
                 until (sb-alien:null-alien entry)
                 for name = (sb-posix:dirent-name entry)
                 unless (member name '("." "..") :test #'string=)
                   collect name)
        (sb-posix:closedir dir)))))

(defun bazel-output-bases (path)
  "Output bases Bazel made for the worktree at PATH, on the host or in a
devcontainer, which mounts it at /workspaces/<basename>."
  (let* ((owners (list path
                       (or (ignore-errors
                            (string-right-trim "/" (sb-ext:native-namestring
                                                    (truename (uiop:parse-native-namestring
                                                               path :ensure-directory t)))))
                           path)
                       (concatenate 'string "/workspaces/" (subseq path (1+ (or (position #\/ path :from-end t) -1))))))
         (cache (join (string-right-trim "/" (uiop:native-namestring (user-homedir-pathname)))
                      ".cache"))
         (roots (append (loop for name in (list-directory "/private/var/tmp")
                              when (uiop:string-prefix-p "_bazel_" name)
                                collect (join "/private/var/tmp" name))
                        (loop for name in (list-directory cache)
                              when (uiop:string-prefix-p "bazel" name)
                                append (mapcar (lambda (child) (join (join cache name) child))
                                               (list-directory (join cache name))))))
         ;; A root is an output base itself (--output_base) or holds them
         ;; (--output_user_root, recognisable by its install/ dir).
         (markers (loop for root in roots
                        collect (join root "DO_NOT_BUILD_HERE")
                        when (directory-p (join root "install"))
                          append (mapcar (lambda (name) (join (join root name) "DO_NOT_BUILD_HERE"))
                                         (list-directory root)))))
    (loop for marker in markers
          when (member (read-trimmed marker) owners :test #'equal)
            collect (subseq marker 0 (position #\/ marker :from-end t)))))

(defun purge-output-base (base)
  "Stop BASE's Bazel server and delete BASE in the background. The rename
frees the path at once; deleting a large read-only tree takes minutes."
  (let ((pid (parse-integer (or (read-trimmed (join base "server/server.pid.txt")) "")
                            :junk-allowed t)))
    ;; A devcontainer server's pid names some unrelated host process.
    (when (and pid (plusp pid)
               (search (concatenate 'string "--output_base=" base)
                       (capture-output (list "ps" "-o" "command=" "-p" (princ-to-string pid))
                                       :directory "/")))
      (ignore-errors (sb-posix:kill pid sb-posix:sigterm))))
  (let ((trash (format nil "~A.deleting-~D" base (sb-posix:getpid))))
    (sb-posix:rename base trash)
    (run-in-background "bazel output base removal"
                       (lambda ()
                         (capture-output (list "sh" "-c" "chmod -R u+w \"$1\"; rm -rf \"$1\"" "sh" trash)
                                         :directory "/")))))

(defun remove-worktree (wt)
  "Force-remove WT and purge its Bazel output bases. Returns NIL or why it
failed, and how many output bases went."
  (let ((bases (bazel-output-bases (wt-path wt))))
    (multiple-value-bind (out ok err)
        (capture-output (list "git" "worktree" "remove" "--force" (wt-path wt))
                        :directory (wt-repo wt))
      (if ok
          (values nil (count-if (lambda (base) (ignore-errors (purge-output-base base) t)) bases))
          (values (trim (if (plusp (length err)) err out)) 0)))))

(defun ui-wait-for (ui view function busy)
  "Call FUNCTION in a thread, keeping the screen alive with BUSY in the status bar."
  (setf (ui-busy ui) busy)
  (let ((thread (sb-thread:make-thread function :name "worktree ui work")))
    (loop while (sb-thread:thread-alive-p thread)
          do (ui-render ui view)
             (read-key (ui-terminal ui) 80))
    (setf (ui-busy ui) nil)
    (sb-thread:join-thread thread :default nil)))

(defun main-worktree-p (wt)
  "Whether WT is its repository's main worktree, which git never removes."
  (directory-p (join (wt-path wt) ".git")))

(defun ui-delete (ui view)
  (let* ((marked-visible (remove-if-not (lambda (wt) (member (wt-path wt) (ui-marked ui)
                                                            :test #'string=))
                                        (mapcar #'car view)))
         (marked (remove-if #'main-worktree-p marked-visible))
         (targets (or marked
                      (unless (ui-marked ui)
                        (let ((entry (nth (ui-cursor ui view) view)))
                          (and entry (not (main-worktree-p (car entry))) (list (car entry))))))))
    (cond
      ((and (null targets) (ui-marked ui) (null marked-visible))
       ;; Falling back to the cursor row would delete something never marked,
       ;; just because the filter hid the marks.
       (setf (ui-message ui) (format nil "~D marked worktree(s) hidden by the filter - clear it or unmark"
                                     (length (ui-marked ui)))))
      ((null targets)
       (setf (ui-message ui) "Nothing to delete (the main worktree cannot be removed)"))
      (t
       (let ((dirty (count t (ui-wait-for
                              ui view
                              (lambda ()
                                (pmap (lambda (wt)
                                        (plusp (length (trim (capture-output
                                                              (list "git" "status" "--porcelain")
                                                              :directory (wt-path wt))))))
                                      targets))
                              (format nil "Checking ~D worktree(s)..." (length targets))))))
         (setf (ui-message ui)
               (format nil "Force-remove ~D worktree(s)~:[~*~; (~D dirty)~]~@[ and keep ~D marked but filtered out~]? (y/n)"
                       (length targets) (plusp dirty) dirty
                       (let ((hidden (count-if-not
                                      (lambda (path)
                                        (find path view :key (lambda (entry) (wt-path (car entry)))
                                                        :test #'string=))
                                      (ui-marked ui))))
                         (and marked (plusp hidden) hidden))))
         (ui-render ui view)
         (let ((key (loop for key = (read-key (ui-terminal ui) 500) when key return key)))
           (setf (ui-message ui) nil)
           (when (member key '(#\y #\Y))
             (let ((removed '()) (failures '()) (purged 0) (done 0))
               (ui-wait-for
                ui view
                (lambda ()
                  (dolist (wt targets)
                    (setf (ui-busy ui)
                          (format nil "Removing [~A] ~D/~D ~A"
                                  (progress-bar done (length targets) 16) (1+ done)
                                  (length targets) (abbreviate-home (wt-path wt))))
                    (multiple-value-bind (failure bases) (remove-worktree wt)
                      (incf purged bases)
                      (if failure (push failure failures) (push wt removed)))
                    (incf done)))
                "Removing")
               (dolist (repo (ui-repos ui))
                 (setf (repo-worktrees repo)
                       (remove-if (lambda (wt) (member wt removed)) (repo-worktrees repo))))
               (setf (ui-marked ui) (set-difference (ui-marked ui) (mapcar #'wt-path removed)
                                                    :test #'string=)
                     (ui-message ui)
                     (format nil "Removed ~D worktree(s)~:[~*~;, ~D Bazel output base(s)~]~@[; ~D failed: ~{~A~^; ~}~]"
                             (length removed) (plusp purged) purged
                             (and failures (length failures)) (reverse failures)))))))))))

;;; Keys

(defun ui-filter-key (ui key)
  (cond ((eq key :enter) (setf (ui-editing ui) nil))
        ((eq key :escape) (setf (ui-filter ui) (ui-editing ui) (ui-editing ui) nil))
        ((eq key :backspace)
         (let ((filter (ui-filter ui)))
           (setf (ui-filter ui) (subseq filter 0 (max 0 (1- (length filter)))))))
        ((eql key (code-char 21)) (setf (ui-filter ui) "")) ; ^U
        ((and (characterp key) (graphic-char-p key))
         (setf (ui-filter ui) (concatenate 'string (ui-filter ui) (string key))))))

(defun ui-key (ui key view)
  "Act on KEY; return true to leave."
  (let* ((cursor (ui-cursor ui view))
         (page (max 1 (- (nth-value 0 (terminal-size (ui-terminal ui))) 4))))
    (flet ((move-to (index)
             (when view
               (setf (ui-cursor-path ui)
                     (wt-path (car (nth (max 0 (min (1- (length view)) index)) view)))))))
      (setf (ui-message ui) nil)
      (case key
        ((:up #\k #.(code-char 16)) (move-to (1- cursor)))
        ((:down #\j #.(code-char 14)) (move-to (1+ cursor)))
        (:page-up (move-to (- cursor page)))
        (:page-down (move-to (+ cursor page)))
        (:home (move-to 0))
        (:end (move-to (1- (length view))))
        (#\Space
         (when view
           (let ((path (wt-path (car (nth cursor view)))))
             (setf (ui-marked ui) (if (member path (ui-marked ui) :test #'string=)
                                      (remove path (ui-marked ui) :test #'string=)
                                      (cons path (ui-marked ui))))
             (move-to (1+ cursor)))))
        (#\U (setf (ui-marked ui) '()))
        (:enter
         (when view
           (setf (ui-chosen ui) (wt-path (car (nth cursor view))))
           (return-from ui-key t)))
        (#\d (ui-delete ui view))
        ((#\1 #\2 #\3 #\4) (cycle-column ui (- (char-code key) (char-code #\1))))
        (#\/
         (unless (column-in-state ui '(:pick))
           (cycle-column ui 0))
         (setf (ui-editing ui) (ui-filter ui)))
        (#\r (ui-reload ui nil))
        (#\R (ui-reload ui t))
        (#\? (setf (ui-help ui) t))
        (:escape
         (if (plusp (length (ui-filter ui)))
             (setf (ui-filter ui) "")
             (return-from ui-key t)))
        ((#\q #.(code-char 3)) (return-from ui-key t)))
      nil)))

(defun ui-loop (ui)
  (loop
    (let ((view (ui-view ui)))
      (unless (ui-cursor-path ui)
        (setf (ui-cursor-path ui) (and view (wt-path (car (first view))))))
      (ui-render ui view)
      (let ((key (read-key (ui-terminal ui) (if (ui-loading ui) 80 500))))
        (cond ((null key))
              ((ui-help ui) (setf (ui-help ui) nil))
              ((ui-editing ui) (ui-filter-key ui key))
              ((ui-key ui key view) (return)))))))

(defun worktree-ui (directories refresh)
  "Browse the worktrees of DIRECTORIES on the client's terminal; print the
chosen one, or exit with status 130 if none was."
  (let ((ui (make-ui :directories directories
                     :repos (mapcar #'load-repo directories))))
    (ui-prefill ui)
    (unwind-protect
         (with-terminal (terminal)
           (setf (ui-terminal ui) terminal)
           (ui-start-load ui refresh)
           (ui-loop ui))
      (setf (ui-closed ui) t))
    (if (ui-chosen ui)
        (write-line (ui-chosen ui))
        (exit-with 130))))
