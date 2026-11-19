(in-package #:tool-server-user)

;;; git-worktree-status -i: browse worktrees on the client's terminal.
;;;
;;; Rows show at once with whatever the caches hold, and a loader thread fills
;;; in what has changed. Each column has a number key (1-4) that walks it
;;; through: picked for filtering (◆), sorted ascending (▲), descending (▼),
;;; off. The PR column takes two more steps that sort by state, then number
;;; (▲▲ ▼▼). One column filters and one sorts at a time; / filters the picked
;;; column as you type. Deletes run in a worker thread, so the list stays live
;;; while they do.

(defparameter *headers* '("Path" "Branch" "PR" "Age"))
(defparameter *path-max-width* 70)
(defparameter *spinner* "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏")

(defparameter *help*
  '("j k ↑ ↓         move"
    "PgUp PgDn       move a page; Home End to the first or last row"
    "^F ^B  ^D ^U    scroll a page, or half a page, down or up"
    "space           mark and move down;  U  unmark all"
    "enter           print the worktree and leave, e.g. cd \"$(git-worktree-status -i)\""
    "d               force-remove the marked worktrees, or the one under the cursor,"
    "                with their Bazel output bases, in the background (· queued, ! failed)"
    "1 2 3 4         cycle the column: pick for filtering ◆, sort ▲, sort ▼, off"
    "                (the PR column also sorts by state, then number: ▲▲ ▼▼)"
    "/               filter the picked column as you type; enter keeps it, esc drops it"
    "r  R            reload;  R also rescans every worktree and re-asks GitHub"
    "q  esc  ^C      leave without choosing (esc clears a filter first)"
    "                Leaving waits for running deletes; q again leaves them to finish alone"
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
  (deletion (make-deletion))
  (quitting nil)           ; left, but waiting for deletes to finish
  (last-screen nil))

(defstruct (deletion (:conc-name del-))
  "Worktrees queued for removal, worked off one at a time by WORKER. The UI
thread only reads this, under LOCK, and draws it."
  (lock (sb-thread:make-mutex :name "worktree ui deletes"))
  (queue '())
  (states (make-hash-table :test 'equal)) ; path -> :queued, :removing or why it failed
  (worker nil)
  (current nil)
  (done 0) (total 0) (removed 0) (purged 0) (failures '())
  (report nil))                           ; summary of a finished run, until shown

(defmacro with-deletion ((var ui) &body body)
  `(let ((,var (ui-deletion ,ui)))
     (sb-thread:with-mutex ((del-lock ,var)) ,@body)))

(defun delete-state (ui path)
  "NIL, :QUEUED, :REMOVING or why removing the worktree at PATH failed."
  (with-deletion (d ui) (gethash path (del-states d))))

(defun pending-p (ui path)
  (member (delete-state ui path) '(:queued :removing)))

(defun ui-deleting (ui)
  "How many deletes are queued or running."
  (with-deletion (d ui) (- (del-total d) (del-done d))))

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
  ;; The delete worker edits the repos it was given; a reload would replace them.
  (if (or (ui-loading ui) (plusp (ui-deleting ui)))
      (setf (ui-message ui) (if (ui-loading ui) "Still loading" "Still deleting"))
      (handler-case
          (progn
            (with-deletion (d ui) (clrhash (del-states d)))
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
         (filter-note (format nil "filter[~A]: ~A" (nth (or pick 0) *headers*) (ui-filter ui)))
         (entry (nth (ui-cursor ui view) view))
         (failure (and entry (delete-state ui (wt-path (car entry))))))
    (cond
      ((ui-editing ui) (format nil "~A▏  (enter keep, esc drop)" filter-note))
      ((ui-busy ui) (format nil "~A ~C" (ui-busy ui) (spinner)))
      ((ui-message ui))
      ((ui-quitting ui)
       (format nil "Waiting for ~D delete(s) ~C  q leaves them running in the background"
               (ui-deleting ui) (spinner)))
      ((stringp failure) (format nil "Removing failed: ~A" failure))
      ((plusp (ui-deleting ui))
       (with-deletion (d ui)
         (format nil "Removing ~C [~A] ~D/~D ~@[~A~]" (spinner)
                 (progress-bar (del-done d) (del-total d) 16)
                 (min (del-total d) (1+ (del-done d))) (del-total d)
                 (and (del-current d) (abbreviate-home (del-current d))))))
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

(defun list-rows (terminal)
  "Rows left for worktrees below the header and rule, above the status bar."
  (max 1 (- (terminal-size terminal) 3)))

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
              (let* ((visible (list-rows terminal))
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
                            for state = (delete-state ui (wt-path wt))
                            do (line (format nil "~:[ ~;▶~]~A  ~A" (= index cursor)
                                             (case state
                                               ((nil) (if marked "*" " "))
                                               (:queued "·")
                                               (:removing (spinner))
                                               (t "!"))
                                             (cells row))
                                     (concatenate 'string
                                                  (if (= index cursor) (format nil "~A[7m" esc) "")
                                                  (if marked (format nil "~A[1m" esc) "")
                                                  (case state
                                                    ((nil) "")
                                                    ((:queued :removing) (format nil "~A[2m" esc))
                                                    (t (format nil "~A[31m" esc))))))))))
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

(defun ui-wait-for (ui function busy)
  "Call FUNCTION in a thread, keeping the screen alive with BUSY in the status bar."
  (setf (ui-busy ui) busy)
  (let ((thread (sb-thread:make-thread function :name "worktree ui work")))
    (loop while (sb-thread:thread-alive-p thread)
          do (ui-render ui (ui-view ui))
             (read-key (ui-terminal ui) 80))
    (setf (ui-busy ui) nil)
    (sb-thread:join-thread thread :default nil)))

(defun main-worktree-p (wt)
  "Whether WT is its repository's main worktree, which git never removes."
  (directory-p (join (wt-path wt) ".git")))

(defun delete-worker (ui)
  "Remove queued worktrees until none are left. Only state changes here; the
UI thread draws it."
  (loop
    (let ((wt (with-deletion (d ui)
                (let ((wt (pop (del-queue d))))
                  (if wt
                      (setf (gethash (wt-path wt) (del-states d)) :removing
                            (del-current d) (wt-path wt))
                      (let ((report (format nil "Removed ~D worktree(s)~:[~*~;, ~D Bazel output base(s)~]~@[; ~D failed: ~{~A~^; ~}~]"
                                            (del-removed d) (plusp (del-purged d)) (del-purged d)
                                            (and (del-failures d) (length (del-failures d)))
                                            (reverse (del-failures d)))))
                        (when (ui-closed ui)
                          (log-message "git-worktree-status: ~A" report))
                        (setf (del-report d) report
                              (del-worker d) nil (del-current d) nil (del-failures d) '()
                              (del-done d) 0 (del-total d) 0 (del-removed d) 0 (del-purged d) 0)))
                  wt))))
      (unless wt (return))
      (multiple-value-bind (failure bases)
          (handler-case (remove-worktree wt)
            (error (condition) (values (princ-to-string condition) 0)))
        (let ((path (wt-path wt)))
          (with-deletion (d ui)
            (incf (del-done d))
            (incf (del-purged d) bases)
            (cond (failure
                   (let ((failure (substitute #\Space #\Newline failure)))
                     (setf (gethash path (del-states d)) failure)
                     (push failure (del-failures d))))
                  (t
                   (incf (del-removed d))
                   (remhash path (del-states d))
                   (cache-remove *worktree-scans* path)
                   (dolist (repo (ui-repos ui))
                     (setf (repo-worktrees repo)
                           (remove path (repo-worktrees repo) :key #'wt-path :test #'string=)))))))))))

(defun ui-queue-deletes (ui targets)
  (with-deletion (d ui)
    (dolist (wt targets)
      (setf (gethash (wt-path wt) (del-states d)) :queued))
    (setf (del-queue d) (append (del-queue d) targets))
    (incf (del-total d) (length targets))
    (unless (del-worker d)
      (setf (del-worker d) (sb-thread:make-thread #'delete-worker :name "worktree ui deletes"
                                                                  :arguments (list ui)))))
  ;; Their row shows the delete now; a pending row cannot be marked.
  (setf (ui-marked ui) (set-difference (ui-marked ui) (mapcar #'wt-path targets)
                                       :test #'string=)))

(defun ui-take-report (ui)
  (let ((report (with-deletion (d ui) (shiftf (del-report d) nil))))
    (when report
      (setf (ui-message ui) report))))

(defun ui-delete (ui view)
  (let* ((deletable (lambda (wt) (not (or (main-worktree-p wt) (pending-p ui (wt-path wt))))))
         (marked-visible (remove-if-not (lambda (wt) (member (wt-path wt) (ui-marked ui)
                                                            :test #'string=))
                                        (mapcar #'car view)))
         (marked (remove-if-not deletable marked-visible))
         (entry (nth (ui-cursor ui view) view))
         (targets (or marked
                      (unless (ui-marked ui)
                        (and entry (funcall deletable (car entry)) (list (car entry)))))))
    (cond
      ((and (null targets) (ui-marked ui) (null marked-visible))
       ;; Falling back to the cursor row would delete something never marked,
       ;; just because the filter hid the marks.
       (setf (ui-message ui) (format nil "~D marked worktree(s) hidden by the filter - clear it or unmark"
                                     (length (ui-marked ui)))))
      ((null targets)
       (setf (ui-message ui)
             (if (and entry (null (ui-marked ui)) (pending-p ui (wt-path (car entry))))
                 "Already being removed"
                 "Nothing to delete (the main worktree cannot be removed)")))
      (t
       (let ((dirty (count t (ui-wait-for
                              ui
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
         ;; Keep drawing: running deletes finish while the question is open.
         (let ((key (loop (ui-render ui (ui-view ui))
                          (let ((key (read-key (ui-terminal ui) 80)))
                            (when key (return key))))))
           (setf (ui-message ui) nil)
           (when (member key '(#\y #\Y))
             (ui-queue-deletes ui targets))))))))

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
         (page (max 1 (- (nth-value 0 (terminal-size (ui-terminal ui))) 4)))
         (visible (list-rows (ui-terminal ui))))
    (flet ((move-to (index)
             (when view
               (setf (ui-cursor-path ui)
                     (wt-path (car (nth (max 0 (min (1- (length view)) index)) view))))))
           (scroll (amount)
             "Move the list by AMOUNT rows, as vim does, so the cursor keeps its
place on screen until an end stops the list; return where the cursor goes."
             (setf (ui-top ui) (max 0 (min (+ (ui-top ui) amount) (- (length view) visible))))
             (+ cursor amount)))
      (setf (ui-message ui) nil)
      (case key
        ((:up #\k #.(code-char 16)) (move-to (1- cursor)))
        ((:down #\j #.(code-char 14)) (move-to (1+ cursor)))
        (:page-up (move-to (- cursor page)))
        (:page-down (move-to (+ cursor page)))
        (#.(code-char 6) (move-to (scroll visible))) ; ^F
        (#.(code-char 2) (move-to (scroll (- visible)))) ; ^B
        (#.(code-char 4) (move-to (scroll (ceiling visible 2)))) ; ^D
        (#.(code-char 21) (move-to (scroll (- (ceiling visible 2))))) ; ^U
        (:home (move-to 0))
        (:end (move-to (1- (length view))))
        (#\Space
         (when view
           (let ((path (wt-path (car (nth cursor view)))))
             (unless (pending-p ui path)
               (setf (ui-marked ui) (if (member path (ui-marked ui) :test #'string=)
                                        (remove path (ui-marked ui) :test #'string=)
                                        (cons path (ui-marked ui)))))
             (move-to (1+ cursor)))))
        (#\U (setf (ui-marked ui) '()))
        (:enter
         (when view
           (let ((path (wt-path (car (nth cursor view)))))
             (if (pending-p ui path)
                 (setf (ui-message ui) "That worktree is being removed")
                 (progn (setf (ui-chosen ui) path)
                        (return-from ui-key t))))))
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
      (ui-take-report ui)
      (when (and (ui-quitting ui) (zerop (ui-deleting ui)))
        (return))
      (ui-render ui view)
      ;; The timeout doubles as the wakeup that shows a delete finishing.
      (let ((key (read-key (ui-terminal ui)
                           (if (or (ui-loading ui) (plusp (ui-deleting ui))) 80 500))))
        (cond ((null key))
              ((ui-quitting ui)
               (when (member key '(#\q :escape #.(code-char 3)))
                 (return)))
              ((ui-help ui) (setf (ui-help ui) nil))
              ((ui-editing ui) (ui-filter-key ui key))
              ((ui-key ui key view)
               ;; Deletes run on in the server either way; waiting shows how they end.
               (if (plusp (ui-deleting ui))
                   (setf (ui-quitting ui) t)
                   (return))))))))

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
