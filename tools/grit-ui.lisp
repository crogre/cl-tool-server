(in-package #:grit)

;;; The grit terminal UI: a Magit-style status page with vim keys, plus log,
;;; commit detail, worktree, help and prompt panels. One thread owns the
;;; terminal; slow git calls run in a helper thread while the footer spins.
;;;
;;; The cursor walks a flat list of targets (section headings, files, hunks,
;;; changed lines, commits) that BUILD derives from the state on every use,
;;; so it never goes stale.

(declaim (ftype function autopilot))

(defparameter *chord-timeout* 1.5 "Seconds before a half-typed chord is dropped.")
(defparameter *log-limit* 1000)
(defparameter *spinner* "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏")
(defparameter *sections* '(:conflicted :untracked :unstaged :staged :recent))
(defparameter *chord-modes* '(:commit-verb :rebase-verb :log-verb :branch-verb :fetch-verb :pull-verb))

(defun section-label (section)
  (ecase section
    (:conflicted "Unmerged paths")
    (:untracked "Untracked files")
    (:unstaged "Unstaged changes")
    (:staged "Staged changes")
    (:recent "Recent commits")))

(defstruct (app (:conc-name app-))
  repo terminal
  (status (make-status))
  loaded
  (cursor 0)
  (expanded (make-hash-table :test 'equal))  ; (section path) -> t
  (patches (make-hash-table :test 'equal))   ; (section path) -> patch, or :none
  (collapsed '())                            ; sections folded by Tab
  (mode '(:normal))                          ; (KIND . DATA)
  chord-started
  (worktrees '())
  (worktree-cursor 0)
  progress                                   ; (description start-time ai)
  log-view
  detail                                     ; (lines . scroll)
  (help-scroll 0)
  (body-height 1)
  message                                    ; (text . :info/:error)
  quit
  last-screen)

(defun mode (app) (first (app-mode app)))
(defun set-mode (app &rest mode) (setf (app-mode app) mode))

(defun info (app control &rest args)
  (setf (app-message app) (cons (apply #'format nil control args) :info)))

(defun err (app control &rest args)
  (setf (app-message app) (cons (apply #'format nil control args) :error)))

;;; Targets: (:header SECTION) (:file SECTION PATH KIND) (:hunk SECTION PATH H)
;;; (:line SECTION PATH H L) (:commit OID)

(defun target-section (target)
  (if (eq (first target) :commit) :recent (second target)))

(defun target-path (target)
  (and (member (first target) '(:file :hunk :line)) (third target)))

;;; Building the status page

(defstruct build
  (lines (make-array 0 :adjustable t :fill-pointer 0))
  (targets (make-array 0 :adjustable t :fill-pointer 0))
  (target-lines (make-array 0 :adjustable t :fill-pointer 0))
  (hunk-first (make-hash-table :test 'equal)))

(defun files-in (app section)
  "(PATH KIND) of SECTION's files; KIND is NIL for untracked ones."
  (let ((status (app-status app)))
    (flet ((changes (list) (mapcar (lambda (c) (list (change-path c) (change-kind c))) list)))
      (ecase section
        (:conflicted (changes (status-conflicted status)))
        (:untracked (mapcar (lambda (path) (list path nil)) (status-untracked status)))
        (:unstaged (changes (status-unstaged status)))
        (:staged (changes (status-staged status)))))))

(defun section-header-spans (section count expanded)
  (list (cons (if expanded "v " "> ") "1;33")
        (cons (format nil "~A (~D)" (section-label section) count) "1;33")))

(defun file-spans (path kind expanded)
  (list (cons (if expanded "v " "> ") nil)
        (cons (format nil "~10A" (kind-label kind))
              (case kind ((nil :added) "32") (:deleted "31") (:renamed "35")))
        (cons " " nil)
        (cons path nil)))

(defun commit-row-spans (entry)
  (append (list (cons "  " nil) (cons (entry-short entry) "1;90") (cons " " nil))
          (refs-spans (entry-refs entry))
          (list (cons (entry-summary entry) nil))))

(defun hunk-header-spans (hunk)
  (list (cons (concatenate 'string "    " (display-text (string-right-trim " " (hunk-header hunk))))
              "36")))

(defun diff-line-spans (line)
  (list (cons (format nil "    ~C~A"
                      (ecase (diff-line-kind line) (:context #\Space) (:add #\+) (:del #\-))
                      (diff-line-text line))
              (case (diff-line-kind line) (:add "32") (:del "31")))))

(defun faded (text) (list (cons text "90")))

(defun build (app)
  (let ((b (make-build))
        (status (app-status app)))
    (flet ((line (spans) (vector-push-extend spans (build-lines b)))
           (target (target)
             (vector-push-extend target (build-targets b))
             (vector-push-extend (length (build-lines b)) (build-target-lines b))))
      (dolist (section *sections*)
        (let ((collapsed (member section (app-collapsed app))))
          (if (eq section :recent)
              (let ((commits (status-recent-commits status)))
                (when commits
                  (target (list :header section))
                  (line (section-header-spans section (length commits) (not collapsed)))
                  (unless collapsed
                    (dolist (entry commits)
                      (target (list :commit (entry-oid entry)))
                      (line (commit-row-spans entry))))
                  (line '())))
              (let ((files (files-in app section)))
                (when files
                  (target (list :header section))
                  (line (section-header-spans section (length files) (not collapsed)))
                  (unless collapsed
                    (loop for (path kind) in files
                          for key = (list section path)
                          for expanded = (gethash key (app-expanded app))
                          do (target (list :file section path kind))
                             (line (file-spans path kind expanded))
                             (when expanded
                               (let ((patch (gethash key (app-patches app))))
                                 (cond
                                   ((eq patch :none) (line (faded "    (binary or no diff)")))
                                   ((null patch) (line (faded "    (loading...)")))
                                   ((file-patch-binary patch) (line (faded "    (binary)")))
                                   ((null (file-patch-hunks patch))
                                    (line (faded "    (no textual changes)")))
                                   (t
                                    (loop for hunk in (file-patch-hunks patch)
                                          for h from 0
                                          do (target (list :hunk section path h))
                                             (line (hunk-header-spans hunk))
                                             (setf (gethash (list section path h) (build-hunk-first b))
                                                   (length (build-lines b)))
                                             (loop for diff-line across (hunk-lines hunk)
                                                   for l from 0
                                                   do (unless (eq (diff-line-kind diff-line) :context)
                                                        (target (list :line section path h l)))
                                                      (line (diff-line-spans diff-line))))))))))
                  (line '()))))))
      (when (zerop (length (build-lines b)))
        (line (list (cons "  Nothing to commit, working tree clean." nil)))))
    b))

(defun target-at-cursor (app &optional (build (build app)))
  (let ((targets (build-targets build)))
    (and (< (app-cursor app) (length targets)) (aref targets (app-cursor app)))))

;;; Refreshing

(defun all-file-keys (app)
  (loop for section in '(:conflicted :untracked :unstaged :staged)
        append (mapcar (lambda (file) (list section (first file))) (files-in app section))))

(defun refresh (app)
  "Reload the status; cached patches go, expansions stay for files still there."
  (setf (app-status app) (load-status (app-repo app))
        (app-loaded app) t)
  (clrhash (app-patches app))
  (let ((valid (all-file-keys app)))
    (loop for key being the hash-keys of (app-expanded app)
          unless (member key valid :test #'equal)
            do (remhash key (app-expanded app)))
    ;; Patches of what stays expanded, so diffs remain open after a stage.
    (loop for key being the hash-keys of (app-expanded app)
          do (load-patch app (first key) (second key))))
  (setf (app-cursor app) (max 0 (min (app-cursor app)
                                     (1- (length (build-targets (build app))))))))

(defun refresh-after (app control &rest args)
  (handler-case (progn (refresh app) (apply #'info app control args))
    (error (condition) (err app "refresh: ~A" condition))))

(defun load-patch (app section path)
  (let ((key (list section path)))
    (setf (gethash key (app-patches app))
          (or (file-patch (app-repo app) (if (eq section :staged) :staged :unstaged) path
                          :untracked (eq section :untracked)
                          :conflicted (eq section :conflicted))
              :none))))

;;; Doing things

(defun call-busy (app description function)
  "Call FUNCTION in a helper thread, keeping the screen alive with a spinner
and DESCRIPTION in the footer. Returns its values or re-signals its error."
  (let* ((result nil) (failure nil)
         (thread (make-command-thread
                  (lambda ()
                    (handler-case (setf result (multiple-value-list (funcall function)))
                      (error (condition) (setf failure condition))))
                  :name "grit work")))
    (setf (app-progress app) (list description (get-internal-real-time) nil))
    (unwind-protect
         (loop while (sb-thread:thread-alive-p thread)
               do (render app)
                  (read-key (app-terminal app) 80))
      (setf (app-progress app) nil))
    (sb-thread:join-thread thread :default nil)
    (when failure (error failure))
    (values-list result)))

(defmacro busy ((app control &rest args) &body body)
  `(call-busy ,app (format nil ,control ,@args) (lambda () ,@body)))

(defmacro attempt ((app what) &body body)
  "Run BODY, turning a failure into an error message \"WHAT: why\"."
  `(handler-case (progn ,@body)
     (error (condition) (err ,app "~A: ~A" ,what condition))))

(defun stage-target (app target)
  (let ((repo (app-repo app))
        (section (target-section target))
        (path (target-path target)))
    (cond
      ;; grit's original cannot stage a resolved conflict except by its AI
      ;; autopilot; staging the file is how to mark one resolved here.
      ((and (eq section :conflicted) (eq (first target) :file))
       (attempt (app "stage") (stage-file repo path) (refresh-after app "staged ~A" path)))
      ((member section '(:untracked :unstaged))
       (attempt (app "stage")
         (ecase (first target)
           (:header (return-from stage-target))
           (:file (stage-file repo path) (refresh-after app "staged"))
           (:hunk (stage-hunk repo path (fourth target) :untracked (eq section :untracked))
            (refresh-after app "staged hunk"))
           (:line (stage-lines repo path (fourth target) (list (fifth target))
                               :untracked (eq section :untracked))
            (refresh-after app "staged line"))))))))

(defun unstage-target (app target)
  (let ((repo (app-repo app))
        (path (target-path target)))
    (when (eq (target-section target) :staged)
      (attempt (app "unstage")
        (ecase (first target)
          (:header)
          (:file (unstage-file repo path) (refresh-after app "unstaged"))
          (:hunk (unstage-hunk repo path (fourth target)) (refresh-after app "unstaged hunk"))
          (:line (unstage-lines repo path (fourth target) (list (fifth target)))
           (refresh-after app "unstaged line")))))))

(defun discard-target (app target)
  (let ((repo (app-repo app))
        (path (target-path target)))
    (attempt (app "discard")
      (case (first target)
        (:file (discard-file repo path))
        ;; Untracked files go whole.
        ((:hunk :line)
         (cond ((eq (target-section target) :untracked) (discard-file repo path))
               ((eq (first target) :hunk) (discard-hunk repo path (fourth target)))
               (t (discard-lines repo path (fourth target) (list (fifth target))))))
        (t (return-from discard-target)))
      (refresh-after app "discarded"))))

(defun toggle-expand (app section path)
  (let ((key (list section path)))
    (if (gethash key (app-expanded app))
        (remhash key (app-expanded app))
        (attempt (app "diff")
          (unless (gethash key (app-patches app))
            (load-patch app section path))
          (setf (gethash key (app-expanded app)) t)))))

(defun move-cursor-to-target (app predicate)
  (let ((index (position-if predicate (build-targets (build app)))))
    (when index (setf (app-cursor app) index))))

(defun toggle-expand-for (app target)
  (case (first target)
    (:header
     (let ((section (second target)))
       (setf (app-collapsed app)
             (if (member section (app-collapsed app))
                 (remove section (app-collapsed app))
                 (cons section (app-collapsed app))))
       (move-cursor-to-target app (lambda (other) (equal other target)))))
    (:file
     (toggle-expand app (second target) (third target))
     (move-cursor-to-target app (lambda (other) (and (eq (first other) :file)
                                                     (eq (second other) (second target))
                                                     (equal (third other) (third target))))))))

(defun move-cursor (app delta)
  (let ((count (length (build-targets (build app)))))
    (when (plusp count)
      (setf (app-cursor app) (max 0 (min (1- count) (+ (app-cursor app) delta)))))))

;;; The editor

(defun editor-environment ()
  (environment (if (base-getenv "TERM") '() '("TERM=xterm-256color"))))

(defun editor-command (repo)
  "The editor git would use, from the client's environment and git config."
  (let ((out (run (list "git" "var" "GIT_EDITOR") (repo-workdir repo) :plain t)))
    (if (plusp (length (trim out))) (trim out) "vi")))

(defun run-editor (app path)
  "Edit native PATH on the client's terminal; true if the editor exited 0."
  (let ((editor (editor-command (app-repo app))))
    (prog1 (zerop (run-on-terminal (app-terminal app) "/bin/sh"
                                   (list "-c" (format nil "~A \"$@\"" editor) editor path)
                                   :directory (repo-workdir (app-repo app))
                                   :environment (editor-environment)))
      (setf (app-last-screen app) nil))))

(defun edit-and-commit (app amend)
  (let* ((repo (app-repo app))
         (prior (if amend
                    (or (head-commit-message repo)
                        (return-from edit-and-commit (err app "nothing to amend (HEAD is unborn)")))
                    (if (status-staged (app-status app))
                        nil
                        (return-from edit-and-commit
                          (err app "nothing to commit (no staged changes)")))))
         (path (commit-message-path repo)))
    (attempt (app "commit")
      (write-file path (commit-template (app-status app) prior))
      (unless (run-editor app path)
        (return-from edit-and-commit (info app "commit cancelled (editor exited non-zero)")))
      (let ((message (strip-comments (or (read-file path) ""))))
        (if (zerop (length message))
            (info app "aborting commit due to empty message")
            (let ((oid (busy (app (if amend "amending" "committing"))
                         (commit repo message :amend amend))))
              (refresh-after app "~:[committed~;amended to~] ~A" amend oid)))))))

(defun open-file (app path)
  (attempt (app "editor")
    (unless (run-editor app (repo-path (app-repo app) path))
      (info app "editor exited non-zero on ~A" path))
    (refresh app)))

;;; Log and detail panels

(defun open-log (app scope)
  (attempt (app "log")
    (let ((entries (load-log (app-repo app) scope *log-limit*)))
      (if (null entries)
          (info app "log: no commits to show")
          (progn
            (setf (app-log-view app) (make-log-view entries scope))
            (set-mode app :log)
            (info app "log: ~D commit(s) from ~A" (length entries)
                  (ecase scope (:local "local branches") (:all "all branches") (:head "HEAD"))))))))

(defun patch-lines (patches)
  (if (null patches)
      (list (faded "(no file changes)"))
      (append (list (faded (format nil "~D file(s) changed" (length patches))) '())
              (loop for patch in patches
                    append (append (list (list (cons (format nil "--- ~A" (file-patch-path patch))
                                                     "1;36")))
                                   (when (file-patch-binary patch)
                                     (list (faded "    (binary)")))
                                   (loop for hunk in (file-patch-hunks patch)
                                         collect (hunk-header-spans hunk)
                                         append (map 'list #'diff-line-spans (hunk-lines hunk)))
                                   (list '()))))))

(defun commit-detail-lines (detail)
  (let ((parents (commit-detail-parents detail)))
    (append (list (list (cons (format nil "commit ~A" (commit-detail-oid detail)) "1;33"))
                  (list (cons (format nil "Author: ~A <~A>" (commit-detail-author detail)
                                      (commit-detail-email detail))
                              nil))
                  (list (cons (format nil "Date:   ~A ago" (relative-time (commit-detail-time detail)))
                              nil)))
            (when parents
              (list (list (cons (format nil "~:[Parent: ~;Parents:~] ~{~A~^ ~}" (rest parents)
                                        (mapcar #'short-oid parents))
                                nil))))
            (list '())
            (if (zerop (length (trim (commit-detail-message detail))))
                (list (faded "(empty message)"))
                (mapcar (lambda (line) (list (cons (concatenate 'string "    " (display-text line 4)) nil)))
                        (split (commit-detail-message detail) #\Newline)))
            (list '())
            (patch-lines (commit-detail-patches detail)))))

(defun show-detail (app lines)
  (setf (app-detail app) (cons (coerce lines 'vector) 0))
  (set-mode app :detail))

(defun open-commit-detail (app oid)
  (attempt (app "commit detail")
    (show-detail app (commit-detail-lines (commit-detail (app-repo app) oid)))))

(defun open-range-diff (app anchor)
  (let* ((view (app-log-view app))
         (entries (log-view-entries view))
         (lo (min anchor (log-view-cursor view)))
         (hi (max anchor (log-view-cursor view))))
    (if (>= hi (length entries))
        (err app "range out of bounds")
        (let ((newest (aref entries lo))
              (oldest (aref entries hi)))
          (attempt (app "range diff")
            (let ((patches (diff-commit-range (app-repo app) (entry-oid oldest) (entry-oid newest)))
                  (count (1+ (- hi lo))))
              (show-detail
               app
               (append (list (list (cons (format nil "Range:  ~A..~A  (~D commit~:P)"
                                                 (entry-short oldest) (entry-short newest) count)
                                         "1;33"))
                             (faded "  (combined diff: diff(parent(oldest), newest), additions = what the range added)")
                             '())
                       (loop for i from lo to hi
                             for entry = (aref entries i)
                             collect (list (cons (format nil "  ~A " (entry-short entry)) "1;90")
                                           (cons (entry-summary entry) nil)))
                       (list '())
                       (patch-lines patches)))))))))

(defun open-vs-head-diff (app oid short summary)
  (let ((head (head-oid (app-repo app))))
    (cond ((null head) (err app "HEAD is unborn; nothing to compare to"))
          ((string= head oid) (info app "cursor is on HEAD — nothing to diff"))
          (t (attempt (app "vs-HEAD")
               (let ((patches (diff-vs-head (app-repo app) oid)))
                 (show-detail
                  app
                  (append (list (list (cons (format nil "Diff:   HEAD (~A) → ~A" (short-oid head) short)
                                            "1;33"))
                                (list (cons (format nil "  ~A ~A" short summary) nil))
                                (faded "  (additions = what the target commit has that HEAD doesn't)")
                                '())
                          (patch-lines patches)))))))))

;;; Worktrees

(defun open-worktrees (app)
  (attempt (app "worktrees")
    (let ((list (worktrees (app-repo app))))
      (setf (app-worktrees app) list
            (app-worktree-cursor app) (or (position-if #'worktree-current list) 0)
            (app-message app) nil)
      (set-mode app :worktree))))

(defun reload-worktrees (app)
  (attempt (app "worktrees")
    (setf (app-worktrees app) (worktrees (app-repo app))
          (app-worktree-cursor app) (max 0 (min (app-worktree-cursor app)
                                                (1- (length (app-worktrees app))))))))

(defun visit-worktree (app path)
  (handler-case
      (let ((repo (open-repo path)))
        (setf (app-repo app) repo
              (app-status app) (make-status)
              (app-cursor app) 0
              (app-collapsed app) '()
              (app-log-view app) nil
              (app-detail app) nil)
        (clrhash (app-expanded app))
        (clrhash (app-patches app))
        (set-mode app :normal)
        (handler-case (progn (refresh app) (info app "now in ~A" path))
          (error (condition) (err app "visit: ~A" condition))))
    (not-a-repository () (err app "open worktree: ~A is not a git worktree" path))
    (error (condition) (err app "open worktree: ~A" condition))))

;;; Remotes

(defun run-push (app spec force)
  (let ((what (format nil "~A → ~A/~A~:[~; (force-with-lease)~]" (push-spec-local-branch spec)
                      (push-spec-remote spec) (push-spec-remote-branch spec) force)))
    (attempt (app "push failed")
      (busy (app "pushing ~A" what) (push-branch (app-repo app) spec force))
      (refresh-after app "pushed ~A" what))))

(defun run-fetch (app remotes)
  (let ((label (format nil "~{~A~^, ~}" remotes)))
    (handler-case
        (progn
          (busy (app "fetching ~A" label)
            (dolist (remote remotes)
              (handler-case (fetch (app-repo app) remote)
                (git-error (condition) (git-fail "fetch ~A: ~A" remote condition)))))
          (refresh-after app "fetched ~A" label))
      (error (condition) (err app "~A" condition)))))

(defun run-pull (app spec)
  (attempt (app "pull failed")
    (multiple-value-bind (outcome to)
        (busy (app "pulling ~A/~A" (push-spec-remote spec) (push-spec-remote-branch spec))
          (pull (app-repo app) spec))
      (ecase outcome
        (:up-to-date (refresh-after app "already up to date"))
        (:fast-forwarded (refresh-after app "fast-forwarded to ~A" to))
        (:rebased
         (refresh app)
         (if (status-rebase (app-status app))
             (info app "pull: diverged — rebasing (resolve conflicts, then r c)")
             (info app "pull: diverged — rebased onto upstream")))))))

;;; Visual selections

(defun visual-selection (app &optional (build (build app)))
  "SECTION PATH HUNK and the selected changed-line indices of a v selection."
  (destructuring-bind (section path hunk anchor) (rest (app-mode app))
    (let ((target (target-at-cursor app build)))
      (when (and (eq (first target) :line) (eq (second target) section)
                 (equal (third target) path) (eql (fourth target) hunk))
        (let* ((head (fifth target))
               (patch (gethash (list section path) (app-patches app)))
               (lines (and (file-patch-p patch) (nth hunk (file-patch-hunks patch)))))
          (when lines
            (values section path hunk
                    (loop for i from (min anchor head) to (max anchor head)
                          when (and (< i (length (hunk-lines lines)))
                                    (not (eq (diff-line-kind (aref (hunk-lines lines) i)) :context)))
                            collect i))))))))

(defun act-on-visual (app action)
  (multiple-value-bind (section path hunk selected) (visual-selection app)
    (let ((repo (app-repo app))
          (count (length selected)))
      (flet ((done () (set-mode app :normal)))
        (cond
          ((null section) (done))
          ((eq action :stage)
           (cond ((not (member section '(:untracked :unstaged))) (done))
                 ((null selected) (done) (info app "nothing to stage"))
                 (t (attempt (app "stage range")
                      (stage-lines repo path hunk selected :untracked (eq section :untracked))
                      (done)
                      (refresh-after app "staged ~D line(s)" count)))))
          ((eq action :unstage)
           (cond ((not (eq section :staged)) (done))
                 ((null selected) (done) (info app "nothing to unstage"))
                 (t (attempt (app "unstage range")
                      (unstage-lines repo path hunk selected)
                      (done)
                      (refresh-after app "unstaged ~D line(s)" count)))))
          ((eq action :discard)
           (cond ((eq section :staged) (err app "can't discard staged lines; unstage them first"))
                 ((eq section :untracked) (done) (info app "for untracked, discard the whole file"))
                 ((null selected) (done) (info app "nothing to discard"))
                 (t (attempt (app "discard range")
                      (discard-lines repo path hunk selected)
                      (done)
                      (refresh-after app "discarded ~D line(s)" count))))))))))

(defun move-cursor-within-hunk (app delta)
  (destructuring-bind (section path hunk anchor) (rest (app-mode app))
    (declare (ignore anchor))
    (let* ((targets (build-targets (build app)))
           (candidates (loop for target across targets
                             for i from 0
                             when (and (eq (first target) :line) (eq (second target) section)
                                       (equal (third target) path) (eql (fourth target) hunk))
                               collect i)))
      (when candidates
        (let ((at (or (position (app-cursor app) candidates) 0)))
          (setf (app-cursor app)
                (nth (max 0 (min (1- (length candidates)) (+ at delta))) candidates)))))))

(defun move-cursor-to-adjacent-file (app direction)
  (let ((targets (build-targets (build app))))
    (loop for i = (+ (app-cursor app) direction) then (+ i direction)
          while (< -1 i (length targets))
          when (eq (first (aref targets i)) :file)
            do (setf (app-cursor app) i) (return))))

(defun selected-files (app sections)
  "Paths of the file rows in a V selection, those in SECTIONS if given."
  (let ((anchor (second (app-mode app)))
        (targets (build-targets (build app))))
    (loop for i from (min anchor (app-cursor app)) to (max anchor (app-cursor app))
          for target = (and (< i (length targets)) (aref targets i))
          when (and (eq (first target) :file)
                    (or (null sections) (member (second target) sections)))
            collect (third target))))

(defun act-on-files (app paths function verb past)
  (set-mode app :normal)
  (dolist (path paths)
    (handler-case (funcall function (app-repo app) path)
      (error (condition)
        (err app "~A ~A: ~A" verb path condition)
        (ignore-errors (refresh app))
        (return-from act-on-files))))
  (refresh-after app "~A ~D file(s)" past (length paths)))

;;; Prompts

(defun prompt (app title kind &optional data)
  (set-mode app :prompt title "" kind data))

(defun dispatch-prompt (app kind data value)
  (let ((v (trim value))
        (repo (app-repo app)))
    (when (zerop (length v))
      (return-from dispatch-prompt (info app "cancelled")))
    (ecase kind
      (:rebase-onto
       (attempt (app "rebase")
         (busy (app "rebasing onto ~A" v) (rebase-onto repo v))
         (refresh-after app "rebase onto ~A: done or paused on conflict" v)))
      (:branch-checkout
       (attempt (app "checkout") (checkout-branch repo v) (refresh-after app "checked out ~A" v)))
      (:branch-create
       (attempt (app "create branch")
         (create-and-checkout-branch repo v data)
         (refresh-after app "created and checked out ~A" v)))
      (:branch-delete
       (attempt (app "delete") (delete-branch repo v) (refresh-after app "deleted branch ~A" v)))
      (:push-elsewhere
       (attempt (app "push") (run-push app (push-target-elsewhere repo v) data)))
      (:fetch-elsewhere (run-fetch app (list v)))
      (:pull-elsewhere
       (attempt (app "pull") (run-pull app (push-target-elsewhere repo v))))
      (:worktree-create
       (let ((words (remove "" (split v #\Space) :test #'string=)))
         (attempt (app "create worktree")
           (let ((path (create-worktree repo (uiop:native-namestring (arg-pathname (first words)))
                                        (second words))))
             (reload-worktrees app)
             (set-mode app :worktree)
             (info app "created worktree ~A" path))))))))

;;; Keys

(defun handle-key (app key)
  (case (mode app)
    (:normal (key-normal app key))
    (:visual (key-visual app key))
    (:visual-file (key-visual-file app key))
    (:commit-verb (key-commit-verb app key))
    (:rebase-verb (key-rebase-verb app key))
    (:log-verb (key-log-verb app key))
    (:branch-verb (key-branch-verb app key))
    (:push-verb (key-push-verb app key))
    (:fetch-verb (key-fetch-verb app key))
    (:pull-verb (key-pull-verb app key))
    (:worktree (key-worktree app key))
    (:prompt (key-prompt app key))
    ((:confirm-discard :confirm-discard-files :confirm-reset) (key-confirm app key))
    (:help (key-help app key))
    (:log (key-log app key))
    (:log-range (key-log-range app key))
    (:detail (key-detail app key))))

(defun start-chord (app mode)
  (set-mode app mode)
  (setf (app-chord-started app) (get-internal-real-time)))

(defun key-normal (app key)
  (setf (app-message app) nil)
  (let ((target (target-at-cursor app)))
    (case key
      ((#\q #.(code-char 3)) (setf (app-quit app) t))
      ((#\j :down) (move-cursor app 1))
      ((#\k :up) (move-cursor app -1))
      (:tab (when target (toggle-expand-for app target)))
      (#\g (attempt (app "refresh") (refresh app) (info app "refreshed")))
      (#\s (when target (stage-target app target)))
      (#\u (when target (unstage-target app target)))
      ((#\K #\x) (when target (set-mode app :confirm-discard target)))
      (#\v (if (eq (first target) :line)
               (progn (set-mode app :visual (second target) (third target) (fourth target)
                                (fifth target))
                      (info app "visual mode (Esc to cancel)"))))
      (#\V (if (eq (first target) :file)
               (progn (set-mode app :visual-file (app-cursor app))
                      (info app "visual file mode · j/k extend · s/u/K act · Esc cancel"))
               (info app "place the cursor on a file row first")))
      (#\c (start-chord app :commit-verb))
      (#\r (start-chord app :rebase-verb))
      (#\l (start-chord app :log-verb))
      (#\b (start-chord app :branch-verb))
      ;; The push menu has a force toggle, so it stays open until answered.
      (#\p (set-mode app :push-verb nil))
      (#\f (start-chord app :fetch-verb))
      (#\F (start-chord app :pull-verb))
      (#\w (open-worktrees app))
      (#\? (setf (app-help-scroll app) 0) (set-mode app :help))
      (:enter
       (cond ((eq (first target) :commit) (open-commit-detail app (second target)))
             ((target-path target) (open-file app (target-path target)))))
      (#\d
       (when (eq (first target) :commit)
         (let ((entry (find (second target) (status-recent-commits (app-status app))
                            :key #'entry-oid :test #'string=)))
           (open-vs-head-diff app (second target) (entry-short entry) (entry-summary entry))))))))

(defun key-commit-verb (app key)
  (set-mode app :normal)
  (case key
    (#\c (edit-and-commit app nil))
    (#\a (edit-and-commit app t))
    (t (info app "commit cancelled"))))

(defun key-rebase-verb (app key)
  (set-mode app :normal)
  (let ((repo (app-repo app)))
    (case key
      (#\e (prompt app "Rebase onto" :rebase-onto))
      (#\c (attempt (app "rebase continue")
             (busy (app "continuing rebase") (rebase-continue repo))
             (refresh-after app "rebase continued")))
      (#\a (attempt (app "rebase abort")
             (busy (app "aborting rebase") (rebase-abort repo))
             (refresh-after app "rebase aborted")))
      (#\s (attempt (app "rebase skip")
             (busy (app "skipping commit") (rebase-skip repo))
             (refresh-after app "rebase: commit skipped")))
      (#\R (autopilot app))
      (t (info app "rebase cancelled")))))

(defun key-log-verb (app key)
  (set-mode app :normal)
  (case key
    (#\l (open-log app :local))
    (#\b (open-log app :all))
    (t (info app "log cancelled"))))

(defun cursor-commit (app)
  (let ((target (target-at-cursor app)))
    (and (eq (first target) :commit) (second target))))

(defun key-branch-verb (app key)
  (let ((oid (cursor-commit app)))
    (set-mode app :normal)
    (case key
      (#\b (prompt app "Checkout branch" :branch-checkout))
      (#\c (let ((at (or oid (head-oid (app-repo app)))))
             (if at
                 (prompt app (format nil "New branch (at ~A)" (short-oid at)) :branch-create at)
                 (err app "nothing to branch from (unborn HEAD)"))))
      (#\x (prompt app "Delete branch" :branch-delete))
      (#\X (if oid
               (set-mode app :confirm-reset oid)
               (err app "place the cursor on a commit row first")))
      (t (info app "branch cancelled")))))

(defun key-push-verb (app key)
  (let ((force (second (app-mode app)))
        (repo (app-repo app)))
    (case key
      ((#\f #\-) (set-mode app :push-verb (not force)))
      (#\u (set-mode app :normal)
       (attempt (app "push") (run-push app (push-target-upstream repo) force)))
      (#\p (set-mode app :normal)
       (attempt (app "push") (run-push app (push-target-push-remote repo) force)))
      (#\e (prompt app "Push to (remote [branch])" :push-elsewhere force))
      (t (set-mode app :normal) (info app "push cancelled")))))

(defun key-fetch-verb (app key)
  (set-mode app :normal)
  (let ((repo (app-repo app)))
    (case key
      (#\u (attempt (app "fetch") (run-fetch app (list (push-spec-remote (push-target-upstream repo))))))
      (#\p (attempt (app "fetch")
             (run-fetch app (list (push-spec-remote (push-target-push-remote repo))))))
      (#\a (attempt (app "fetch")
             (let ((remotes (remote-names repo)))
               (if remotes (run-fetch app remotes) (info app "no remotes configured")))))
      (#\e (prompt app "Fetch from remote" :fetch-elsewhere))
      (t (info app "fetch cancelled")))))

(defun key-pull-verb (app key)
  (set-mode app :normal)
  (let ((repo (app-repo app)))
    (case key
      (#\u (attempt (app "pull") (run-pull app (push-target-upstream repo))))
      (#\p (attempt (app "pull") (run-pull app (push-target-push-remote repo))))
      (#\e (prompt app "Pull from (remote [branch])" :pull-elsewhere))
      (t (info app "pull cancelled")))))

(defun page (app) (max 1 (app-body-height app)))

(defun key-worktree (app key)
  (flet ((move (delta)
           (setf (app-worktree-cursor app)
                 (max 0 (min (1- (length (app-worktrees app))) (+ (app-worktree-cursor app) delta))))))
    (case key
      ((#\q :escape) (set-mode app :normal) (setf (app-message app) nil))
      ((#\j :down) (move 1))
      ((#\k :up) (move -1))
      ((:page-down #.(code-char 6)) (move (page app)))
      ((:page-up #.(code-char 2)) (move (- (page app))))
      (#\g (reload-worktrees app))
      (#\c (prompt app "New worktree (path [branch])" :worktree-create))
      (:enter
       (let ((wt (nth (app-worktree-cursor app) (app-worktrees app))))
         (when wt
           (if (worktree-current wt)
               (info app "already in this worktree")
               (visit-worktree app (worktree-path wt)))))))))

(defun key-log (app key)
  (let ((view (app-log-view app)))
    (flet ((move (delta) (log-move view delta) (log-adjust-scroll view (app-body-height app))))
      (case key
        ((#\q :escape) (setf (app-log-view app) nil (app-message app) nil) (set-mode app :normal))
        ((#\j :down) (move 1))
        ((#\k :up) (move -1))
        ((:page-down #.(code-char 6)) (move (page app)))
        ((:page-up #.(code-char 2)) (move (- (page app))))
        (#\g (setf (app-log-view app) nil) (set-mode app :normal)
         (open-log app (log-view-scope view)))
        (:tab (if (log-toggle-fold view)
                  (log-adjust-scroll view (app-body-height app))
                  (info app "Tab only folds branch tips (rows with a branch decoration)")))
        (:enter (let ((entry (log-current view)))
                  (when entry (open-commit-detail app (entry-oid entry)))))
        (#\V (set-mode app :log-range (log-view-cursor view))
         (info app "visual range: j/k extend · d / Enter combined diff · Esc cancel"))
        (#\d (let ((entry (log-current view)))
               (when entry
                 (open-vs-head-diff app (entry-oid entry) (entry-short entry) (entry-summary entry)))))))))

(defun key-log-range (app key)
  (let ((view (app-log-view app))
        (anchor (second (app-mode app))))
    (flet ((move (delta) (log-move view delta) (log-adjust-scroll view (app-body-height app))))
      (case key
        ((:escape #\V) (set-mode app :log) (setf (app-message app) nil))
        (#\q (set-mode app :log))
        ((#\j :down) (move 1))
        ((#\k :up) (move -1))
        ((:page-down #.(code-char 6)) (move (page app)))
        ((:page-up #.(code-char 2)) (move (- (page app))))
        ((#\d :enter) (open-range-diff app anchor))))))

(defun key-detail (app key)
  (let* ((detail (app-detail app))
         (height (max 1 (app-body-height app)))
         (step (max 1 (1- height))))
    (flet ((scroll (delta)
             (setf (cdr detail) (max 0 (min (max 0 (- (length (car detail)) height))
                                            (+ (cdr detail) delta))))))
      (case key
        ((#\q :escape) (setf (app-detail app) nil)
         (set-mode app (if (app-log-view app) :log :normal)))
        ((#\j :down) (scroll 1))
        ((#\k :up) (scroll -1))
        ((:page-down #.(code-char 6) #\Space) (scroll step))
        ((:page-up #.(code-char 2)) (scroll (- step)))
        (:home (setf (cdr detail) 0))
        (:end (scroll (length (car detail))))))))

(defun key-help (app key)
  (case key
    ((#\? #\q :escape) (set-mode app :normal))
    ((#\j :down) (incf (app-help-scroll app)))
    ((#\k :up) (setf (app-help-scroll app) (max 0 (1- (app-help-scroll app)))))))

(defun key-prompt (app key)
  (destructuring-bind (title buffer kind data) (rest (app-mode app))
    (case key
      (:escape (set-mode app :normal) (info app "cancelled"))
      (:enter (set-mode app :normal) (dispatch-prompt app kind data buffer))
      (:backspace (set-mode app :prompt title (subseq buffer 0 (max 0 (1- (length buffer)))) kind data))
      (#.(code-char 21) (set-mode app :prompt title "" kind data))
      (t (when (and (characterp key) (graphic-char-p key))
           (set-mode app :prompt title (concatenate 'string buffer (string key)) kind data))))))

(defun key-confirm (app key)
  (destructuring-bind (kind data) (app-mode app)
    (set-mode app :normal)
    (let ((yes (member key '(#\y #\Y))))
      (ecase kind
        (:confirm-discard (if yes (discard-target app data) (info app "discard cancelled")))
        (:confirm-discard-files
         (if yes (act-on-files app data #'discard-file "discard" "discarded") (info app "discard cancelled")))
        (:confirm-reset
         (if yes
             (attempt (app "reset")
               (reset-mixed (app-repo app) data)
               (refresh-after app "reset HEAD to ~A" (short-oid data)))
             (info app "reset cancelled")))))))

(defun key-visual (app key)
  (setf (app-message app) nil)
  (case key
    ((:escape #\v) (set-mode app :normal) (info app "visual cancelled"))
    (#\q (setf (app-quit app) t))
    ((#\j :down) (move-cursor-within-hunk app 1))
    ((#\k :up) (move-cursor-within-hunk app -1))
    (#\s (act-on-visual app :stage))
    (#\u (act-on-visual app :unstage))
    ((#\K #\x) (act-on-visual app :discard))))

(defun key-visual-file (app key)
  (setf (app-message app) nil)
  (case key
    ((:escape #\V) (set-mode app :normal) (info app "selection cancelled"))
    (#\q (setf (app-quit app) t))
    ((#\j :down) (move-cursor-to-adjacent-file app 1))
    ((#\k :up) (move-cursor-to-adjacent-file app -1))
    (#\s (let ((paths (selected-files app '(:untracked :unstaged))))
           (if paths
               (act-on-files app paths #'stage-file "stage" "staged")
               (progn (set-mode app :normal) (info app "no unstaged files in selection")))))
    (#\u (let ((paths (selected-files app '(:staged))))
           (if paths
               (act-on-files app paths #'unstage-file "unstage" "unstaged")
               (progn (set-mode app :normal) (info app "no staged files in selection")))))
    ((#\K #\x) (let ((paths (selected-files app nil)))
                 (if paths
                     (set-mode app :confirm-discard-files paths)
                     (progn (set-mode app :normal) (info app "no files selected")))))))

(defun expire-chord (app)
  (if (member (mode app) *chord-modes*)
      (when (and (app-chord-started app)
                 (>= (- (get-internal-real-time) (app-chord-started app))
                     (* *chord-timeout* internal-time-units-per-second)))
        (set-mode app :normal)
        (setf (app-chord-started app) nil)
        (info app "chord cancelled"))
      (setf (app-chord-started app) nil)))

;;; Drawing

(defun label (text) (cons text "90"))

(defun head-lines (app)
  (let* ((status (app-status app))
         (upstream (status-upstream status))
         (tag (status-tag status))
         (rebase (status-rebase status)))
    (remove nil
            (list
             (append (list (label "Head:   "))
                     (cond ((status-detached status)
                            (list (cons "(detached) " "1;31")
                                  (cons (short-oid (or (status-oid status) "")) "90")))
                           ((status-oid status)
                            (list (cons (format nil "~A " (status-branch status)) "1;36")
                                  (cons (short-oid (status-oid status)) "90")))
                           (t (list (cons "(unborn branch)" "90"))))
                     (when (status-summary status)
                       (list (cons (format nil " ~A" (status-summary status)) nil))))
             (when upstream
               (append (list (label "Merge:  ")
                             (cons (format nil "~A " (upstream-name upstream)) "1;31")
                             (cons (upstream-short-oid upstream) "90"))
                       (when (upstream-summary upstream)
                         (list (cons (format nil " ~A" (upstream-summary upstream)) nil)))
                       (when (or (plusp (upstream-ahead upstream)) (plusp (upstream-behind upstream)))
                         (list (cons (format nil "  (~D ahead, ~D behind)" (upstream-ahead upstream)
                                             (upstream-behind upstream))
                                     "33")))))
             ;; `git describe' gives "v1.0" on the tag, "v1.0-3-gabc1234" past it.
             (when tag
               (let ((dash (position #\- tag)))
                 (list (label "Tag:    ")
                       (cons (subseq tag 0 dash) "1;33")
                       (cons (if dash (subseq tag dash) "") "90"))))
             (when rebase
               (list (cons (format nil "Rebasing ~A (~D/~D)~:[~; · conflicts present~]"
                                   (let ((name (rebase-state-head-name rebase)))
                                     (if (and name (uiop:string-prefix-p "refs/heads/" name))
                                         (subseq name 11)
                                         (or name "HEAD")))
                                   (or (rebase-state-current rebase) 0) (rebase-state-total rebase)
                                   (rebase-state-conflicts rebase))
                           (if (rebase-state-conflicts rebase) "1;31" "1;35"))))))))

(defun spinner ()
  (char *spinner* (mod (floor (get-internal-real-time) (floor internal-time-units-per-second 12))
                       (length *spinner*))))

(defun footer (app)
  (let ((progress (app-progress app))
        (mode (app-mode app)))
    (flet ((menu (text) (list (cons text "1;33"))))
      (cond
        (progress
         (destructuring-bind (description start ai) progress
           (append (if ai
                       (list (cons " AI " "1;30;45") (cons " " nil))
                       (list (cons (format nil "~C " (spinner)) "1;35")))
                   (list (cons description "1;35")
                         (cons (format nil " · ~Ds" (floor (- (get-internal-real-time) start)
                                                           internal-time-units-per-second))
                               nil))
                   (when ai (list (cons "   (Esc to cancel)" "90"))))))
        ((eq (first mode) :commit-verb) (menu "Commit: c=create · a=amend · any other key cancels"))
        ((eq (first mode) :rebase-verb)
         (menu "Rebase: e=onto · c=continue · a=abort · s=skip · R=AI autopilot · any other key cancels"))
        ((eq (first mode) :log-verb) (menu "Log: l=local branches · b=all branches · any other key cancels"))
        ((eq (first mode) :branch-verb)
         (menu "Branch: b=checkout · c=create+checkout · x=delete · X=reset (mixed) · any other key cancels"))
        ((eq (first mode) :push-verb)
         (menu (format nil "Push: p=push-remote · u=upstream · e=elsewhere · f=force-with-lease[~:[off~;ON~]] · Esc cancels"
                       (second mode))))
        ((eq (first mode) :fetch-verb)
         (menu "Fetch: u=upstream remote · p=push-remote · a=all remotes · e=elsewhere · any other key cancels"))
        ((eq (first mode) :pull-verb)
         (menu "Pull: u=upstream · p=push-remote · e=elsewhere · (ff or rebase) · any other key cancels"))
        ((eq (first mode) :prompt)
         (list (cons (format nil "~A: " (second mode)) "1;36")
               (cons (third mode) nil)
               (cons "_" "5")
               (cons "   (Enter=submit · Esc=cancel)" "90")))
        ((eq (first mode) :confirm-discard)
         (let ((target (second mode)))
           (menu (format nil "Discard ~A? (y/N)"
                         (ecase (first target)
                           (:file (format nil "file ~A" (third target)))
                           (:hunk (format nil "hunk #~D of ~A" (1+ (fourth target)) (third target)))
                           (:line (format nil "line in ~A" (third target)))
                           (:commit "this commit")
                           (:header (format nil "section ~A" (section-label (second target)))))))))
        ((eq (first mode) :confirm-discard-files)
         (menu (format nil "Discard ~D selected file(s)? (y/N)" (length (second mode)))))
        ((eq (first mode) :confirm-reset)
         (menu (format nil "Reset HEAD (mixed) to ~A ? (y/N)  worktree edits will be kept, the branch ref moves."
                       (short-oid (second mode)))))
        ((app-message app)
         (list (cons (car (app-message app)) (if (eq (cdr (app-message app)) :error) "31" "32"))))
        (t
         (faded (case (first mode)
                  (:visual "VISUAL · j/k extend · s stage · u unstage · K discard · Esc cancel")
                  (:visual-file "VISUAL FILE · j/k extend · s stage · u unstage · K discard · Esc cancel")
                  (:help "HELP · press ? or Esc to close")
                  (:log "LOG · j/k · Enter detail · d vs HEAD · V range · Tab fold · g reload · q back to status")
                  (:log-range "LOG · RANGE · j/k extend · d / Enter combined diff · Esc cancel")
                  (:detail "COMMIT · j/k scroll · PgUp/PgDn or Ctrl-B/F page · Home/End jump · q back to log")
                  (:worktree "WORKTREES · j/k · PgUp/PgDn · Enter visit · c create · g reload · q back to status")
                  (t "j/k · s stage · u unstage · K discard · V select · c commit · r rebase · b branch · p push · f fetch · F pull · w worktrees · l log · ? help · q quit"))))))))

(defun status-body (app height)
  "Lines of the status page and the base SGR of each, scrolled to keep the
cursor about mid-screen."
  (let* ((b (build app))
         (lines (build-lines b))
         (bases (make-array (length lines) :initial-element nil))
         (mode (app-mode app))
         (cursor-line (if (< (app-cursor app) (length (build-target-lines b)))
                          (aref (build-target-lines b) (app-cursor app))
                          0)))
    (case (first mode)
      (:visual
       (destructuring-bind (section path hunk anchor) (rest mode)
         (let ((target (target-at-cursor app b))
               (first-line (gethash (list section path hunk) (build-hunk-first b))))
           (when (and first-line (eq (first target) :line) (eq (second target) section)
                      (equal (third target) path) (eql (fourth target) hunk))
             (loop for i from (min anchor (fifth target)) to (max anchor (fifth target))
                   when (< (+ first-line i) (length bases))
                     do (setf (aref bases (+ first-line i)) "7"))))))
      (:visual-file
       (loop for i from (min (second mode) (app-cursor app)) to (max (second mode) (app-cursor app))
             when (and (< i (length (build-targets b))) (eq (first (aref (build-targets b) i)) :file))
               do (setf (aref bases (aref (build-target-lines b) i)) "7"))))
    (when (< (app-cursor app) (length (build-target-lines b)))
      (setf (aref bases cursor-line) "7"))
    (let ((scroll (max 0 (min (- cursor-line (floor height 2)) (- (length lines) height)))))
      (loop for i from scroll below (min (length lines) (+ scroll height))
            collect (cons (aref lines i) (aref bases i))))))

(defun log-body (app height)
  (let* ((view (app-log-view app))
         (rows (log-view-rows view))
         (entry-rows (log-view-entry-rows view))
         (cursor-row (aref entry-rows (log-view-cursor view)))
         (range (and (eq (mode app) :log-range)
                     (let ((anchor (second (app-mode app))))
                       (loop for i from (min anchor (log-view-cursor view))
                               to (max anchor (log-view-cursor view))
                             when (aref entry-rows i) collect (aref entry-rows i))))))
    (loop for i from (log-view-scroll view) below (min (length rows) (+ (log-view-scroll view) height))
          collect (cons (car (aref rows i))
                        (and (or (eql i cursor-row) (member i range)) "7")))))

(defun detail-body (app height)
  (destructuring-bind (lines . scroll) (app-detail app)
    (loop for i from scroll below (min (length lines) (+ scroll height))
          collect (cons (aref lines i) nil))))

(defun worktree-body (app height)
  (let* ((lines (append (list (list (cons (format nil "Worktrees (~D)" (length (app-worktrees app))) "1;33"))
                              '())
                        (loop for wt in (app-worktrees app)
                              collect (append
                                       (list (cons (if (worktree-current wt) "● " "  ") "1;36")
                                             (cons (format nil "~20A " (or (worktree-branch wt) "(detached)"))
                                                   "1;32")
                                             (cons (format nil "~8A " (or (worktree-head-short wt) "")) "90")
                                             (cons (worktree-path wt) nil))
                                       (when (worktree-main wt) (list (cons "  [main]" "90")))
                                       (when (worktree-locked wt) (list (cons "  [locked]" "31")))))))
         (cursor-line (+ 2 (app-worktree-cursor app)))
         (scroll (max 0 (min (- cursor-line (floor height 2)) (- (length lines) height)))))
    (loop for line in (nthcdr scroll lines)
          for i from scroll
          repeat height
          collect (cons line (and (= i cursor-line) "7")))))

(defun help-lines ()
  (flet ((key (k description) (list (cons (format nil "  ~14A" k) "1;33") (cons description nil)))
         (heading (text) (list (cons text "1;36"))))
    (list '()
          (heading "  Navigation")
          (key "j / ↓" "Move cursor down")
          (key "k / ↑" "Move cursor up")
          (key "Tab" "Collapse / expand the section (on a section heading) or the file (on a file row)")
          (key "g" "Reload status")
          '()
          (heading "  Staging")
          (key "s" "Stage file / hunk / line / visual range (a file under Unmerged marks it resolved)")
          (key "u" "Unstage")
          (key "K, x" "Discard (with confirmation)")
          (key "v" "Visual mode on a diff line: j/k extend, s/u apply")
          (key "V" "Select a range of files: j/k extend, s/u/K act on all")
          (key "Enter" "Open the file under cursor in $EDITOR")
          '()
          (heading "  Commit")
          (key "c c" "Commit (opens $EDITOR)")
          (key "c a" "Amend HEAD (opens $EDITOR)")
          '()
          (heading "  Rebase")
          (key "r e" "Start rebase onto a ref (prompts for the target)")
          (key "r c" "Continue an in-progress rebase")
          (key "r a" "Abort an in-progress rebase")
          (key "r s" "Skip the current commit")
          (key "r R" "AI autopilot: resolve conflicts and continue")
          '()
          (heading "  Branch")
          (key "b b" "Checkout an existing branch (prompts for name)")
          (key "b c" "Create + checkout at cursor commit (or HEAD)")
          (key "b x" "Delete a branch (prompts for name)")
          (key "b X" "Reset HEAD (mixed) to the commit under the cursor")
          '()
          (heading "  Push")
          (key "p u" "Push current branch to its upstream")
          (key "p p" "Push to the push-remote (sets upstream if unset)")
          (key "p e" "Push elsewhere (prompts for `remote [branch]`)")
          (key "p f" "Toggle force-with-lease for the next push")
          '()
          (heading "  Fetch / Pull")
          (key "f u" "Fetch the upstream's remote")
          (key "f p" "Fetch the push-remote")
          (key "f a" "Fetch all remotes")
          (key "f e" "Fetch a named remote (prompt)")
          (key "F u" "Pull upstream (fast-forward, else rebase)")
          (key "F p" "Pull the push-remote")
          (key "F e" "Pull elsewhere (prompt for `remote [branch]`)")
          '()
          (heading "  Worktrees")
          (key "w" "Open the worktree list")
          (key "Enter" "Visit the highlighted worktree (in the list)")
          (key "c" "Create a worktree: `<path> [branch]` (in the list)")
          '()
          (heading "  Recent commits (status page)")
          (key "Enter" "Open commit detail on the highlighted row")
          (key "d" "Diff cursor commit vs HEAD")
          '()
          (heading "  Log")
          (key "l l" "Open log of local branches")
          (key "l b" "Open log of all branches (local + remotes)")
          (key "j / k" "Move through commits")
          (key "PgDn / Ctrl-F" "Page forward")
          (key "PgUp / Ctrl-B" "Page backward")
          (key "Enter" "Open commit detail (header, message, patches)")
          (key "d" "Diff cursor commit vs HEAD")
          (key "V" "Visual range: extend with j/k, then d / Enter for combined diff")
          (key "Tab" "Hide / show this branch (cursor must be on a branch tip)")
          (key "g" "Reload the log")
          (key "q" "Close log / detail panel")
          '()
          (heading "  Misc")
          (key "?" "Toggle this help (j/k scroll it)")
          (key "Esc" "Cancel chord / visual / prompt / confirmation")
          (key "q" "Quit (status view) · close panel (log / detail / help)")
          '()
          (faded "  A chord (c / r / l / b / f / F) auto-cancels if you don't press the second key."))))

(defun pad-spans (spans width)
  "SPANS cut or padded to WIDTH columns."
  (let ((used 0))
    (append (loop for (text . sgr) in spans
                  while (< used width)
                  collect (cons (with-output-to-string (out)
                                  (loop for char across text
                                        for w = (display-width (string char))
                                        while (<= (+ used w) width)
                                        do (write-char char out) (incf used w)))
                                sgr))
            (list (cons (make-string (max 0 (- width used)) :initial-element #\Space) nil)))))

(defun help-box (app width height)
  "The help overlay as HEIGHT lines of spans WIDTH columns wide."
  (let* ((inner (- width 2))
         (lines (help-lines))
         (visible (- height 2))
         (scroll (setf (app-help-scroll app)
                       (max 0 (min (app-help-scroll app) (- (length lines) visible)))))
         (title " Help · press ? or Esc to close ")
         (border "36"))
    (append (list (list (cons (format nil "┌~A~A┐" title
                                      (make-string (max 0 (- inner (length title)))
                                                   :initial-element #\─))
                              border)))
            (loop for i from scroll below (+ scroll visible)
                  collect (append (list (cons "│" border))
                                  (pad-spans (nth i lines) inner)
                                  (list (cons "│" border))))
            (list (list (cons (format nil "└~A┘" (make-string inner :initial-element #\─)) border))))))

(defun splash (app)
  (list (cons '() nil)
        (cons (list (cons "  Loading repository…" "1;36")) nil)
        (cons (list (cons (format nil "  ~A" (repo-workdir (app-repo app))) "90")) nil)))

(defun draw (app)
  "The whole screen as one string."
  (multiple-value-bind (rows cols) (terminal-size (app-terminal app))
    (let* ((esc (code-char 27))
           (head (if (app-loaded app) (head-lines app) '()))
           (height (max 1 (- rows (length head) 1)))
           (body (cond ((not (app-loaded app)) (splash app))
                       ((member (mode app) '(:log :log-range)) (log-body app height))
                       ((eq (mode app) :detail) (detail-body app height))
                       ((eq (mode app) :worktree) (worktree-body app height))
                       (t (status-body app height))))
           (screen (append (mapcar (lambda (line) (cons line nil)) head) body)))
      (setf (app-body-height app) height)
      (with-output-to-string (out)
        (loop for (spans . base) in screen
              for row from 1 below rows
              do (format out "~C[~D;1H~A" esc row (fit-spans-to-width spans cols base)))
        (loop for row from (1+ (length screen)) below rows
              do (format out "~C[~D;1H~C[K" esc row esc))
        (when (eq (mode app) :help)
          ;; Fixed-width (no wide characters) help text, so the box lines up.
          (let* ((width (max 20 (floor (* cols 7) 10)))
                 (box-height (max 5 (floor (* rows 8) 10)))
                 (left (1+ (floor (- cols width) 2)))
                 (top (1+ (floor (- rows box-height) 2))))
            (loop for line in (help-box app width box-height)
                  for row from top
                  do (format out "~C[~D;~DH~A" esc row left (fit-spans-to-width line width)))))
        (format out "~C[~D;1H~A" esc rows (fit-spans-to-width (footer app) cols))))))

(defun render (app)
  (let ((screen (draw app)))
    (unless (equal screen (app-last-screen app))
      (setf (app-last-screen app) screen)
      (terminal-write (app-terminal app) screen)
      (terminal-flush (app-terminal app)))))

;;; The command

(defun run-app (app)
  (render app)
  (handler-case (refresh app)
    (error (condition) (err app "load: ~A" condition)))
  (setf (app-loaded app) t)
  (loop until (app-quit app)
        do (expire-chord app)
           (render app)
           (let ((key (read-key (app-terminal app) 250)))
             (when key
               (handler-case (handle-key app key)
                 (terminal-closed (condition) (error condition))
                 (error (condition) (err app "~A" condition)))))))

(defcommand (grit :name "grit") (&optional path)
  "A keyboard-driven Git client: stage hunks and lines, commit, rebase, push.
usage: grit [PATH]

Runs on the repository holding PATH, by default the current directory.
Press ? inside for the keys. Commit messages are edited in the editor git
would use (GIT_EDITOR, core.editor, VISUAL or EDITOR)."
  (let ((directory (or (string-right-trim "/" (uiop:native-namestring
                                               (if path (arg-directory path) *default-pathname-defaults*)))
                       "/")))
    (when (zerop (length directory)) (setf directory "/"))
    (let ((repo (handler-case (open-repo directory)
                  (not-a-repository ()
                    (error 'command-failed
                           :code 2
                           :message (format nil "not inside a Git repository (started from ~A)~%~
                                                 hint: cd into a checkout, or pass a path: `grit /path/to/repo`"
                                            (if path path directory)))))))
      (with-terminal (terminal)
        (run-app (make-app :repo repo :terminal terminal))))))
