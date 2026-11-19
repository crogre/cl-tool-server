(in-package #:grit)

;;; Log view: the commit graph, folding and scrolling.
;;;
;;; The graph is drawn by lane: each lane is a column carrying the next
;;; commit (going back in time) it expects. A commit lands in the leftmost
;;; lane expecting it, or opens a new lane as a branch tip. As in
;;; `git log --graph', diagonals appear only on transition rows between
;;; commit rows: one before a commit where lanes converge on it, one after a
;;; commit where the lane shape changes. Linear history and two-parent merges
;;; come out right; rarer shapes are drawn approximately.
;;;
;;; Tab on a branch tip folds it: commits that only folded tips reach are
;;; hidden, and the tip stays, marked, so it can be unfolded.

(defparameter *lane-colors* #("34" "32" "36" "35" "94" "92" "96" "95"))

(defun lane-color (column)
  (aref *lane-colors* (mod column (length *lane-colors*))))

(defun ref-color (kind)
  (ecase kind (:head "1;36") (:branch "1;32") (:remote "1;31") (:tag "1;93")))

(defun refs-spans (refs)
  (when refs
    (append (list (cons "(" "1;33"))
            (loop for ((name . kind) . more) on refs
                  collect (cons name (ref-color kind))
                  when more collect (cons ", " "33"))
            (list (cons ") " "1;33")))))

(defun unix-now ()
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

(defun relative-time (seconds)
  "\"3h\", \"2d\", \"1w\": how long ago SECONDS, a Unix time, was."
  (let ((diff (max 0 (- (unix-now) (or seconds 0)))))
    (cond ((< diff 60) (format nil "~Ds" diff))
          ((< diff 3600) (format nil "~Dm" (floor diff 60)))
          ((< diff 86400) (format nil "~Dh" (floor diff 3600)))
          ((< diff 604800) (format nil "~Dd" (floor diff 86400)))
          ((< diff 2592000) (format nil "~Dw" (floor diff 604800)))
          ((< diff 31536000) (format nil "~Dmo" (floor diff 2592000)))
          (t (format nil "~Dy" (floor diff 31536000))))))

(defun foldable-p (entry)
  (some (lambda (ref) (member (cdr ref) '(:branch :remote))) (entry-refs entry)))

(defun build-reach (entries)
  "Hash of oid -> the foldable tips whose ancestry includes it."
  (let ((by-oid (make-hash-table :test 'equal))
        (reach (make-hash-table :test 'equal)))
    (loop for entry across entries do (setf (gethash (entry-oid entry) by-oid) entry))
    (loop for tip across entries
          when (foldable-p tip)
            do (let ((seen (make-hash-table :test 'equal))
                     (stack (list (entry-oid tip))))
                 (loop while stack
                       do (let ((oid (pop stack)))
                            (unless (gethash oid seen)
                              (setf (gethash oid seen) t)
                              (pushnew (entry-oid tip) (gethash oid reach) :test #'equal)
                              (let ((entry (gethash oid by-oid)))
                                (when entry
                                  (dolist (parent (entry-parents entry))
                                    (push parent stack)))))))))
    reach))

(defun hidden-p (entry folded reach)
  "Whether every tip reaching ENTRY is folded; folded tips themselves show."
  (let ((tips (gethash (entry-oid entry) reach)))
    (and (not (member (entry-oid entry) folded :test #'equal))
         tips
         (every (lambda (tip) (member tip folded :test #'equal)) tips))))

(defun count-hidden-under (tip entries folded reach)
  (count-if (lambda (entry)
              (let ((tips (gethash (entry-oid entry) reach)))
                (and (string/= (entry-oid entry) tip)
                     (member tip tips :test #'equal)
                     (every (lambda (other) (member other folded :test #'equal)) tips))))
            entries))

(defun build-rows (entries folded reach)
  "The graph's rows, a vector of (SPANS . ENTRY-INDEX-OR-NIL), and a vector
mapping each entry to its row, NIL where hidden."
  (let ((rows (make-array 0 :adjustable t :fill-pointer 0))
        (entry-rows (make-array (length entries) :initial-element nil))
        (lanes (make-array 0 :adjustable t :fill-pointer 0)))
    (flet ((cell (char color) (cons (format nil "~C " char) color)))
      (loop for entry across entries
            for index from 0
            for oid = (entry-oid entry)
            for hidden = (hidden-p entry folded reach)
            for tip-folded = (member oid folded :test #'equal)
            do (let* ((column (or (position oid lanes :test #'equal)
                                  (progn (vector-push-extend oid lanes) (1- (length lanes)))))
                      (convergent (loop for lane across lanes
                                        for c from 0
                                        when (and (/= c column) (equal lane oid)) collect c)))
                 ;; Lanes also expecting this commit join its column.
                 (when (and (not hidden) convergent)
                   (vector-push-extend
                    (cons (loop for lane across lanes
                                for c from 0
                                collect (cond ((member c convergent)
                                               (cell (if (> c column) #\/ #\\) "35"))
                                              ((or (= c column) lane) (cell #\| (lane-color c)))
                                              (t (cell #\Space nil))))
                          nil)
                    rows))
                 (dolist (c convergent) (setf (aref lanes c) nil))
                 (if hidden
                     (setf (aref entry-rows index) nil)
                     (let ((spans (loop for lane across lanes
                                        for c from 0
                                        collect (cond ((= c column) (cell #\* "33"))
                                                      (lane (cell #\| (lane-color c)))
                                                      (t (cell #\Space nil))))))
                       (setf spans (append spans
                                           (list (cons (format nil " ~A " (entry-short entry)) "1;90"))
                                           (refs-spans (entry-refs entry))
                                           (list (cons (entry-summary entry) nil)
                                                 (cons (format nil "  · ~A · ~A" (entry-author entry)
                                                               (relative-time (entry-time entry)))
                                                       "90"))))
                       (when tip-folded
                         (setf spans (append spans
                                             (list (cons (format nil "   ⟪ folded · ~D hidden ⟫"
                                                                 (count-hidden-under oid entries folded reach))
                                                         "1;35")))))
                       (setf (aref entry-rows index) (length rows))
                       (vector-push-extend (cons spans index) rows)))
                 ;; Roll the lanes on; a folded tip lets its lane end.
                 (let ((next (make-array (length lanes) :adjustable t :fill-pointer (length lanes)
                                                        :initial-contents lanes)))
                   (setf (aref next column) nil)
                   (unless tip-folded
                     (let ((parents (entry-parents entry)))
                       (when parents
                         (setf (aref next column) (first parents)))
                       (dolist (parent (rest parents))
                         (unless (find parent next :test #'equal)
                           (let ((free (position nil next)))
                             (if free
                                 (setf (aref next free) parent)
                                 (vector-push-extend parent next)))))))
                   (when (and (not hidden) (< (1+ index) (length entries))
                              (not (equal (map 'list (lambda (lane) (and lane t)) lanes)
                                          (map 'list (lambda (lane) (and lane t)) next))))
                     (vector-push-extend
                      (cons (loop for c below (max (length lanes) (length next))
                                  for before = (and (< c (length lanes)) (aref lanes c))
                                  for after = (and (< c (length next)) (aref next c))
                                  collect (cond ((and before after) (cell #\| (lane-color c)))
                                                (after (cond ((> c column) (cell #\\ "35"))
                                                             ((< c column) (cell #\/ "35"))
                                                             (t (cell #\| (lane-color c)))))
                                                (before (cond ((= c column) (cell #\Space nil))
                                                              ((> c column) (cell #\/ "35"))
                                                              (t (cell #\\ "35"))))
                                                (t (cell #\Space nil))))
                            nil)
                      rows))
                   (setf lanes next)
                   (loop while (and (plusp (length lanes)) (null (aref lanes (1- (length lanes)))))
                         do (vector-pop lanes))))))
    (values rows entry-rows)))

(defstruct (log-view (:constructor %make-log-view))
  entries scope (cursor 0) (scroll 0) rows entry-rows (folded '()) reach)

(defun make-log-view (entries scope)
  (let* ((entries (coerce entries 'vector))
         (view (%make-log-view :entries entries :scope scope :reach (build-reach entries))))
    (rebuild-log-rows view)
    view))

(defun rebuild-log-rows (view)
  (multiple-value-bind (rows entry-rows)
      (build-rows (log-view-entries view) (log-view-folded view) (log-view-reach view))
    (setf (log-view-rows view) rows
          (log-view-entry-rows view) entry-rows)))

(defun log-current (view)
  (let ((entries (log-view-entries view)))
    (and (< (log-view-cursor view) (length entries)) (aref entries (log-view-cursor view)))))

(defun log-move (view delta)
  "Move the cursor DELTA visible commits, skipping folded ones."
  (let ((last (1- (length (log-view-entries view))))
        (n (log-view-cursor view)))
    (unless (or (minusp last) (zerop delta))
      (let ((step (if (plusp delta) 1 -1))
            (remaining (abs delta)))
        (loop while (plusp remaining)
              do (let ((candidate (+ n step)))
                   (when (or (minusp candidate) (> candidate last)) (return))
                   (setf n candidate)
                   (when (aref (log-view-entry-rows view) n) (decf remaining)))))
      (setf (log-view-cursor view) n))))

(defun log-toggle-fold (view)
  (let ((entry (log-current view)))
    (when (and entry (foldable-p entry))
      (let ((oid (entry-oid entry)))
        (setf (log-view-folded view)
              (if (member oid (log-view-folded view) :test #'equal)
                  (remove oid (log-view-folded view) :test #'equal)
                  (cons oid (log-view-folded view)))))
      (rebuild-log-rows view)
      t)))

(defun log-adjust-scroll (view height)
  "Keep the cursor's row in a viewport HEIGHT rows tall."
  (let ((row (and (< (log-view-cursor view) (length (log-view-entry-rows view)))
                  (aref (log-view-entry-rows view) (log-view-cursor view)))))
    (when (and row (plusp height))
      (cond ((< row (log-view-scroll view)) (setf (log-view-scroll view) row))
            ((>= row (+ (log-view-scroll view) height))
             (setf (log-view-scroll view) (- row (1- height))))))))
