(defpackage #:grit-tests
  (:use #:cl)
  (:export #:run-tests))

(in-package #:grit-tests)

;;; Tests for grit's pure parts (status and diff parsing, patch building,
;;; the log graph, config) and for its git operations on throwaway
;;; repositories, many ported from the original's Rust tests.
;;;
;;;   sbcl --non-interactive --eval '(require :asdf)' \
;;;        --eval '(asdf:load-asd (truename "tool-server.asd"))' \
;;;        --eval '(asdf:test-system "tool-server")'

(defvar *tests* '())
(defvar *failures*)

(defmacro deftest (name &body body)
  `(progn (defun ,name () ,@body)
          (pushnew ',name *tests*)))

(defmacro check (form &optional description)
  `(unless ,form
     (push (format nil "~(~S~)~@[ (~A)~]" ',form ,description) *failures*)))

(defun run-tests ()
  "Run every test; print failures and return true if there were none."
  (let ((failed 0))
    (dolist (test (reverse *tests*))
      (let ((*failures* '()))
        (handler-case (funcall test)
          (error (condition) (push (format nil "error: ~A" condition) *failures*)))
        (when *failures*
          (incf failed)
          (format t "~&FAIL ~(~A~)~%~{    ~A~%~}" test (reverse *failures*)))))
    (format t "~&~D test(s), ~D failed~%" (length *tests*) failed)
    (zerop failed)))

(defun s (&rest strings)
  "STRINGS joined by newlines, with a final one."
  (format nil "~{~A~%~}" strings))

;;; Status

(deftest status-sections
  (let* ((nul (string (code-char 0)))
         (status (grit::parse-status
                  (concatenate 'string
                               "# branch.oid 1234567890abcdef" nul
                               "# branch.head main" nul
                               "# branch.upstream origin/main" nul
                               "# branch.ab +2 -1" nul
                               "1 .M N... 100644 100644 100644 aaa aaa file.txt" nul
                               "1 MM N... 100644 100644 100644 aaa bbb both.txt" nul
                               "1 D. N... 100644 000000 000000 aaa 000 gone.txt" nul
                               "2 R. N... 100644 100644 100644 aaa aaa R100 new name.txt" nul
                               "old name.txt" nul
                               "u UU N... 100644 100644 100644 100644 a b c clash.txt" nul
                               "? dir/" nul
                               "? loose.txt" nul))))
    (check (equal (grit::status-branch status) "main"))
    (check (equal (grit::status-oid status) "1234567890abcdef"))
    (check (= (grit::upstream-ahead (grit::status-upstream status)) 2))
    (check (= (grit::upstream-behind (grit::status-upstream status)) 1))
    (check (equal (grit::status-untracked status) '("dir/" "loose.txt")))
    (check (equal (mapcar #'grit::change-path (grit::status-unstaged status)) '("both.txt" "file.txt")))
    (check (equal (mapcar #'grit::change-path (grit::status-staged status))
                  '("both.txt" "gone.txt" "new name.txt")))
    (let ((rename (find "new name.txt" (grit::status-staged status) :key #'grit::change-path
                                                                  :test #'string=)))
      (check (eq (grit::change-kind rename) :renamed))
      (check (equal (grit::change-old-path rename) "old name.txt")))
    (check (eq (grit::change-kind (find "gone.txt" (grit::status-staged status)
                                        :key #'grit::change-path :test #'string=))
               :deleted))
    (check (equal (mapcar #'grit::change-path (grit::status-conflicted status)) '("clash.txt")))))

(deftest status-unborn-and-detached
  (let ((nul (string (code-char 0))))
    (let ((status (grit::parse-status (concatenate 'string "# branch.oid (initial)" nul
                                                   "# branch.head main" nul))))
      (check (null (grit::status-oid status)))
      (check (equal (grit::status-branch status) "main")))
    (let ((status (grit::parse-status (concatenate 'string "# branch.oid abc" nul
                                                   "# branch.head (detached)" nul))))
      (check (grit::status-detached status))
      (check (null (grit::status-branch status))))))

;;; Diffs

(defparameter *two-file-diff*
  (s "diff --git a/f.txt b/f.txt"
     "index 1111111..2222222 100644"
     "--- a/f.txt"
     "+++ b/f.txt"
     "@@ -1,5 +1,5 @@ heading"
     " a"
     "-b"
     "+B"
     ""
     " d"
     "@@ -20 +20,2 @@"
     "-last"
     "\\ No newline at end of file"
     "+last"
     "+more"
     "\\ No newline at end of file"
     "diff --git a/img.png b/img.png"
     "index 3333333..4444444 100644"
     "Binary files a/img.png and b/img.png differ"
     "diff --git \"a/sp\\303\\244ce\\tx\" \"b/sp\\303\\244ce\\tx\""
     "new file mode 100755"
     "index 0000000..5555555"
     "--- /dev/null"
     "+++ \"b/sp\\303\\244ce\\tx\""
     "@@ -0,0 +1 @@"
     "+hello"))

(deftest diff-parsing
  (destructuring-bind (text binary new) (grit::parse-diff *two-file-diff*)
    (check (equal (grit::file-patch-path text) "f.txt"))
    (check (= (length (grit::file-patch-hunks text)) 2))
    (let ((first (first (grit::file-patch-hunks text)))
          (second (second (grit::file-patch-hunks text))))
      (check (= (grit::hunk-old-start first) 1))
      (check (= (grit::hunk-new-count first) 5))
      (check (= (length (grit::hunk-lines first)) 5) "an empty line is blank context")
      (check (eq (grit::diff-line-kind (aref (grit::hunk-lines first) 3)) :context))
      (check (= (grit::hunk-old-count second) 1) "a count of one may be left out")
      (check (grit::diff-line-no-eol (aref (grit::hunk-lines second) 0)))
      (check (not (grit::diff-line-no-eol (aref (grit::hunk-lines second) 1))))
      (check (grit::diff-line-no-eol (aref (grit::hunk-lines second) 2)))
      (check (equal (grit::hunk-text second)
                    (s "@@ -20 +20,2 @@" "-last" "\\ No newline at end of file" "+last" "+more"
                       "\\ No newline at end of file"))
             "a hunk round-trips"))
    (check (grit::file-patch-binary binary))
    (check (equal (grit::file-patch-path binary) "img.png"))
    (check (null (grit::file-patch-hunks binary)))
    (check (equal (grit::file-patch-path new) (format nil "späce~Cx" #\Tab)) "C-quoted UTF-8 path")
    (check (grit::file-patch-new-p new))
    (check (equal (grit::file-patch-mode new) "100755"))))

(deftest diff-display-text
  (check (equal (grit::display-text (format nil "a~Cb" #\Tab) 0) "a       b"))
  (check (equal (grit::display-text (format nil "a~Cb" #\Tab) 5) "a  b"))
  (check (equal (grit::display-text (sb-ext:octets-to-string
                                     (sb-ext:string-to-octets "ü" :external-format :utf-8)
                                     :external-format :latin-1))
                "ü"))
  (check (equal (grit::display-text (format nil "x~Cy" (code-char 7))) "x?y")))

;;; Patches for lines

(defun line-fixture-patch ()
  "a b c d e -> a B c D e: one hunk of interleaved pairs."
  (first (grit::parse-diff
          (s "diff --git a/f.txt b/f.txt"
             "--- a/f.txt"
             "+++ b/f.txt"
             "@@ -1,5 +1,5 @@"
             " a" "-b" "+B" " c" "-d" "+D" " e"))))

(defun index-of (hunk kind raw)
  (position-if (lambda (line) (and (eq (grit::diff-line-kind line) kind)
                                   (string= (grit::diff-line-raw line) raw)))
               (grit::hunk-lines hunk)))

(deftest partial-forward-pair
  (let* ((patch (line-fixture-patch))
         (hunk (first (grit::file-patch-hunks patch))))
    (check (equal (grit::partial-patch patch hunk (list (index-of hunk :del "b") (index-of hunk :add "B")) nil)
                  (s "diff --git a/f.txt b/f.txt" "--- a/f.txt" "+++ b/f.txt"
                     "@@ -1,5 +1,5 @@" " a" "-b" "+B" " c" " d" " e")))))

(deftest partial-forward-add-only
  (let* ((patch (line-fixture-patch))
         (hunk (first (grit::file-patch-hunks patch))))
    (check (equal (grit::partial-patch patch hunk (list (index-of hunk :add "B")) nil)
                  (s "diff --git a/f.txt b/f.txt" "--- a/f.txt" "+++ b/f.txt"
                     "@@ -1,5 +1,6 @@" " a" " b" "+B" " c" " d" " e")))))

(deftest partial-reverse
  (let* ((patch (line-fixture-patch))
         (hunk (first (grit::file-patch-hunks patch))))
    (check (equal (grit::partial-patch patch hunk (list (index-of hunk :del "d") (index-of hunk :add "D")) t)
                  (s "diff --git a/f.txt b/f.txt" "--- a/f.txt" "+++ b/f.txt"
                     "@@ -1,5 +1,5 @@" " a" " B" " c" "+d" "-D" " e")))))

(deftest partial-context-only-is-nil
  (let* ((patch (line-fixture-patch))
         (hunk (first (grit::file-patch-hunks patch))))
    (check (null (grit::partial-patch patch hunk (list 0 3 6) nil)))
    (check (null (grit::partial-patch patch hunk '() t)))))

(deftest partial-new-file
  (let* ((patch (first (grit::parse-diff (s "diff --git a/n b/n" "new file mode 100755"
                                            "index 0000000..1111111" "--- /dev/null" "+++ b/n"
                                            "@@ -0,0 +1,3 @@" "+x" "+y" "+z"))))
         (hunk (first (grit::file-patch-hunks patch))))
    (check (equal (grit::partial-patch patch hunk '(1) nil)
                  (s "diff --git a/n b/n" "new file mode 100755" "--- /dev/null" "+++ b/n"
                     "@@ -0,0 +1,1 @@" "+y"))
           "staging part of a new file creates it")
    (check (equal (grit::partial-patch patch hunk '(0 1 2) t)
                  (s "diff --git a/n b/n" "--- a/n" "+++ b/n" "@@ -1,3 +0,0 @@" "-x" "-y" "-z"))
           "unstaging all of a new file empties it")))

(deftest partial-deleted-file
  (let* ((patch (first (grit::parse-diff (s "diff --git a/g b/g" "deleted file mode 100644"
                                            "--- a/g" "+++ /dev/null"
                                            "@@ -1,2 +0,0 @@" "-p" "-q"))))
         (hunk (first (grit::file-patch-hunks patch))))
    (check (equal (grit::partial-patch patch hunk '(0) nil)
                  (s "diff --git a/g b/g" "--- a/g" "+++ b/g" "@@ -1,2 +1,1 @@" "-p" " q")))
    (check (equal (grit::partial-patch patch hunk '(1) t)
                  (s "diff --git a/g b/g" "new file mode 100644" "--- /dev/null" "+++ b/g"
                     "@@ -0,0 +1,1 @@" "+q"))
           "restoring part of a deleted file recreates it")))

(deftest partial-no-newline
  (let* ((patch (first (grit::parse-diff (s "diff --git a/f b/f" "--- a/f" "+++ b/f"
                                            "@@ -1,2 +1,2 @@" " a" "-b"
                                            "\\ No newline at end of file" "+c"
                                            "\\ No newline at end of file"))))
         (hunk (first (grit::file-patch-hunks patch))))
    (check (equal (grit::partial-patch patch hunk '(1 2) nil)
                  (s "diff --git a/f b/f" "--- a/f" "+++ b/f" "@@ -1,2 +1,2 @@" " a" "-b"
                     "\\ No newline at end of file" "+c" "\\ No newline at end of file")))))

(deftest hunk-patch-keeps-header
  (let ((patch (first (grit::parse-diff *two-file-diff*))))
    (check (equal (grit::hunk-patch patch (second (grit::file-patch-hunks patch)))
                  (s "diff --git a/f.txt b/f.txt" "index 1111111..2222222 100644" "--- a/f.txt"
                     "+++ b/f.txt" "@@ -20 +20,2 @@" "-last" "\\ No newline at end of file"
                     "+last" "+more" "\\ No newline at end of file")))))

;;; Commit messages

(deftest commit-template-sections
  (let* ((status (grit::make-status
                  :branch "main" :oid "abc1234"
                  :untracked '("scratch.txt")
                  :unstaged (list (grit::make-change :path "u.rs" :kind :modified))
                  :staged (list (grit::make-change :path "s.rs" :kind :added))))
         (template (grit::commit-template status nil)))
    (check (search "# On branch main" template))
    (check (search "# Changes to be committed:" template))
    (check (search "new file: s.rs" template))
    (check (search "# Changes not staged for commit:" template))
    (check (search "modified: u.rs" template))
    (check (search "#   scratch.txt" template))
    (check (uiop:string-prefix-p (s "previous subject" "" "body")
                                 (grit::commit-template status (format nil "previous subject~%~%body"))))))

(deftest strip-comments
  (check (equal (grit::strip-comments (s "real subject" "" "body line" "# comment" "#  another"))
                (format nil "real subject~%~%body line")))
  (check (equal (grit::strip-comments (s "# only" "# comments")) "")))

;;; Log

(deftest log-parsing
  (let* ((rs (string (code-char #x1e))) (us (string (code-char #x1f)))
         (entries (grit::parse-log
                   (concatenate 'string
                                "aaaaaaaaaa" us "bbbbbbbbbb cccccccccc" us "Ann" us "1700000000" us
                                "merge it" us "HEAD -> refs/heads/main, refs/remotes/origin/main, tag: refs/tags/v1, refs/stash"
                                rs (string #\Newline)
                                "bbbbbbbbbb" us "" us "Bob" us "1600000000" us "root" us "HEAD" rs))))
    (check (= (length entries) 2))
    (check (equal (grit::entry-parents (first entries)) '("bbbbbbbbbb" "cccccccccc")))
    (check (equal (grit::entry-short (first entries)) "aaaaaaa"))
    (check (equal (grit::entry-refs (first entries))
                  '(("HEAD -> main" . :branch) ("origin/main" . :remote) ("v1" . :tag))))
    (check (null (grit::entry-parents (second entries))))
    (check (equal (grit::entry-refs (second entries)) '(("HEAD" . :head))))))

(defun entry (short summary parents &optional refs)
  (grit::make-entry :oid short :short short :parents parents :author "T" :time 0
                    :summary summary :refs refs))

(defun row-text (row)
  (format nil "~{~A~}" (mapcar #'car (car row))))

(deftest graph-linear
  (multiple-value-bind (rows index)
      (grit::build-rows (vector (entry "ccc" "third" '("bbb")) (entry "bbb" "second" '("abc"))
                                (entry "abc" "first" '()))
                        '() (make-hash-table :test 'equal))
    (check (equalp index #(0 1 2)))
    (check (= (length rows) 3))))

(deftest graph-merge
  (multiple-value-bind (rows index)
      (grit::build-rows (vector (entry "dad" "merge" '("bad" "cab")) (entry "bad" "main b" '("ace"))
                                (entry "cab" "side a" '("ace")) (entry "ace" "base" '()))
                        '() (make-hash-table :test 'equal))
    (check (>= (length rows) 6))
    (let ((after-merge (aref rows (1+ (aref index 0))))
          (before-base (aref rows (1- (aref index 3)))))
      (check (null (cdr after-merge)) "a transition row follows the merge")
      (check (find #\\ (row-text after-merge)))
      (check (null (cdr before-base)))
      (check (find #\/ (row-text before-base)))
      (check (not (find #\/ (row-text (aref rows (aref index 3)))))))))

(deftest graph-folding
  (let* ((entries (vector (entry "dad" "merge" '("bad" "cab"))
                          (entry "bad" "main b" '("ace") '(("main" . :branch)))
                          (entry "cab" "side tip" '("ade") '(("side" . :branch)))
                          (entry "ade" "side pre" '("ace"))
                          (entry "ace" "base" '())))
         (reach (grit::build-reach entries)))
    (multiple-value-bind (rows index) (grit::build-rows entries '("cab") reach)
      (check (null (aref index 3)) "side pre hides with its only tip folded")
      (check (search "folded · 1 hidden" (row-text (aref rows (aref index 2)))))
      (check (aref index 4) "base is still reached through main"))
    (let ((view (grit::make-log-view entries :local)))
      (setf (grit::log-view-cursor view) 2)
      (check (grit::log-toggle-fold view))
      (grit::log-move view 1)
      (check (= (grit::log-view-cursor view) 4) "moving skips hidden commits"))))

;;; AI config

(deftest ai-config-parsing
  (check (equal (grit::parse-ai-config (s "# comment" "[other]" "command = \"no\"" "[ai]"
                                          "command = \"claude -p \\\"x\\\"\"  # trailing"
                                          "verify_command = 'make check'" "broken ="))
                '(("command" . "claude -p \"x\"") ("verify_command" . "make check"))))
  (check (grit::conflict-markers-p (sb-ext:string-to-octets (s "a" "<<<<<<< ours" "b"))))
  (check (grit::conflict-markers-p (sb-ext:string-to-octets (s "a" "=======" "b"))))
  (check (not (grit::conflict-markers-p (sb-ext:string-to-octets (s "a" "======== x" "b"))))))

;;; Operations on real repositories

(defun sh (directory &rest command)
  (multiple-value-bind (out ok err) (grit::run command directory)
    (unless ok (error "~{~A~^ ~} failed: ~A" command err))
    out))

(defun call-with-repo (function)
  (let ((dir (format nil "~A/grit-test-~36R"
                     (string-right-trim "/" (uiop:native-namestring (uiop:temporary-directory)))
                     (random (expt 36 8) (make-random-state t)))))
    (ensure-directories-exist (uiop:ensure-directory-pathname dir))
    (unwind-protect
         (progn
           (sh dir "git" "init" "-q" "-b" "main")
           (sh dir "git" "config" "user.name" "Test")
           (sh dir "git" "config" "user.email" "test@example.com")
           (sh dir "git" "config" "commit.gpgsign" "false")
           (funcall function dir (grit::open-repo dir)))
      (uiop:delete-directory-tree (uiop:ensure-directory-pathname dir) :validate t
                                                                       :if-does-not-exist :ignore))))

(defmacro with-repo ((dir repo) &body body)
  `(call-with-repo (lambda (,dir ,repo) (declare (ignorable ,dir ,repo)) ,@body)))

(defun put (dir name text)
  (let ((path (format nil "~A/~A" dir name)))
    (ensure-directories-exist (uiop:parse-native-namestring path))
    (grit::write-file path text)))

(defun commit-all (dir message)
  (sh dir "git" "add" "-A")
  (sh dir "git" "commit" "-q" "-m" message))

(defun index-blob (dir path)
  (sh dir "git" "show" (format nil ":~A" path)))

(defun content (dir path)
  (grit::read-file (format nil "~A/~A" dir path)))

(defun patch (repo side path &rest args)
  (apply #'grit::file-patch repo side path args))

(defun has-add (patch text)
  (some (lambda (hunk) (find-if (lambda (line) (and (eq (grit::diff-line-kind line) :add)
                                                    (string= (grit::diff-line-raw line) text)))
                                (grit::hunk-lines hunk)))
        (grit::file-patch-hunks patch)))

(defun hunk-index (repo side path kind raw &optional (hunk 0))
  (index-of (nth hunk (grit::file-patch-hunks (patch repo side path))) kind raw))

(defun two-hunk-fixture (dir)
  (let ((base (loop for i from 1 to 20 collect (format nil "line~D" i))))
    (put dir "f.txt" (format nil "~{~A~%~}" base))
    (commit-all dir "init")
    (setf (nth 1 base) "LINE2_CHANGED" (nth 17 base) "LINE18_CHANGED")
    (put dir "f.txt" (format nil "~{~A~%~}" base))))

(defun line-fixture (dir)
  (put dir "f.txt" (s "a" "b" "c" "d" "e"))
  (commit-all dir "init")
  (put dir "f.txt" (s "a" "B" "c" "D" "e")))

(defun trim-out (string) (string-trim '(#\Newline) string))

(deftest repo-status-and-untracked-dir
  (with-repo (dir repo)
    (put dir "a.txt" "hello")
    (put dir "pkg/a.txt" "x")
    (put dir "pkg/sub/b.txt" "y")
    (let ((status (grit::load-status repo)))
      (check (equal (grit::status-untracked status) '("a.txt" "pkg/")))
      (check (null (grit::status-oid status))))
    (check (null (patch repo :unstaged "pkg/" :untracked t)) "an untracked directory has no patch")
    (grit::stage-file repo "pkg/")
    (check (equal (mapcar #'grit::change-path (grit::status-staged (grit::load-status repo)))
                  '("pkg/a.txt" "pkg/sub/b.txt")))
    (put dir "junk/x" "x")
    (grit::discard-file repo "junk/")
    (check (not (probe-file (format nil "~A/junk/" dir))))))

(deftest repo-unstage-new-file-unborn
  (with-repo (dir repo)
    (put dir "a.txt" "hello")
    (grit::stage-file repo "a.txt")
    (grit::unstage-file repo "a.txt")
    (let ((status (grit::load-status repo)))
      (check (equal (grit::status-untracked status) '("a.txt")))
      (check (null (grit::status-staged status))))))

(deftest repo-stage-deleted-and-discard
  (with-repo (dir repo)
    (put dir "a.txt" (s "v1"))
    (commit-all dir "init")
    (put dir "a.txt" (s "v2"))
    (grit::discard-file repo "a.txt")
    (check (equal (content dir "a.txt") (s "v1")))
    (delete-file (format nil "~A/a.txt" dir))
    (grit::stage-file repo "a.txt")
    (check (eq (grit::change-kind (first (grit::status-staged (grit::load-status repo)))) :deleted))))

(deftest repo-untracked-patch
  (with-repo (dir repo)
    (put dir "new.txt" (s "fresh" "content"))
    (let ((patch (patch repo :unstaged "new.txt" :untracked t)))
      (check (grit::file-patch-new-p patch))
      (check (has-add patch "content")))
    (grit::stage-hunk repo "new.txt" 0 :untracked t)
    (check (equal (index-blob dir "new.txt") (s "fresh" "content")))))

(deftest repo-stage-part-of-untracked-file
  (with-repo (dir repo)
    (put dir "new.txt" (s "fresh" "content" "more"))
    (grit::stage-lines repo "new.txt" 0 '(1) :untracked t)
    (check (equal (index-blob dir "new.txt") (s "content")))))

(deftest repo-hunks
  (with-repo (dir repo)
    (two-hunk-fixture dir)
    (check (= (length (grit::file-patch-hunks (patch repo :unstaged "f.txt"))) 2))
    (grit::stage-hunk repo "f.txt" 0)
    (let ((unstaged (patch repo :unstaged "f.txt"))
          (staged (patch repo :staged "f.txt")))
      (check (= (length (grit::file-patch-hunks unstaged)) 1))
      (check (has-add unstaged "LINE18_CHANGED"))
      (check (has-add staged "LINE2_CHANGED"))
      (check (not (has-add staged "LINE18_CHANGED"))))
    (grit::stage-hunk repo "f.txt" 0)
    (grit::unstage-hunk repo "f.txt" 0)
    (check (has-add (patch repo :unstaged "f.txt") "LINE2_CHANGED"))
    (check (has-add (patch repo :staged "f.txt") "LINE18_CHANGED"))
    (grit::discard-hunk repo "f.txt" 0)
    (check (search (s "line1" "line2" "line3") (content dir "f.txt")))
    (check (null (patch repo :unstaged "f.txt")))))

(deftest repo-discard-one-of-two-hunks
  (with-repo (dir repo)
    (two-hunk-fixture dir)
    (grit::discard-hunk repo "f.txt" 0)
    (check (search (format nil "~%line2~%") (content dir "f.txt")))
    (check (search "LINE18_CHANGED" (content dir "f.txt")))))

(deftest repo-lines
  (with-repo (dir repo)
    (line-fixture dir)
    (grit::stage-lines repo "f.txt" 0 (list (hunk-index repo :unstaged "f.txt" :del "b")
                                            (hunk-index repo :unstaged "f.txt" :add "B")))
    (check (equal (index-blob dir "f.txt") (s "a" "B" "c" "d" "e")))
    (check (has-add (patch repo :unstaged "f.txt") "D"))
    (check (not (has-add (patch repo :unstaged "f.txt") "B")))))

(deftest repo-stage-only-add-line
  (with-repo (dir repo)
    (line-fixture dir)
    (grit::stage-lines repo "f.txt" 0 (list (hunk-index repo :unstaged "f.txt" :add "B")))
    (check (equal (index-blob dir "f.txt") (s "a" "b" "B" "c" "d" "e")))))

(deftest repo-unstage-and-discard-lines
  (with-repo (dir repo)
    (line-fixture dir)
    (grit::stage-file repo "f.txt")
    (grit::unstage-lines repo "f.txt" 0 (list (hunk-index repo :staged "f.txt" :add "D")
                                              (hunk-index repo :staged "f.txt" :del "d")))
    (check (equal (index-blob dir "f.txt") (s "a" "B" "c" "d" "e")))
    (grit::discard-lines repo "f.txt" 0 (list (hunk-index repo :unstaged "f.txt" :add "D")
                                              (hunk-index repo :unstaged "f.txt" :del "d")))
    (check (equal (content dir "f.txt") (s "a" "B" "c" "d" "e")))))

(deftest repo-no-newline-at-eof
  (with-repo (dir repo)
    (put dir "f" (format nil "a~%b"))
    (commit-all dir "init")
    (put dir "f" (format nil "a~%c"))
    (grit::stage-lines repo "f" 0 (list (hunk-index repo :unstaged "f" :del "b")
                                        (hunk-index repo :unstaged "f" :add "c")))
    (check (equal (index-blob dir "f") (format nil "a~%c")))
    (grit::unstage-lines repo "f" 0 (list (hunk-index repo :staged "f" :del "b")
                                          (hunk-index repo :staged "f" :add "c")))
    (check (equal (index-blob dir "f") (format nil "a~%b")))
    (put dir "f" (format nil "a~%b~%"))
    (grit::stage-lines repo "f" 0 (list (hunk-index repo :unstaged "f" :del "b")
                                        (hunk-index repo :unstaged "f" :add "b")))
    (check (equal (index-blob dir "f") (s "a" "b")) "adding the final newline")))

(deftest repo-binary-and-rename
  (with-repo (dir repo)
    (with-open-file (out (format nil "~A/bin.dat" dir) :direction :output
                                                       :element-type '(unsigned-byte 8))
      (write-sequence #(0 1 2 0 255) out))
    (put dir "old.txt" (s "one" "two" "three"))
    (commit-all dir "init")
    (with-open-file (out (format nil "~A/bin.dat" dir) :direction :output :if-exists :supersede
                                                       :element-type '(unsigned-byte 8))
      (write-sequence #(0 9 9 0 255) out))
    (check (grit::file-patch-binary (patch repo :unstaged "bin.dat")))
    (grit::stage-file repo "bin.dat")
    (check (equal (mapcar #'grit::change-path (grit::status-staged (grit::load-status repo)))
                  '("bin.dat")))
    (sh dir "git" "mv" "old.txt" "new.txt")
    (let ((rename (find "new.txt" (grit::status-staged (grit::load-status repo))
                        :key #'grit::change-path :test #'string=)))
      (check (eq (grit::change-kind rename) :renamed))
      (check (equal (grit::change-old-path rename) "old.txt")))
    (grit::unstage-file repo "new.txt")
    (check (equal (grit::status-untracked (grit::load-status repo)) '("new.txt")))))

(deftest repo-commit-and-amend
  (with-repo (dir repo)
    (put dir "a.txt" (s "hello"))
    (grit::stage-file repo "a.txt")
    (check (= (length (grit::commit repo "initial commit")) 7))
    (check (equal (grit::head-commit-message repo) "initial commit"))
    (check (handler-case (progn (grit::commit repo "again") nil)
             (grit::git-error () t))
           "nothing staged refuses")
    (grit::commit repo "rewritten" :amend t)
    (check (equal (grit::head-commit-message repo) "rewritten"))
    (put dir "b.txt" (s "second"))
    (grit::stage-file repo "b.txt")
    (grit::commit repo "first + b" :amend t)
    (let ((status (grit::load-status repo)))
      (check (null (grit::status-staged status)))
      (check (= (length (grit::status-recent-commits status)) 1))
      (check (equal (grit::status-summary status) "first + b")))))

(defun conflict-fixture (dir)
  (put dir "f.txt" (s "base"))
  (commit-all dir "base")
  (sh dir "git" "switch" "-q" "-c" "feature")
  (put dir "f.txt" (s "feature"))
  (commit-all dir "feature change")
  (sh dir "git" "switch" "-q" "main")
  (put dir "f.txt" (s "base side"))
  (commit-all dir "base change")
  (sh dir "git" "switch" "-q" "feature"))

(deftest repo-rebase-conflict-continue
  (with-repo (dir repo)
    (conflict-fixture dir)
    (check (null (grit::read-rebase-state repo)))
    (grit::rebase-onto repo "main")
    (let* ((status (grit::load-status repo))
           (rebase (grit::status-rebase status)))
      (check rebase)
      (check (and rebase (grit::rebase-state-conflicts rebase)))
      (check (and rebase (= (grit::rebase-state-total rebase) 1)))
      (check (and rebase (equal (grit::rebase-state-head-name rebase) "refs/heads/feature")))
      (check (equal (mapcar #'grit::change-path (grit::status-conflicted status)) '("f.txt"))))
    (check (handler-case (progn (grit::rebase-continue repo) nil)
             (grit::git-error (condition) (search "conflict" (princ-to-string condition)))))
    (put dir "f.txt" (s "resolved"))
    (grit::stage-file repo "f.txt")
    (grit::rebase-continue repo)
    (check (null (grit::read-rebase-state repo)))
    (check (equal (sh dir "git" "show" "HEAD:f.txt") (s "resolved")))))

(deftest repo-rebase-abort
  (with-repo (dir repo)
    (conflict-fixture dir)
    (grit::rebase-onto repo "main")
    (check (grit::read-rebase-state repo))
    (grit::rebase-abort repo)
    (check (null (grit::read-rebase-state repo)))))

(deftest repo-branches-and-log
  (with-repo (dir repo)
    (put dir "a" "1")
    (commit-all dir "one")
    (put dir "a" "2")
    (commit-all dir "two")
    (sh dir "git" "tag" "-a" "v1" "-m" "v1" "HEAD~1")
    (let ((first (trim-out (sh dir "git" "rev-parse" "HEAD~1"))))
      (grit::create-and-checkout-branch repo "topic" first)
      (check (equal (grit::current-branch repo) "topic"))
      (check (equal (grit::status-tag (grit::load-status repo)) "v1"))
      (grit::checkout-branch repo "main")
      (grit::delete-branch repo "topic")
      (check (handler-case (progn (grit::delete-branch repo "main") nil) (grit::git-error () t)))
      (let ((log (grit::load-log repo :local 10)))
        (check (= (length log) 2))
        (check (equal (grit::entry-refs (first log)) '(("HEAD -> main" . :branch))))
        (check (equal (grit::entry-refs (second log)) '(("v1" . :tag)))))
      (grit::reset-mixed repo first)
      (check (equal (content dir "a") "2") "a mixed reset keeps the worktree")
      (check (equal (mapcar #'grit::change-path (grit::status-unstaged (grit::load-status repo)))
                    '("a"))))))

(deftest repo-commit-detail-and-ranges
  (with-repo (dir repo)
    (put dir "a" (s "1"))
    (commit-all dir "one")
    (put dir "a" (s "2"))
    (commit-all dir "two")
    (let* ((log (grit::load-log repo :head 10))
           (root (grit::entry-oid (second log)))
           (head (grit::entry-oid (first log))))
      (let ((detail (grit::commit-detail repo root)))
        (check (null (grit::commit-detail-parents detail)))
        (check (has-add (first (grit::commit-detail-patches detail)) "1") "a root commit is all additions"))
      (check (has-add (first (grit::diff-commit-range repo root head)) "2"))
      (check (has-add (first (grit::diff-vs-head repo root)) "1")))))

(deftest repo-remotes-and-worktrees
  (with-repo (dir repo)
    (put dir "a" "1")
    (commit-all dir "one")
    (let ((remote (format nil "~A/remote.git" dir)))
      (sh dir "git" "init" "-q" "--bare" remote)
      (sh dir "git" "remote" "add" "origin" remote)
      (put dir ".gitignore" (s "remote.git/" "clone/"))
      (commit-all dir "ignore")
      (check (handler-case (progn (grit::push-target-upstream repo) nil) (grit::git-error () t)))
      (let ((spec (grit::push-target-push-remote repo)))
        (check (equal (grit::push-spec-remote spec) "origin"))
        (check (grit::push-spec-set-upstream spec))
        (grit::push-branch repo spec nil))
      (check (equal (grit::push-spec-remote-branch (grit::push-target-upstream repo)) "main"))
      (let ((spec (grit::push-target-elsewhere repo "origin other")))
        (check (equal (grit::push-spec-remote-branch spec) "other")))
      (check (eq (grit::pull repo (grit::push-target-upstream repo)) :up-to-date))
      (let ((clone (format nil "~A/clone" dir)))
        (sh dir "git" "clone" "-q" remote clone)
        (put clone "a" "2")
        (sh clone "git" "-c" "user.name=T" "-c" "user.email=t@t" "commit" "-qam" "two")
        (sh clone "git" "push" "-q")
        (check (eq (grit::pull repo (grit::push-target-upstream repo)) :fast-forwarded))
        (check (equal (content dir "a") "2"))))
    (let ((list (grit::worktrees repo)))
      (check (= (length list) 1))
      (check (grit::worktree-current (first list)))
      (check (grit::worktree-main (first list))))
    (let ((path (format nil "~A-wt" dir)))
      (grit::create-worktree repo path nil)
      (unwind-protect
           (let ((list (grit::worktrees repo)))
             (check (= (length list) 2))
             (check (equal (grit::worktree-branch (second list))
                           (subseq path (1+ (position #\/ path :from-end t))))))
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname path) :validate t)))))

(deftest not-a-repository
  (check (handler-case (progn (grit::open-repo "/") nil)
           (grit::not-a-repository () t))))

(deftest repo-awkward-paths
  (dolist (name '("spå ce.txt" "quo\"te.txt"))
    (with-repo (dir repo)
      (put dir name (s "a" "b" "c"))
      (commit-all dir "init")
      (put dir name (s "a" "B" "c" "d"))
      (check (equal (mapcar #'grit::change-path (grit::status-unstaged (grit::load-status repo)))
                    (list name)))
      (let ((patch (patch repo :unstaged name)))
        (check (equal (grit::file-patch-path patch) name))
        (grit::stage-lines repo name 0 (list (hunk-index repo :unstaged name :add "d")))
        (check (equal (index-blob dir name) (s "a" "b" "c" "d")) name)
        (grit::stage-hunk repo name 0)
        (check (equal (index-blob dir name) (s "a" "B" "c" "d")) name)
        (grit::unstage-hunk repo name 0)
        (check (equal (index-blob dir name) (s "a" "b" "c")) name)))))

(deftest repo-unborn-and-clean
  (with-repo (dir repo)
    (let ((status (grit::load-status repo)))
      (check (null (grit::status-recent-commits status)))
      (check (null (grit::status-tag status)))
      (check (null (grit::status-upstream status)))
      (check (equal (grit::status-branch status) "main")))
    (check (null (grit::head-commit-message repo)))
    (check (null (grit::load-log repo :local 10)))))
