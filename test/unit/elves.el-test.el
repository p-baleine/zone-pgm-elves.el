;;; elves.el-test.el --- Tests for elves.el

;;; Code:

(require 'elves)
(require 'elves-librarian)
(require 'elves-reference)

(ert-deftest elves-test-elves--get-context ()
  (let* ((file (elves-test--make-fixture-file))
         (buffer (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char 65)
          (should (equal (elves--get-context :len 40)
                         "de\nWhen he filled the leaves with green.")))
      (kill-buffer buffer))))

(ert-deftest elves-test-elves--create-draft-buffer ()
  (let* ((file (elves-test--make-fixture-file))
         (reference
          (make-instance
           'elves-librarian-reference-head
           :repository-url ""
           :path file
           :line-number 2
           :column 37
           :matching "\n"))
         (draft (elves--create-draft-buffer reference))
         (expected (with-current-buffer (find-file-noselect file)
                     (buffer-substring 65 (point-max)))))
    (unwind-protect
        (with-current-buffer draft
          (let ((actual (buffer-substring (point-min) (point-max))))
            (should (equal expected actual))))
      (kill-buffer draft))))

(ert-deftest elves-test-elves--buffer-line-count ()
  (let* ((file (elves-test--make-fixture-file))
         (buffer (find-file-noselect file)))
    (unwind-protect
        (should (equal (elves--buffer-line-count buffer)
                       (1+ (s-count-matches-all
                            "\n"
                            elves-test--fixture-text))))
      (kill-buffer buffer))))

;;; elves.el-test.el ends here
