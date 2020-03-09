;;; elves-utils.el-test.el -- Tests for elves-utils.el

;;; Code:

(require 'elves-utils)

(ert-deftest elves-test-elves--sample-from-normd ()
  (let ((v (elves-sample-from-normd)))
    ;; まぁそりゃぁそうだろうなぁ
    (should (and (< -1.0e+INF v) (< v 1.0e+INF)))))

(ert-deftest elves-test-elves--sample-from-normd-prepared ()
  (let ((prepared (elves-sample-from-normd-prepared :maxsize 10)))
    (cl-loop for x from 0 while (< x 11) do
      (let ((v (funcall prepared)))
        (should (and (< -1.0e+INF v) (< v 1.0e+INF)))))))

;;; elves-utils.el-test.el ends here
