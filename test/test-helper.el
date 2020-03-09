;;; test-helper.el --- Helpers for zone-pgm-elves.el-test.el

(require 'f)
(require 's)

;; https://github.com/rejeep/ert-runner.el/issues/49#issuecomment-518541100
(when (> emacs-major-version 26)
  (defalias 'ert--print-backtrace 'backtrace-to-string))

(defvar elves-test--fixture-text
  (s-join
   "\n"
   '("For God wrote in Lisp code"
     "When he filled the leaves with green."
     "The fractal flowers and recursive roots:"
     "The most lovely hack I've seen."
     "And when I ponder snowflakes,"
     "never finding two the same,"
     "I know God likes a language"
     "with its own four-letter name.")))

(defvar elves-test--example-cpython-path
  (f-join (file-name-directory load-file-name)
          "example-repositories"
          "cpython"))

(defun elves-test--make-fixture-file ()
  "Create a fixture file."
  (make-temp-file "test-" nil nil elves-test--fixture-text))

;;; test-helper.el ends here
