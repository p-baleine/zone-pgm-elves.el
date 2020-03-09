;;; elves.usecases.el-test.el --- Tests for elves.el

;;; Code:

(require 'dash)
(require 'f)
(require 'levenshtein)
(require 's)
(require 'zone)

(require 'elves)

(defvar elves-test--example-dir-path
  (f-join (file-name-directory load-file-name)
          "example-repositories"))

(defvar elves-test--uscases-dir-path
  (f-join (file-name-directory load-file-name)
          "usecases"))

(defvar elves-test--usecase-timeout 40)

(defmacro elves-test-define-usecase (usecase spec)
  "Define an ert test case.

The test case name would be `zone-pgm-elves/test-usecase-<USECASE file name>'.

`SPEC' should define `target-file-path', `cursor-point', `expected-range',
`scoc'(Source Characters of Code) and `distance-threshold'."
  (let* ((prefix "elves-test-usecase-")
         (name (concat prefix (file-name-base usecase)))
         (example-file
          (f-join elves-test--example-dir-path
                  (alist-get 'target-file-path spec)))
         (cwd (file-name-directory example-file))
         (start (alist-get 'cursor-point spec))
         (expected-range (alist-get 'expected-range spec))
         (scoc (alist-get 'scoc spec))
         (distance-threshold (alist-get 'distance-threshold spec))
         (expected (alist-get 'expected spec)))
    `(ert-deftest ,(intern name) ()
       (message (format "Vrifing usecase %s..." ,name))
       (let ((default-directory ,cwd))
         (with-temp-buffer
           (insert-buffer-substring (find-file-noselect ,example-file))
           (goto-char ,start)
           (zone-call
            ;; #'zone-old-fashioned-terminal-zone-elves-pgm
            #'elves-phlegmatic
            ,elves-test--usecase-timeout)
           (let* ((actual (buffer-substring
                           ,(car expected-range)
                           ,(+ (cdr expected-range) scoc)))
                  (distance (levenshtein-distance actual ,expected)))
             (print (format "The destunce of expected and actual strings are %d."
                            distance))
             (should (< distance ,distance-threshold))))))))

(defmacro elves-test-define-usecase-groups ()
  "Define usecases."
  `(progn
     ,@(-flatten-n
        1
        (cl-loop
         for usecase in
         (directory-files elves-test--uscases-dir-path t "\.el$")
         collect
         (let ((specs (with-current-buffer (find-file-noselect usecase)
                        (goto-char (point-min))
                        (cadr (read (current-buffer))))))
           (cl-loop
            for spec in specs
            collect
            `(elves-test-define-usecase
              ,usecase ,spec)))))))

(elves-test-define-usecase-groups)

;;; elves.usecases.el-test.el ends here
