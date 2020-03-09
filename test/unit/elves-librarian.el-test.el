;;; elves-librarian.el-test.el --- Tests for elves-librarian.el

;;; Code:

(require 'elves-librarian)

(ert-deftest elves-test-elves-enumerate-references ()
  (let* ((librarian (make-instance 'elves-librarian))
         (default-directory elves-test--example-cpython-path)
         (references (elves-enumerate-referencces
                      librarian
                      "f.printErrorList('ERROR', self.errors)
        self.printErrorList('FAIL', self.failures)

    def printErrorList(self, flavour, errors):
        for test, err in errors:
            self.stream.writeln(self.separator1)
            self.stream.writeln(\"%s: %s\" % (flavour,self.getDescription(test)))
")))
    (should (equal '("Lib/test/support/testresult.py" . 4430)
                   (nth 0 references)))
    (should (equal '("Lib/test/support/testresult.py" . 4481)
                   (nth 1 references)))
    (should (equal '("Lib/unittest/runner.py" . 3484)
                   (nth 7 references)))))

;;; elves-librarian.el-test.el ends here
