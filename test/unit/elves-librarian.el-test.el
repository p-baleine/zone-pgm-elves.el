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
    (should (equal "Lib/test/support/testresult.py"
                   (elves-quote-path-of (nth 0 references))))
    (should (equal 4431
                   (elves-quote-offset-of (nth 0 references))))
    (should (equal "Lib/test/support/testresult.py"
                   (elves-quote-path-of (nth 1 references))))
    (should (equal 4482
                   (elves-quote-offset-of (nth 1 references))))
    (should (equal "Lib/unittest/runner.py"
                   (elves-quote-path-of (nth 7 references))))
    (should (equal 3485
                   (elves-quote-offset-of (nth 7 references))))

    (should (equal (substring
"'''Test runner and result class for the regression test suite.

'''

import functools
import io
import sys
import time
import traceback
import unittest

import xml.etree.ElementTree as ET

from datetime import datetime

class RegressionTestResult(unittest.TextTestResult):"
                    0 99)
                   (with-current-buffer
                       (elves-quote-contents-of
                        (nth 1 references))
                     (buffer-substring 1 100))))))

;;; elves-librarian.el-test.el ends here
