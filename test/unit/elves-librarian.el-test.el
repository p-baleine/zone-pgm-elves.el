;;; elves-librarian.el-test.el --- Tests for elves-librarian.el

;;; Code:

(require 'elves-librarian)

(ert-deftest elves-test-elves-enumerate-quotes ()
  (let* ((the-context "f.printErrorList('ERROR', self.errors)
        self.printErrorList('FAIL', self.failures)

    def printErrorList(self, flavour, errors):
        for test, err in errors:
            self.stream.writeln(self.separator1)
            self.stream.writeln(\"%s: %s\" % (flavour,self.getDescription(test)))
")
         (librarian (make-instance 'elves-librarian 'context the-context))
         (default-directory elves-test--example-cpython-path)
         (quotes (elves-enumerate-quotes librarian)))
    (should (equal "Lib/test/support/testresult.py"
                   (elves-quote-path-of (nth 0 quotes))))
    (should (equal 4431
                   (elves-quote-offset-of (nth 0 quotes))))
    (should (equal "Lib/test/support/testresult.py"
                   (elves-quote-path-of (nth 1 quotes))))
    (should (equal 4482
                   (elves-quote-offset-of (nth 1 quotes))))
    (should (equal "Lib/unittest/runner.py"
                   (elves-quote-path-of (nth 7 quotes))))
    (should (equal 3485
                   (elves-quote-offset-of (nth 7 quotes))))

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
                        (nth 1 quotes))
                     (buffer-substring 1 100))))))

;;; elves-librarian.el-test.el ends here
