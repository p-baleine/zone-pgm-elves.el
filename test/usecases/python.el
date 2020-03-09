;;; python-1.el --- description -*- lexical-binding: t; -*-

;;; Code:

'(
  (
   (target-file-path . "cpython/Lib/unittest/runner.py")
   (cursor-point . 3564)
   (expected-range . (3484 . 4064))
   (expected . "
            self.stream.writeln(\"%s: %s\" % (flavour,self.getDescription(test)))

        self.printErrorList('FAIL', self.failures)

    def printErrorList(self, flavor, errors):
        for test, err in errors:
            self.stream.write(self.separator1)
            self.stream.write(f'{flavor}: {self.getDescription(test_
            self.stream.writeln(self.separator2)
            self.stream.writeln(\"%s\" % err)


class TextTestRunner(object):
    \"\"\"A test runner class that displays results in textual form.

    It prints out the names of tests as they are run, errors as they
    occur, and a summary of th")
   (scoc . 43)
   (distance-threshold . 100)
   ))

;;; python-1.el ends here
