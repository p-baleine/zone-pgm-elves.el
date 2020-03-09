;;; elves-scrutinizer.el --- Scrutinizer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Junpei Tajima
;;
;; Author: Junpei Tajima <http://github/p-baleine>
;; Maintainer: Junpei Tajima <p-baleine@gmail.com>
;; Created: March 13, 2020
;; Modified: March 13, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/p-baleine/zone-pgm-elves.el
;; Package-Requires: ((emacs 27.0.60) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Scrutinizer
;;
;;; Code:

(require 'eieio)

(cl-defgeneric elves-scrutinize-references (scrutinizer references)
  "Return a reference by scrutinizing `REFERENCES' by using `SCRUTINIZER'.

`REFERENCES' is a list of items where an item would be the form
of `(file-path . point)'."
  (unless scrutinizer
    (nth 1 references)))

(defclass elves-deterministic-scrutinizer ()
  ((n :initarg :n
     :initform 0
     :accessor elves-deterministic-scrutinizer-n-of
     :type number
     :documentation "N-th element will be chosen.")))

(cl-defmethod elves-scrutinize-references
  ((scrutinizer elves-deterministic-scrutinizer) references)
  (nth (elves-deterministic-scrutinizer-n-of scrutinizer) references))

(defclass elves-probabilistic-scrutinizer () ())

(cl-defmethod elves-scrutinize-references
  ((scrutinizer elves-probabilistic-scrutinizer) references)
  (let* ((n (random (length references))))
    (nth n references)))

(provide 'elves-scrutinizer)
;;; elves-scrutinizer.el ends here
