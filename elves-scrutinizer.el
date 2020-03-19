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

(defclass elves-deterministic-scrutinizer ()
  ((n :initarg :n
     :initform 0
     :accessor elves-deterministic-scrutinizer-n-of
     :type number
     :documentation "N-th element will be chosen.")))

(defclass elves-probabilistic-scrutinizer () ())

(cl-defgeneric elves-scrutinize-quotes (scrutinizer quotes)
  "Return a quote by scrutinizing `QUOTE's by using `SCRUTINIZER'.

`QUOTE's is a list of items where an item would be the form
of `(file-path . point)'."
  (unless scrutinizer
    (nth 1 quotes)))

(cl-defmethod elves-scrutinize-quotes
  ((scrutinizer elves-deterministic-scrutinizer) quotes)
  (nth (elves-deterministic-scrutinizer-n-of scrutinizer) quotes))

(cl-defmethod elves-scrutinize-quotes
  ((_scrutinizer elves-probabilistic-scrutinizer) quotes)
  (let* ((n (random (length quotes))))
    (nth n quotes)))

(provide 'elves-scrutinizer)
;;; elves-scrutinizer.el ends here
