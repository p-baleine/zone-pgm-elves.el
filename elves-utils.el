;;; elves-utils.el --- Utils -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Junpei Tajima
;;
;; Author: Junpei Tajima <http://github/p-baleine>
;; Maintainer: Junpei Tajima <p-baleine@gmail.com>
;; Created: March 14, 2020
;; Modified: March 14, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/p-baleine/zone-pgm-elves.el
;; Package-Requires: ((emacs 27.0.60) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Utils
;;
;;; Code:

(cl-defun elves-sample-from-normd (&key (mean 0.0) (sigma 1.0))
  "Draw random samples from N(`MEAN', `SIGMA').

https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
https://stackoverrun.com/ja/q/1441816"
  ;; FIXME: 実装が中途半端じゃないかね？
  (let* ((rand (lambda () (/ (random 1000) 1000.0)))
         (theta (* 2 pi (funcall rand)))
         (rho (sqrt (* -2 (log (- 1 (funcall rand))))))
         (scale (* sigma rho)))
    (+ mean (* scale (cos theta)))))

(cl-defun elves-sample-from-normd-prepared
    (&key (mean 0.0) (sigma 1.0) (maxsize 128))
  (let (samples)
    (lambda ()
      (when (not samples)
        (setf samples
          (cl-loop for x from 0 while (< x maxsize) collect
                   (elves-sample-from-normd
                    :mean mean :sigma sigma))))
      (pop samples))))

(provide 'elves-utils)
;;; elves-utils.el ends here
