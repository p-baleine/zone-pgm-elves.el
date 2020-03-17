;;; elves-logging.el --- elves-logging -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Junpei Tajima
;;
;; Author: Junpei Tajima <http://github/p-baleine>
;; Maintainer: Junpei Tajima <p-baleine@gmail.com>
;; Created: March 17, 2020
;; Modified: March 17, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/p-baleine/elves-logging
;; Package-Requires: ((emacs 27.0.60) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  elves-logging
;;
;;; Code:

(require 'log4e)

(log4e:deflogger
 "elves" "%t [%l] %m" "%H:%M:%S"
 '((fatal . "fatal")
   (error . "error")
   (warn  . "warn")
   (info  . "info")
   (debug . "debug")
   (trace . "trace")))

;; (elves--log-open-log) でログがみれる

(provide 'elves-logging)
;;; elves-logging.el ends here
