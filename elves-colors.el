;;; elves-colors.el --- 「キャンバスは君のもの〜♪」 -*- lexical-binding: t; -*-
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
;; Package-Requires: ((cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Chitchat
;;
;;; Code:

(defconst elves-colors--colors
  '(
    (:black . 30)
    (:red . 31)
    (:green . 32)
    (:yellow . 33)
    (:blue . 34)
    (:magenta . 35)
    (:cyan . 36)
    (:white . 37)
    (:gray . 90)
    ))

(cl-defun elves-colors-apply (seq &optional style)
  ;; FIXME: 太字とか含めてちゃんと実装してくりゃれ
  ;; というか、こんなん自前で用意する必要あるの？
  (let ((style
         (or style
             (nth (random (length elves-colors--colors))
                  (mapcar #'car elves-colors--colors)))))
  (concat
   "\e["
   (number-to-string (alist-get style elves-colors--colors))
   ;; FIXME: なんか keuword 引数設定できない？？
   ;; (when bold ";1")
   ";1m"
   seq
   "\e[0m")))

(provide 'elves-colors)
;;; elves-chitchat.el ends here
