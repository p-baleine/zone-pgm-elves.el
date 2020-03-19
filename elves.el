;;; elves.el --- elves.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Junpei Tajima
;;
;; Author: Junpei Tajima <http://github/p-baleine>
;; Maintainer: Junpei Tajima <p-baleine@gmail.com>
;; Created: March 12, 2020
;; Modified: March 12, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/p-baleine/zone-pgm-elves.el
;;
;; TODO: Package-Requires ちゃんとかけよ
;; Package-Requires: ((cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  elves.el
;;
;;  A Zone Mode where elves will work on behalf of you,
;; like the Doraemon’s gadget “小人ロボット” or
;;“The Elves and the Shoemaker” in Grimm’s Fairy Tales.
;;
;; こういう生産性に一切寄与しない物作っているときは、やる気があるんだよなぁ
;;
;;; Code:

;; FIXME: CI ひっかけろよ、まじで

(require 'dash)
(require 's)
(require 'zone)

(require 'elves-artist)
(require 'elves-chitchat)
(require 'elves-librarian)
(require 'elves-logging)
(require 'elves-scrutinizer)
(require 'elves-utils)

(defvar elves-debugging t)

(when elves-debugging
  (elves--log-set-level 'debug)
  (elves--log-enable-logging))

;; Workshop

(cl-defun elves--get-context (&key (len 300))
  "Return context up to `LEN' length."
  ; FIXME: 範囲外のハンドリングしろよ(テストもかけよ)
  (let* ((end (point))
         (start (- end len)))
    (buffer-substring start end)))

(defun elves--create-draft-buffer (quote)
  "Create a temporary buffer which contain the contents of `QUOTE'."
  (-let* ((draft (get-buffer-create (make-temp-name "*elves-")))
          (start (elves-quote-offset-of quote)))
    (with-current-buffer draft
      (insert-buffer-substring
       (elves-quote-contents-of quote) start))
    draft))

(defun elves--buffer-line-count (buffer)
  "Return number of lines on `BUFFER'."
  (with-current-buffer buffer
    (goto-char (point-min))
    (count-lines (point-min) (point-max))))

;; Zone pgm

(defun elves-sanguine ()
  (elves-pgm :artist (make-instance 'elves-sanguine-artist)))

(defun elves-sanguine@時の回廊 ()
  (elves-pgm
   :artist (make-instance 'elves-sanguine-artist)
   :scrutinizer (make-instance 'elves-probabilistic-scrutinizer)
   :librarian (make-instance 'elves-librarian@時の回廊)))

(defun elves-phlegmatic ()
  (elves-pgm :artist (make-instance 'elves-phlegmatic-artist)))

;; TODO: autoload にして
;; FIXME: 色々、(バッファとか諸々)リークしてるのまじでやめて

(cl-defun elves-pgm
    (&key
     (librarian (make-instance 'elves-librarian))
     (scrutinizer (make-instance 'elves-deterministic-scrutinizer))
     (artist (make-instance 'elves-phlegmatic-artist)))
  "A Zone Mode where elves will work on behalf of you.
Like the Doraemon’s gadget “小人ロボット” or “The Elves and the Shoemaker”
in Grimm's Fairy Tales.

>> 小人ばこ（こびとばこ）は、「小人ロボット」（てんとう虫コミックス第7巻に収録）
>> に登場する。
>>
>> グリム童話「小人の靴屋」をモチーフにした道具。これに仕事を頼んで寝ると、
>> この箱から5、6体の小人型ロボットが現れ、童話の内容と同様に頼んだ人間が
>> 寝ている間に仕事を片付けてくれる。仕事は靴磨きや草むしりといった雑用から、
>> 自動車の故障の修理まで、なんでも可能。ただし頼んだ本人が眠らない限り、
>> ロボットは出てきてくれない。

ドラえもんのひみつ道具 (こ) (Mar. 3, 2020, 11:07 UTC).
In Wikipedia: The Free Encyclopedia.
Retrieved from https://ja.wikipedia.org/wiki/%E3%83%89%E3%83%A9%E3%81%88%E3%82%82%E3%82%93%E3%81%AE%E3%81%B2%E3%81%BF%E3%81%A4%E9%81%93%E5%85%B7_(%E3%81%93)

Format of quotes follows
“RFC 3676 Text/Plain Format and DelSp Parameters”
https://www.ietf.org/rfc/rfc3676.txt"
  (elves--debug
   "Elves will work behalf of you, [%s %s %s]."
   (eieio-object-class-name librarian)
   (eieio-object-class-name scrutinizer)
   (eieio-object-class-name artist))

  (let* ((the-context (elves--get-context))
         (quotes (elves-enumerate-quotes librarian the-context))
         (the-quote
          (elves-scrutinize-quotes scrutinizer quotes))
         (draft-buffer (elves--create-draft-buffer the-quote))
         (window (get-buffer-window)))

    (elves--debug
     "%s found quotes: %s"
     (eieio-object-class-name librarian)
     quotes)

    (elves--debug
     "%s elected quote: %s"
     (eieio-object-class-name scrutinizer)
     the-quote)

    (unwind-protect
        (elves-chitchat-with-chitchat
         (setf (elves-artist-draft-buffer-of artist)
               draft-buffer)
         (while (and (not (input-pending-p))
                     (not (elves-artist-completed? artist)))
           (elves-artist-depict artist)
           ;; FIXME: スクロールで追従するようにする
           ;; (with-selected-window window
           ;;   (when (<= (- (window-end) (point)) 500)
           ;;     ;; FIXME: ここ window-adjuster みたいなオブジェクトにきりだして
           ;;     ;; そもそも毎回実行しているのがやだ、あと色々乱数でぶらして
           ;;     (cl-loop for x from 0 while (< x 10)
           ;;              do (progn
           ;;                   (scroll-up-line 1)
           ;;                   (sit-for 0.2)))))
           ))
      (kill-buffer draft-buffer))))

;; (zone-call #'elves-sanguine@時の回廊)

(provide 'elves)
;;; elves.el ends here
