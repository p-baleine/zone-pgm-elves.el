;;; elves-chitchat.el --- Chitchat -*- lexical-binding: t; -*-
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

(require 'ansi-color)
(require 'async)
(require 'eieio)

(require 'elves-colors)
(require 'elves-utils)

(defvar elves-chitchat-thread
  'elves-chitchat-thread-human-declined)
;; (defvar elves-chitchat-thread
;;   'elves-chitchat-thread-got-wrote-in-lisp-code)

(defvar elves-chitchat-interval
  'elves-chitchat-shut-interval)

(defalias '🎨 'elves-colors-apply)

;; TODO: ここらへんもてすとかこう

(cl-iter-defun elves-chitchat-thread-got-wrote-in-lisp-code ()
  ;; 世界は Lisp でできている♪
  (let ((lyrics
         `(
           "♪ For God wrote in Lisp code"
           "♪ When he filled the leaves with green."
           ,(🎨 "♪ When he filled the leaves with green.")
           "♪ The fractal flowers and recursive roots:"
           "♪ The most lovely hack I've seen."
           "♪ And when I ponder snowflakes,"
           "♪ never finding two the same,"
           "♪ I know God likes a language"
           "♪ with its own four-letter name."))
        (idx 0))
    (while t
      (when (>= idx (length lyrics)) (setf idx 0))
      (iter-yield (nth idx lyrics))
      (cl-incf idx))))

(cl-iter-defun elves-chitchat-thread-human-declined ()
  (let ((lyrics
         '(
           ;; FIXME: 「著作権」ってしってます？
           "やつは、ちいさいです　ちびです　かてる？"
           "れーせーといえば、ひとことでいうとこれって"
           "くいずかー　しけんかも"
           "へいきみたいな　でんぱありませぬここ"
           "そこは、きあいで？"
           "からからゆーてます？　　おまけいり？　　あげぞこ？"
           "＼お役立ちー！／　どうかしましたです？"
           "うしろからたべるつもりです？"
           "こどものなまえ、ようせいってかいてふぇありーってつけました？"
           "させたさせた"
           "おこられない？"
           "ぼくら、きいろいちごうとかてんかされてますゆえー"
           "ほんろうされるの、いや？"
           "そのなまなましさはないわー"
           "あにめかちゅうし！ごめーん"
           "やーん、くださいー"
           "いきてるってすてきです？"
           "ぼくらてきには、みずものですが？"
           "きゃー　　こっちにもいたー　　いっぱいいたー　　おなかまー"
           ))
        (smiles
         '(
           "・ヮ・"
           "(・ワ・)"
           "(・ヮ・)"
           "･ワ･"
           "･ヮ･"
           )))
    (while t
      (let ((l (nth (random (length lyrics)) lyrics))
            (s (nth (random (length smiles)) smiles))
            (ts (format-time-string "%H:%M")))
        (iter-yield (concat ts  (🎨 (format "\t<%s>\t" s)) l))))))

(cl-defun elves-chitchat-shut-interval (&key (mean 3.0) (sigma 1.5))
  "Return seconds between chats."
  (elves-sample-from-normd :mean mean :sigma sigma))

;; TODO: 子プロセスからメッセージもらって親で良いかんじに出力したい
;; 今は子プロセスとのメッセージのやりとりの仕方が分からないため
;; display-buffer でお茶を濁している

(defun elves-chitchat--symbol-function (sym)
  "Return the function definition of `SYM'."
  (symbol-function (symbol-value sym)))

(defvar elves-chitchat--script-dir
  (file-name-directory load-file-name))

(cl-defun elves-chit-chat--apply-ansi-colors
    (&optional (beginning (point-min)) (end (point-max)) _len)
  (ansi-color-apply-on-region beginning end))

(cl-defmacro elves-chitchat-with-chitchat (&rest body)
  "Execute `BODY' with chitchat.

Chitchat would be developed on a child process."
  ;; FIXME: 子プロセスちゃんと殺して
  `(let*
     ((proc
       (async-start
        `(lambda ()
          (progn
            (add-to-list 'load-path
                         ,elves-chitchat--script-dir)

            (require 'generator)

            ;; elves-chitchat は elves-chitchat-with-chitchat を
            ;; 定義するために elves-chitchat の 定義を要するため
            ;; require している。
            ;; なんだかこのコードを書いていて頭がとてもメタメタしてきたので
            ;; GEB か、Metamagical Themas 詠みませふ
            (require 'elves-chitchat)

            (require 'elves-utils)

            (let*
              ((thread
                (quote
                 ,(funcall (elves-chitchat--symbol-function
                            'elves-chitchat-thread))))
               (interval
                (quote ,(elves-chitchat--symbol-function
                         'elves-chitchat-interval))))
              ;; TODO: iterator 以外の thread にも対応する
              (while t
                (condition-case _x
                    (message (iter-next thread))
                  (iter-end-of-sequence
                   ;; どうしようもない
                   (user-error
                    "Elves chat thread iterator raise error")))
                (sleep-for (funcall interval)))))))))

     ;; あまりちゃんと理解してないんですが、run-at-time で display-buffer
     ;; を遅延しないと、body が走ってくれない…今度理由を調べる、今日は寝る
     (run-at-time
      "1 sec" nil
      (lambda ()
        (let ((buffer (process-buffer proc)))
          (with-current-buffer buffer
            (elves-chit-chat--apply-ansi-colors)
            (add-hook
             'after-change-functions
             #'elves-chit-chat--apply-ansi-colors
             nil t)
            (display-buffer buffer)))))
     ,@body))

(provide 'elves-chitchat)
;;; elves-chitchat.el ends here
