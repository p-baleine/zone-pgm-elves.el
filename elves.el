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
;; Package-Requires: ((cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  elves.el
;;
;;; Code:

(require 'dash)
(require 's)

(require 'elves-artist)
(require 'elves-librarian)
(require 'elves-scrutinizer)
(require 'elves-utils)

;; Workshop

(cl-defun elves--get-context (&key (len 300))
  "Return context up to `LEN' length."
  ; TODO: 範囲外のハンドリング(テストも)
  (let* ((end (point))
         (start (- end len)))
    (buffer-substring start end)))

(defun elves--create-draft-buffer (file-loc)
  "Create a temporary buffer which contain the contents of `FILE-LOC'.

`FILE-LOC' should be the form of `(file-path . point)'."
  (-let* ((draft (get-buffer-create (make-temp-name "*elves-")))
          ((file . start) file-loc))
    (with-current-buffer draft
      (insert-buffer-substring
       (find-file-noselect file) start))
    draft))

(defun elves--buffer-line-count (buffer)
  "Return number of lines on `BUFFER'."
  (with-current-buffer buffer
    (goto-char (point-min))
    (count-lines (point-min) (point-max))))

;; Chatter

;; TODO: Wikiquote からひろってきたい
;; https://wiki.archlinux.jp/index.php/Fortune
;; steiner 先生とかね: https://en.wikiquote.org/wiki/Rudolf_Steiner
(cl-defun elves--chatter-fortune-cookie-:D
    (&key (max-of-dice 32) (quote-limit 120))
  (let* ((fn
          (lambda ()
            (let*
                ((n (shell-command-to-string "rig | head -n 1"))
                 (name (s-snake-case (s-trim n)))
                 (q (s-truncate
                     quote-limit
                     (s-trim (shell-command-to-string "fortune")))))
              (format "@%s: %s" name q))))
         (noop (lambda () ""))
         (chat (if (and
                    ;; apt install fortune
                    (executable-find "fortune")
                    ;; apt install rig
                    (executable-find "rig"))
                   fn noop))
         (dice (lambda () (+ (random max-of-dice) 1))))
    (lambda ()
      (when (= (funcall dice) 1)
        (funcall chat)))))

;; Zone pgm
;; (zone-call #'elves-sanguine)

(defun elves-sanguine ()
  (elves-pgm :artist (make-instance 'elves-sanguine-artist)))

(defun elves-phlegmatic ()
  (elves-pgm :artist (make-instance 'elves-phlegmatic-artist)))

(cl-defun elves-pgm
    (&key
     (librarian (make-instance 'elves-librarian))
     (scrutinizer (make-instance 'elves-deterministic-scrutinizer))
     (artist (make-instance 'elves-phlegmatic-artist))
     (chat (elves--chatter-fortune-cookie-:D)))
  "A Zone Mode where elves will work on behalf of you.
Like the Doraemon’s gadget “小人ロボット” or “The Elves and the Shoemaker”
in Grimm's Fairy Tales. `KEYSTROKE-STD'"
  (let* ((context (elves--get-context))
         (references
          (elves-enumerate-referencces librarian context))
         (reference-loc
          (elves-scrutinize-references scrutinizer references))
         (draft-buffer (elves--create-draft-buffer reference-loc))
         (line-count (elves--buffer-line-count draft-buffer)))
    (unwind-protect
        (progn
          (setf (elves-artist-elves-artist-draft-buffer-of artist)
                draft-buffer)
          (while (and (not (input-pending-p))
                      (not (elves-artist-completed? artist)))
            (elves-artist-depict artist)
            ;; TODO: ここなんとかならん？クロージャ使うのやめようよ
            (let ((c (funcall chat)))
              (when c (message c)))))
      (kill-buffer draft-buffer))))

(provide 'elves)
;;; elves.el ends here
