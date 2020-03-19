;;; elves-quote.el --- Quotes -*- lexical-binding: t; -*-
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
;;  Librarians
;;
;;; Code:

(require 'dash)
(require 'f)
(require 'eieio)
(require 's)

(require 'elves-logging)

(defclass elves-quote ()
  ((repository-url
    :initarg :repository-url
    :accessor elves-quote-repository-url-of
    :type string)
   (commit-hash
    :initarg :commit-hash
    :accessor elves-quote-commit-hash-of
    :type string)
   (path
    :initarg :path
    :accessor elves-quote-path-of
    :type string)
   (line-number
    :initarg :line-number
    :accessor elves-quote-line-number-of
    :type number)
   (column
    :initarg :column
    :accessor elves-quote-column-of
    :type number)
   (matching
    :initarg :matching
    :accessor elves-quote-matching-of
    :type string)))

(defclass elves-quote-head (elves-quote) ())

(cl-defgeneric elves-quote-offset-of (reference)
  (let ((buffer
         (elves-quote-contents-of reference))
        (line-number
         (elves-quote-line-number-of reference))
        (column
         (elves-quote-column-of reference))
        (matching
         (elves-quote-matching-of reference)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (forward-line (1- line-number))
      (forward-char (+ column (string-width matching)))
      (point))))

(cl-defgeneric elves-quote-contents-of (reference)
  ;; FIXME: ここ二度呼ぶのまじでやめて
  ;; FIXME: 一々 temporary な worktree が projectile に登録されるのやめて
  ;; FIXME: テスト書けよ
  (let* ((commit-ish
          (elves-quote-commit-hash-of reference))
         (dir-name (s-join
                    "."
                    `("zone-pgm-elves"
                      ,(format-time-string "%s"))))
         (work-dir
          (let ((path (f-join "/" "tmp" dir-name)))
            (mkdir path t)
            path)))
    (elves--debug "Add worktree of %s at %s" commit-ish work-dir)
    (shell-command-to-string
     (s-join
      " "
      `("git worktree add --detach"
        ,work-dir ,commit-ish)))
    (find-file-noselect
     (f-join
      work-dir
      (elves-quote-path-of reference)))))

(cl-defmethod elves-quote-contents-of
  ((reference elves-quote-head))
  (find-file-noselect
   (f-join
    (elves-quote-repository-url-of reference)
    (elves-quote-path-of reference))))

(provide 'elves-quote)
;;; elves-quote.el ends here
