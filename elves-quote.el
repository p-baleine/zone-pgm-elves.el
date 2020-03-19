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
    :type string)
   (created-at
    :initarg :created-at
    :initform (format-time-string "%s")
    :accessor elves-quote-created-at-of
    :type string)))

(defclass elves-quote-head (elves-quote) ())

(cl-defgeneric elves-quote-offset-of (quote)
  (let ((buffer
         (elves-quote-contents-of quote))
        (line-number
         (elves-quote-line-number-of quote))
        (column
         (elves-quote-column-of quote))
        (matching
         (elves-quote-matching-of quote)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (forward-line (1- line-number))
      (forward-char (+ column (string-width matching)))
      (point))))

(cl-defgeneric elves-quote-contents-of (quote)
  ;; FIXME: 一々 temporary な worktree が projectile に登録されるのやめて
  ;; 多分一時的んいprojectile-globally-ignored-file-suffixes bind すればょいのかな
  ;; FIXME: テスト書けよ
  ;; FIXME: worktree のクリーンアップ
  ;; TODO: やはりelves-quoteも一段特殊にしたい
  (let* ((commit-ish
          (elves-quote-commit-hash-of quote))
         (dir-name (s-join
                    "."
                    `("zone-pgm-elves"
                      ,(elves-quote-created-at-of quote))))
         (work-dir
          (let* ((path (f-join "/" "tmp" dir-name))
                 (result (ignore-errors (not (mkdir path nil)))))
            (if (not result)
                (elves--debug "Worktree of %s at %s is already created."
                              commit-ish path)
              (progn
                (elves--debug "Add worktree of %s at %s" commit-ish path)
                (shell-command-to-string
                 (s-join
                  " "
                  `("git worktree add --detach"
                    ,path ,commit-ish)))))
            path)))
    (find-file-noselect
     (f-join
      work-dir
      (elves-quote-path-of quote)))))

(cl-defmethod elves-quote-contents-of
  ((quote elves-quote-head))
  (find-file-noselect
   (f-join
    (elves-quote-repository-url-of quote)
    (elves-quote-path-of quote))))

(provide 'elves-quote)
;;; elves-quote.el ends here
