;;; elves-reference.el --- References -*- lexical-binding: t; -*-
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

(cl-defgeneric elves-librarian-reference-offset-of (reference)
  (let ((buffer
         (elves-librarian-reference-contents-of reference))
        (line-number
         (elves-librarian-reference-line-number-of reference))
        (column
         (elves-librarian-reference-column-of reference))
        (matching
         (elves-librarian-reference-matching-of reference)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (forward-line (1- line-number))
      (forward-char (+ column (string-width matching)))
      (point))))

(cl-defgeneric elves-librarian-reference-contents-of (reference)
  (find-file-noselect
   (f-join
    (elves-librarian-reference-repository-url-of reference)
    (elves-librarian-reference-path-of reference))))

(defclass elves-librarian-reference ()
  ((repository-url
    :initarg :repository-url
    :accessor elves-librarian-reference-repository-url-of
    :type string)
   (commit-hash
    :initarg :commit-hash
    :accessor elves-librarian-reference-commit-hash-of
    :type string)
   (path
    :initarg :path
    :accessor elves-librarian-reference-path-of
    :type string)
   (line-number
    :initarg :line-number
    :accessor elves-librarian-reference-line-number-of
    :type number)
   (column
    :initarg :column
    :accessor elves-librarian-reference-column-of
    :type number)
   (matching
    :initarg :matching
    :accessor elves-librarian-reference-matching-of
    :type string)))

(defclass elves-librarian-reference-@時の回廊
  (elves-librarian-reference) ())

(cl-defmethod elves-librarian-reference-contents-of
  ((reference elves-librarian-reference-@時の回廊))
  ;; FIXME: ここ二度呼ぶのまじでやめて
  ;; FIXME: 一々 temporary な worktree が projectile に登録されるのやめて
  ;; FIXME: テスト書けよ
  (let* ((commit-ish
          (elves-librarian-reference-commit-hash-of reference))
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
      (elves-librarian-reference-path-of reference)))))

(provide 'elves-reference)
;;; elves-reference.el ends here
