;;; elves-librarian.el --- Librarians -*- lexical-binding: t; -*-
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
(require 'eieio)
(require 's)

;; TODO: historyも検索対象とする、次これやりたい、絶対やってて楽しい
;; TODO: 検索条件をもっとfuzzyにする
;; TODO: 拡張子を考慮する、今 clj ファイル開いてるなら clj しか検索しないみたいな
;; 久々に clojure かきたいな

(defclass elves-librarian-reference ()
  ((repository-url
    :initarg :repository-url
    :accessor elves-librarian-reference-repository-url-of
    :type string)
   (commit-hash
    :initarg :hash
    :accessor elves-librarian-reference-commit-hash-of
    :type string)
   (path
    :initarg :path
    :accessor elves-librarian-reference-path-of
    :type string)
   (offset
    :initarg :offset
    :accessor elves-librarian-reference-offset-of
    :type number)
   (contents
    :accessor elves-librarian-reference-contents-of)))

(defclass elves-librarian-reference-local
  (elves-librarian-reference) ())

(cl-defmethod elves-librarian-reference-contents-of
  ((reference elves-librarian-reference-local))
  (find-file-noselect
   (f-join
    (elves-librarian-reference-repository-url-of reference)
    (elves-librarian-reference-path-of reference))))

(cl-defgeneric elves-enumerate-referencces (librarian context)
  "Return a list of reference that would be searched by
LIBRARIAN' based on `CONTEXT'.

A reference should be the form of `(file-path . point)'."
  (let* ((patterns (elves-librarian--patterns-from context))
         (cmd (elves-librarian--search-cmd patterns))
         (output (shell-command-to-string cmd))
         (cwd
         (s-trim
          (shell-command-to-string "git rev-parse --show-toplevel"))))
    (->> (s-split "\n" output)
         (-remove #'s-blank?)
         (-map (lambda (x) (s-split "\t" x)))
         (--map (make-instance
                 'elves-librarian-reference-local
                 :repository-url cwd
                 :path (nth 0 it)
                 :offset (string-to-number (nth 2 it)))))))

(defclass elves-librarian () ())

(defun elves-librarian--patterns-from (context)
  (->> (s-split "\n" context)
               (-map #'s-trim)
               (-remove #'s-blank?)
               (-map #'shell-quote-argument)
               (s-join  " --or -e ")))

(defun elves-librarian--search-cmd (patterns)
  (let ((search-cmd-prefix
         "git --no-pager grep -n --no-color -I -e")
        (search-cmd-postfix
         ;; バイトオフセットが欲しいので awk にひっかけている
         ;; …エグいなぁー
         (s-join " "
           '("| gawk -F ':' -v P=\"$(git rev-parse --show-toplevel)\""
             "'{\"head -n \"$2\" \"$1\" | wc -c\" | getline p;"
             "\"realpath --relative-to \" P \" \" $1 | getline f;"
             "print f \"\t\" $2 \"\t\" p}'")))
        (cwd
         (s-trim
          (shell-command-to-string "git rev-parse --show-toplevel"))))
    (s-join " "
      (list search-cmd-prefix patterns cwd search-cmd-postfix))))

(provide 'elves-librarian)
;;; elves-librarian.el ends here
