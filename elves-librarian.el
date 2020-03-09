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

;; TODO: gitとかのコマンドを抽象化する
;; TODO: historyも検索対象とする
;; TODO: 検索条件をもっとfuzzyにする
;; TODO: 拡張子を考慮する

(cl-defgeneric elves-enumerate-referencces (librarian context)
  "Return a list of reference that would be searched by
LIBRARIAN' based on `CONTEXT'.

A reference should be the form of `(file-path . point)'."
  (let* ((patterns (elves-librarian--patterns-from context))
         (cmd (elves-librarian--search-cmd patterns))
         (output (shell-command-to-string cmd)))
    (->> (s-split "\n" output)
         (-remove #'s-blank?)
         (-map (lambda (x) (s-split "\t" x)))
         (--map (cons (nth 0 it) (string-to-number (nth 2 it)))))))

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
         (s-join " "
           '("| gawk -F ':' '{\"head -n \"$2\" \"$1\""
             "| wc -c\" | getline p; print $1 \"\t\" $2 \"\t\" p}'")))
        (cwd
         (s-trim
          (shell-command-to-string "git rev-parse --show-toplevel"))))
    (s-join " "
      (list search-cmd-prefix patterns cwd search-cmd-postfix))))

(provide 'elves-librarian)
;;; elves-librarian.el ends here
