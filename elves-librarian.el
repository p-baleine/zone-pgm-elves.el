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
(require 'f)
(require 'eieio)
(require 's)

(require 'elves-logging)
(require 'elves-reference)

;; TODO: 検索条件をもっとfuzzyにする
;; TODO: 検索結果から検索に用いたファイルに関するエントリは除去する
;; TODO: 拡張子を考慮する、今 clj ファイル開いてるなら clj しか検索しないみたいな

(cl-defgeneric elves-enumerate-referencces (librarian context)
  "Return a list of references that would be searched by
`LIBRARIAN' based on `CONTEXT'."
  (let* ((patterns (elves-librarian--patterns-from context))
         (cmd
          (let ((cmd
                 (elves-librarian-search-cmd-of librarian patterns)))
            ;; NOTE: 自動的に librarian のクラス名をログに含めたいね
            (elves--debug
             "%s will grep by command: `%s'"
             (eieio-object-class-name librarian)
             cmd)
            cmd))
         (output (shell-command-to-string cmd))
         (cwd
         (s-trim
          (shell-command-to-string "git rev-parse --show-toplevel")))
         (reference-class (elves-librarian-reference-class-of librarian)))
    (->> (s-split "\n" output)
         (-remove #'s-blank?)
         (-map (lambda (x) (s-split "\t" x)))
         (--map (make-instance
                 reference-class
                 :repository-url cwd
                 :commit-hash (nth 0 it)
                 :path (nth 1 it)
                 :line-number (string-to-number (nth 2 it))
                 :column (string-to-number (nth 3 it))
                 :matching (nth 4 it))))))

(defclass elves-librarian ()
  ((search-cmd
    :accessor elves-librarian-search-cmd-of)
   (reference-class
    :accessor elves-librarian-reference-class-of
    :initform 'elves-librarian-reference)))

(cl-defmethod elves-librarian-search-cmd-of
  ((_librarian elves-librarian) patterns)
  (elves-librarian--search-cmd patterns))

(defclass elves-librarian@時の回廊 (elves-librarian) ()
  "はい
https://www.youtube.com/watch?v=9ECai7f2Y40")

(cl-defmethod elves-librarian-search-cmd-of
  ((_librarian elves-librarian@時の回廊) patterns)
  (elves-librarian--search-cmd
   patterns
   ;; FIXME: head で絞らないと「zsh:1: 引数リストが長すぎます: git」と怒られる
   ;; なんでだろう？？
   :commit-objects-cmd "git rev-list --all | head -n 10"))

(cl-defmethod elves-librarian-reference-class-of
  ((_librarian elves-librarian@時の回廊))
  'elves-librarian-reference-@時の回廊)

(defun elves-librarian--patterns-from (context)
  (->> (s-split "\n" context)
       (-map #'s-trim)
       (-remove #'s-blank?)
       (-map #'shell-quote-argument)
       (s-join  " --or -e ")))

(cl-defun elves-librarian--search-cmd
    (patterns &key (commit-objects-cmd "git rev-parse HEAD"))
  (let ((search-cmd-prefix
         (s-join
          " "
          '("git --no-pager grep --line-number --column -I"
            "--no-color --full-name --only-matching -e")))
        (search-cmd-postfix
         (s-join
          " "
          `("$("
            ,commit-objects-cmd ;; 危ないなぁ 😱
            ")"
            "--"
            "$(git rev-parse --show-toplevel)"
            ;; FIXME: gawk 必須なのなんとかならないかしら？
            "| gawk -F ':'"
            "'{print $1 \"\t\" $2 \"\t\" $3 \"\t\" $4 \"\t\" $5}'"))))
    (s-join " "
      (list search-cmd-prefix patterns search-cmd-postfix))))

(provide 'elves-librarian)
;;; elves-librarian.el ends here
