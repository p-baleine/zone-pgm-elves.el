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
(require 'elves-quote)

;; TODO: 検索条件をもっとfuzzyにする
;; TODO: 検索結果から検索に用いたファイルに関するエントリは除去する
;; TODO: 拡張子を考慮する、今 clj ファイル開いてるなら clj しか検索しないみたいな

(defclass elves-librarian-keyword-enumerable-mixin ()
  ((context
    :initarg context
    :accessor elves-librarian-keyword-enumerable-context-of)))

(defclass elves-librarian-keyword-enumerable-fuzzily-mixin
  (elves-librarian-keyword-enumerable-mixin)
  ((maxlength
    :initarg maxlength
    :initform 100
    :accessor elves-librarian-keyword-enumerable-length-of)))

;; FIXME: librarianはmixin継承しないで別途クラスつくる
(defclass elves-librarian
  (elves-librarian-keyword-enumerable-mixin)
  ((search-cmd
    :accessor elves-librarian-search-cmd-of)
   (quote-class
    :accessor elves-librarian-quote-class-of
    :initform 'elves-quote-head)))

(defclass elves-librarian@時の回廊 (elves-librarian) ()
  "はい
https://www.youtube.com/watch?v=9ECai7f2Y40")

(cl-defmethod elves-librarian--debug
  ((librarian elves-librarian) msg &rest args)
  (apply #'elves--debug
         (s-join " " (list "Librarian %s," msg))
         (eieio-object-class-name librarian)
         `,@args))

(cl-defgeneric elves-enumerate-quotes (librarian)
  "Return a list of `QUOTE's that would be searched by
`LIBRARIAN'."
  ;; FIXME: TEST 書いてからもう少し綺麗にして
  (cl-assert
   (elves-librarian-keyword-enumerable-context-of librarian))

  (let* ((patterns (elves-librarian-emurate-keywords librarian))
         (cmd
          (let ((cmd
                 (elves-librarian-search-cmd-of librarian patterns)))
            (elves-librarian--debug
             librarian "grep by command: `%s'" cmd)
            cmd))
         (output (shell-command-to-string cmd))
         (cwd
         (s-trim
          (shell-command-to-string "git rev-parse --show-toplevel")))
         (quote-class (elves-librarian-quote-class-of librarian)))
    (->> (s-split "\n" output)
         (-remove #'s-blank?)
         (-map (lambda (x) (s-split "\t" x)))
         (--map (make-instance
                 quote-class
                 :repository-url cwd
                 :commit-hash (nth 0 it)
                 :path (nth 1 it)
                 :line-number (string-to-number (nth 2 it))
                 :column (string-to-number (nth 3 it))
                 :matching (nth 4 it))))))

;; FIXME: contextは引数でもらうようにする
(cl-defgeneric elves-librarian-emurate-keywords (keyword-enumerator)
  (let ((context
         (elves-librarian-keyword-enumerable-context-of
          keyword-enumerator)))
    (->> (s-split "\n" context)
       (-map #'s-trim)
       (-remove #'s-blank?)
       (-map #'shell-quote-argument)
       (s-join  " --or -e "))))

(cl-defmethod elves-librarian-emurate-keywords
  ((keyword-enumerator elves-librarian-keyword-enumerable-fuzzily-mixin))
  (let ((context
         (elves-librarian-keyword-enumerable-context-of
          keyword-enumerator))
        (maxlength
         (elves-librarian-keyword-enumerable-length-of
          keyword-enumerator)))
    (with-temp-buffer
      (insert context)
      (goto-char (point-max))
      (let* ((chars
              (cl-loop
               with len = 0 while (< len maxlength)
               do (backward-char 1)
               for c = (char-after)
               for is-whitespace = (member c '(0 10 32))
               unless is-whitespace
               do (cl-incf len)
               and
               collect c))
             (str (apply #'string (reverse chars)))
             (words (-> str (shell-quote-argument) (s-split-words)))
             (pattern (s-join ".*" `("" ,@words ""))))
        (s-join "" (list "-e \"" pattern "\""))))))

(cl-defmethod elves-librarian-search-cmd-of
  ((_librarian elves-librarian) patterns)
  (elves-librarian--search-cmd patterns))

(cl-defmethod elves-librarian-search-cmd-of
  ((_librarian elves-librarian@時の回廊) patterns)
  (elves-librarian--search-cmd
   patterns
   ;; FIXME: head で絞らないと「zsh:1: 引数リストが長すぎます: git」と怒られる
   ;; なんでだろう？？
   :commit-objects-cmd "git rev-list --all | head -n 10"))

(cl-defmethod elves-librarian-quote-class-of
  ((_librarian elves-librarian@時の回廊))
  'elves-quote)

;; Utilty.

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
