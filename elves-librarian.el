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

(require 'elves-logging)

;; TODO: ろぎんぐろぎんぐろぎんぐろぎんぐ
;; TODO: 検索条件をもっとfuzzyにする
;; TODO: 検索結果から検索に用いたファイルに関するエントリは除去する
;; TODO: 拡張子を考慮する、今 clj ファイル開いてるなら clj しか検索しないみたいな
;; 久々に clojure かきたいな

(cl-defgeneric elves-enumerate-referencces (librarian context)
  "Return a list of reference that would be searched by
LIBRARIAN' based on `CONTEXT'."
  (let* ((patterns (elves-librarian--patterns-from context))
         (cmd (elves-librarian-search-cmd-of librarian patterns))
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
                 :column (string-to-number (nth 3 it)))))))

(defclass elves-librarian ()
  ((search-cmd
    :accessor elves-librarian-search-cmd-of)
   (reference-class
    :accessor elves-librarian-reference-class-of
    :initform 'elves-librarian-reference-local)))

(cl-defmethod elves-librarian-search-cmd-of
  ((librarian elves-librarian) patterns)
  (elves--debug "%s will search via `%s'"
                (eieio-object-class-name librarian)
                (elves-librarian--search-cmd patterns))
  (elves-librarian--search-cmd patterns))

(defclass elves-librarian-@corridors_of_time (elves-librarian) ()
  "はい

https://www.youtube.com/watch?v=9ECai7f2Y40")

(cl-defmethod elves-librarian-search-cmd-of
  ((librarian elves-librarian-@corridors_of_time) patterns)
  ;; FIXME: 多分ここら辺、around とか使えばスッキリできるんじゃない？
  (elves--debug "%s will search via `%s'"
                (eieio-object-class-name librarian)
                (elves-librarian--search-cmd patterns))
  (elves-librarian--search-cmd
   patterns
   ;; FIXME: head で絞らないと「zsh:1: 引数リストが長すぎます: git」と怒られる
   ;; なんでだろう？？
   :commit-objects-cmd "git rev-list --all | head -n 10"))

(cl-defmethod elves-librarian-reference-class-of
  ((librarian elves-librarian-@corridors_of_time))
  'elves-librarian-reference-@corridors_of_time)

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
   (offset
    :initarg :offset
    :accessor elves-librarian-reference-offset-of
    :type number)
   (contents
    :accessor elves-librarian-reference-contents-of)))

(defclass elves-librarian-reference-local
  (elves-librarian-reference) ())

(defclass elves-librarian-reference-@corridors_of_time
  (elves-librarian-reference) ())

(cl-defmethod elves-librarian-reference-offset-of
  ((reference elves-librarian-reference))
  (let ((buffer
         (elves-librarian-reference-contents-of reference))
        (line-number
         (elves-librarian-reference-line-number-of reference))
        (column
         (elves-librarian-reference-column-of reference)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (forward-line (1- line-number))
      ;; FIXME: 行末ではなく検索でヒットした内容の末尾に移動するようにする
      ;; (forward-char column)
      (end-of-line)
      (point))))

(cl-defmethod elves-librarian-reference-contents-of
  ((reference elves-librarian-reference-local))
  (find-file-noselect
   (f-join
    (elves-librarian-reference-repository-url-of reference)
    (elves-librarian-reference-path-of reference))))

(cl-defmethod elves-librarian-reference-contents-of
  ((reference elves-librarian-reference-@corridors_of_time))
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
    (elves--debug
     "Add worktree of %s at %s"
     commit-ish work-dir)
    (shell-command-to-string
     (s-join
      " "
      `("git worktree add --detach"
        ,work-dir ,commit-ish)))
    (find-file-noselect
     (f-join
      work-dir
      (elves-librarian-reference-path-of reference)))))

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
            "--no-color --full-name -e")))
        (search-cmd-postfix
         (s-join
          " "
          `("$("
            ,commit-objects-cmd ;; 危ないなぁ
            ")"
            "--"
            "$(git rev-parse --show-toplevel)"
            "| gawk -F ':' '{print $1 \"\t\" $2 \"\t\" $3 \"\t\" $4}'"))))
    (s-join " "
      (list search-cmd-prefix patterns search-cmd-postfix))))

(provide 'elves-librarian)
;;; elves-librarian.el ends here
