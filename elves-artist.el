;;; elves-artist.el --- Artists -*- lexical-binding: t; -*-
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
;;  Artists
;;
;;; Code:

(require 'dash)
(require 'eieio)
(require 's)

(require 'elves-utils)

;; TODO: custom にする
(defconst elves-default-frame-seconds 0.005)
(defconst elves-default-prompt-frame-seconds 0.5)

(cl-defgeneric elves-artist-completed? (artist)
  "Whether `ARTIST' completes depicting?")

(cl-defgeneric elves-artist-depict (artist)
  "Depict by `ARTIST'.")

(defclass elves-artist ()
  ((draft-buffer :initform nil
     :accessor elves-artist-draft-buffer-of)
   (line-idx :initform 0
     :accessor elves-artist-line-idx-of)
   (line-frames :initform nil
     :accessor elves-artist-line-frames-of)))

(cl-defmethod elves-artist-completed? ((_artist elves-artist))
  ;; TODO: ↓これ、嘘じゃない？
  (eobp))

(cl-defmethod elves-artist-depict ((artist elves-artist))
  (cl-assert (not (elves-artist-completed? artist)))
  (unless (elves-artist-line-frames-of artist)
    (end-of-line)
    (insert "\n")
    (let* ((line (elves-artist--buffer-line-content
                  (elves-artist-draft-buffer-of artist)
                  (elves-artist-line-idx-of artist)))
           (frames (elves-artist--line-frames line)))
      (setf (elves-artist-line-frames-of artist) frames)
      (cl-incf (elves-artist-line-idx-of artist))))
  (let* ((frame (pop (elves-artist-line-frames-of artist)))
         (content (elves-artist-frame-content frame))
         (seconds (elves-artist-frame-seconds frame))
         (lbp (line-beginning-position))
         (lep (line-end-position))
         (delay (elves-artist-tweak-spf artist seconds)))
    (setf (buffer-substring lbp lep) content)
    (sit-for delay)))

;; アイスクリーム屋さん
(defclass elves-artist-temperament-phlegmatic-mixin () ())

(defclass elves-artist-temperament-sanguine-mixin ()
  ((sigma :initarg :sigma
     :accessor elves-artist-temperament-sanguine-mixin-sigma-of
     :initform 0.1
     :type number)
   (rg :initform (elves-sample-from-normd-prepared)
     :accessor elves-artist-temperament-sanguine-mixin-rg-of)))

(cl-defgeneric elves-artist-tweak-spf (_artist seconds)
  seconds)

(cl-defmethod elves-artist-tweak-spf
  ((temp elves-artist-temperament-sanguine-mixin) seconds)
  (let ((rg (elves-artist-temperament-sanguine-mixin-rg-of temp))
        (sigma (elves-artist-temperament-sanguine-mixin-sigma-of temp)))
    ;; 正規分布でぶれを作ると負数になることがあるんだよな、
    ;; 分布の選択が間違ってる気がするけれど、頭悪いのでどうればよいのか分かりません
    (abs (+ seconds (* (funcall rg) sigma)))))

(defclass elves-phlegmatic-artist
  (elves-artist elves-artist-temperament-phlegmatic-mixin) ())

(defclass elves-sanguine-artist
  (elves-artist elves-artist-temperament-sanguine-mixin) ())

;; Frames

(cl-defstruct elves-artist-frame
  content
  (seconds elves-default-frame-seconds))

(defconst elves-artist--prompt-frame
  (make-elves-artist-frame
   :content "_"
   :seconds elves-default-prompt-frame-seconds))

(defconst elves-artist--prompt-frame-inverse
  (make-elves-artist-frame
   :content " "
   :seconds elves-default-prompt-frame-seconds))

(defconst elves-artist--prompt-frames
  (list
   elves-artist--prompt-frame
   elves-artist--prompt-frame-inverse
   elves-artist--prompt-frame
   elves-artist--prompt-frame-inverse))

(cl-defun elves-artist--line-frames
    (line &key
          (preceding-frames elves-artist--prompt-frames)
          (prompt-seconds 0.1))
  "Return a list of `elves-artist-frame' of`LINE'.

`PRECEDING-FRAMES' are prepended to result."
  (let ((transient-prompt-frame
         (lambda (x)
           (make-elves-artist-frame
            :content (concat (substring (s-join "" x) 0 -1) "_")
            :seconds prompt-seconds))))
    (-concat
     preceding-frames
     (--> line
          (s-split "" it t)
          (-inits it)
          (-non-nil it)
          (-flatten-n 1
            (-map
             (lambda (x)
               (list
                (funcall transient-prompt-frame x)
                (make-elves-artist-frame :content (s-join "" x))))
             it))))))

;; Utils

(defun elves-artist--buffer-line-content (buffer line)
  "Return contents on `LINE' in `BUFFER'."
  (with-current-buffer buffer
    (goto-char (point-min))
    (forward-line line)
    (buffer-substring (line-beginning-position)
                      (line-end-position))))

(provide 'elves-artist)
;;; elves-artist.el ends here
