;;; elves-artist.el-test.el --- Tests for elves.el

;;; Code:

(require 'dash)

(require 'elves-artist)

(ert-deftest elves-test-elves--buffer-line-content ()
  (let* ((file (elves-test--make-fixture-file))
         (buffer (find-file-noselect file)))
    (unwind-protect
        (should (equal (elves-artist--buffer-line-content buffer 3)
                       "The most lovely hack I've seen."))
      (kill-buffer))))

(ert-deftest elves-test--elves--line-frames ()
  (should
   (equal
    (elves-artist--line-frames "present day")
    (-concat
     elves-artist--prompt-frames
     (list
      (make-elves-artist-frame :content "_" :seconds 0.1)
      (make-elves-artist-frame :content "p")
      (make-elves-artist-frame :content "p_" :seconds 0.1)
      (make-elves-artist-frame :content "pr")
      (make-elves-artist-frame :content "pr_" :seconds 0.1)
      (make-elves-artist-frame :content "pre")
      (make-elves-artist-frame :content "pre_" :seconds 0.1)
      (make-elves-artist-frame :content "pres")
      (make-elves-artist-frame :content "pres_" :seconds 0.1)
      (make-elves-artist-frame :content "prese")
      (make-elves-artist-frame :content "prese_" :seconds 0.1)
      (make-elves-artist-frame :content "presen")
      (make-elves-artist-frame :content "presen_" :seconds 0.1)
      (make-elves-artist-frame :content "present")
      (make-elves-artist-frame :content "present_" :seconds 0.1)
      (make-elves-artist-frame :content "present ")
      (make-elves-artist-frame :content "present _" :seconds 0.1)
      (make-elves-artist-frame :content "present d")
      (make-elves-artist-frame :content "present d_" :seconds 0.1)
      (make-elves-artist-frame :content "present da")
      (make-elves-artist-frame :content "present da_" :seconds 0.1)
      (make-elves-artist-frame :content "present day"))))))

;;; elves-artist.el-test.el ends here
