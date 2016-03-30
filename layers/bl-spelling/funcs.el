;;; funcs.el ---
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Björn Larsson <bjornlarsson@MacBookPro>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code

;; Taken from
;; https://www.emacswiki.org/emacs/FlySpell
(defun bl-spelling/check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (helm-flyspell-correct)
  )

;; Taken from the flyspell package
;; http://web.mit.edu/Emacs/source/emacs/lisp/textmodes/flyspell.el
(defun bl-spelling/check-previous-highlighted-word (&optional arg)
  "Correct the closer misspelled word.
This function scans a mis-spelled word before the cursor. If it finds one
it proposes replacement for that word. With prefix arg, count that many
misspelled words backwards."
  (interactive)
  (let ((pos1 (point))
        (pos  (point))
        (arg  (if (or (not (numberp arg)) (< arg 1)) 1 arg))
        ov ovs)
    (if (catch 'exit
          (while (and (setq pos (previous-overlay-change pos))
                      (not (= pos pos1)))
            (setq pos1 pos)
            (if (> pos (point-min))
                (progn
                  (setq ovs (overlays-at (1- pos)))
                  (while (consp ovs)
                    (setq ov (car ovs))
                    (setq ovs (cdr ovs))
                    (if (and (flyspell-overlay-p ov)
                             (= 0 (setq arg (1- arg))))
                        (throw 'exit t)))))))
        (save-excursion
          (goto-char pos)
          (helm-flyspell-correct))
      (error "No word to correct before point"))))
