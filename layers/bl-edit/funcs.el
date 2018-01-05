;;; funcs.el --- bl-edit layer functions file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Björn Larsson <develop@bjornlarsson.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;; Code:

(defun bl-edit/goto-first-empty-line ()
  "Finds the first empty line and moves point to it"
  (interactive)
  (beginning-of-buffer)
  (re-search-forward "^$"))

(defun bl-edit/manual-save-hook ()
  "Used in `after-save-hook' to reset the edit mode to normal mode again."
  (when (memq this-command '(save-buffer save-some-buffers))
    (evil-normal-state)))

(defun bl-edit/maybe-save-and-compile ()
  "Depending on the value of bl-save-project-when compile, the project is saved and compiled"
  (interactive)
  (if (projectile-project-p)
      (progn
        (when bl-edit-save-project-when-compile
          (projectile-save-project-buffers))
        (projectile-compile-project nil nil))))


;; https://www.emacswiki.org/emacs/SortWords
;; http://emacs.stackexchange.com/questions/7548/sorting-words-with-hyphens-e-g-in-a-lisp-mode
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (let ((temp-table (copy-syntax-table text-mode-syntax-table)))
    (with-syntax-table temp-table
      (modify-syntax-entry ?- "w" temp-table)
      (modify-syntax-entry ?_ "w" temp-table)
      (sort-regexp-fields reverse "\\w+" "\\&" beg end))))

;; http://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (run-with-timer bl-edit-compile-auto-close-time nil
                    (lambda (buf)
                      (bury-buffer buf)
                      (switch-to-prev-buffer (get-buffer-window buf) 'kill)
                      (redraw-display))
                    buffer)))

;; http://emacsredux.com/blog/2013/05/30/joining-lines/
(defun bl-edit/top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))
;;; funcs.el ends here
