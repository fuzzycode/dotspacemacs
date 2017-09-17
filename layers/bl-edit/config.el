;;; config.el --- bl-edit layer configuration file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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
(defvar spacemacs/compile-auto-close-time 4
  "The time in seconds that the compile buffer will be closed after a successful compile.")

(defvar bl-edit-use-beacon t
  "Control of the beacon package should be used or not")

(defvar bl-save-project-when-compile t
  "")

(defun bl-edit/manual-save-hook ()
  "Used in `after-save-hook'."
  (when (memq this-command '(save-buffer save-some-buffers))
    (evil-normal-state)))

(add-hook 'after-save-hook 'bl-edit/manual-save-hook)

(add-hook 'semantic-mode-hook 'semantic-decoration-mode)


(defun bl-maybe-save-and-compile ()
  "Depending on the value of bl-save-project-when compile, the project is saved and compiled"
  (interactive)
  (if (projectile-project-p)
      (progn
        (when bl-save-project-when-compile
          (projectile-save-project-buffers))
        (projectile-compile-project))))

;; Theme modifications
(setq-default
 theming-modifications
 '((dracula

    ;; Magit
    (magit-diff-lines-boundary :foreground "#f1fa8c" :background "#f1fa8c" :inherit magit-diff-lines-heading)

    ;; Misc
    (sp-show-pair-match-face :underline t)
    )))
