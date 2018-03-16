;;; config.el --- bl-edit layer configuration file for Spacemacs.
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
(defvar bl-edit-use-beacon t
  "Control of the beacon package should be used or not")

(defvar bl-edit-close-compile-on-success t
  "Control if a compile buffer should be automatically closed on success.")

(defvar bl-edit-compile-auto-close-time 4
  "The time in seconds that the compile buffer will be closed after a successful compile.")

(defvar bl-edit-save-project-when-compile t
  "Configure if a project is saved before compile or not")

(defvar bl-edit-enable-abbrev-mode nil
  "Configure usage of abbreviation mode")

(add-hook 'after-save-hook 'bl-edit/manual-save-hook)

;; Auto close compile buffer
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;; Make sure that point is in the right place for typing when editing commit messages
(add-hook 'find-file-hook (lambda ()
                            (when (string-suffix-p "COMMIT_EDITMSG" buffer-file-name)
                              (bl-edit/goto-first-empty-line t))))

(add-hook 'git-commit-mode-hook (lambda ()
                                  (define-key git-commit-mode-map (kbd "TAB") 'bl-edit/goto-first-empty-line)
                                  ))

;; Make sure that key-chords is disabled in minibuffer
(add-hook 'minibuffer-setup-hook #'disable-key-chord-mode)

;; Theme modifications
(setq-default
 theming-modifications
 '((dracula

    ;; Magit
    (magit-diff-lines-boundary :foreground "#f1fa8c" :background "#f1fa8c" :inherit magit-diff-lines-heading)

    ;; Misc
    (sp-show-pair-match-face :underline t)

    ;; lsp mode
    (lsp-face-highlight-textual :background "#464752")
    )))

;; Define abbreviations
(define-abbrev-table 'global-abbrev-table
  '(
    ("teh" "the")
    ))

(setq default-abbrev-mode bl-edit-enable-abbrev-mode)
