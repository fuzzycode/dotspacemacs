;;; config.el --- bl-edit layer configuration file for Spacemacs.
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
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

(defvar bl-edit-notify-compile-finished t
  "Control if there should be a compile finished message or not")

(defvar bl-edit-compile-auto-close-time 2
  "The time in seconds that the compile buffer will be closed after a successful compile.")

(defvar bl-edit-save-project-when-compile t
  "Configure if a project is saved before compile or not")

(defvar bl-edit-enable-abbrev-mode nil
  "Configure usage of abbreviation mode")

(defvar bl-edit-dap-mode-hooks '(c-mode-hook c++-mode-hook python-mode-hook)
  "Modes that should configure dap key bindings")

(defvar bl-edit-key-freq-ignore '(self-insert-command
                                  abort-recursive-edit
                                  forward-char
                                  backward-char
                                  previous-line
                                  next-line
                                  smart-backspace
                                  left-char
                                  right-char
                                  save-buffer
                                  forward-word
                                  backward-word
                                  undo-tree-redo
                                  undo-tree-undo
                                  )
  "Functions to ignore when calculating key-frequency")

(add-hook 'after-save-hook 'bl-edit/manual-save-hook)

;; Auto close compile buffer
(add-hook 'compilation-finish-functions 'bl-edit/bury-compile-buffer-if-successful)
(add-hook 'compilation-finish-functions 'bl-edit/maybe-notify-compile-finish)

;; Make sure that point is in the right place for typing when editing commit messages
(add-hook 'find-file-hook (lambda ()
                            (when (string-suffix-p "COMMIT_EDITMSG" buffer-file-name)
                              (bl-edit/goto-first-empty-line t))))

(add-hook 'git-commit-mode-hook (lambda ()
                                  (define-key git-commit-mode-map (kbd "TAB") 'bl-edit/goto-first-empty-line)
                                  ))

;; Make sure that key-chords is disabled in minibuffer
(add-hook 'minibuffer-setup-hook #'bl-edit/disable-key-chord-mode)

(spacemacs|add-toggle delete-selection
  :mode delete-selection-mode
  :documentation "Enable Delete Selection"
  :evil-leader "otd")

;; Enable by default
(delete-selection-mode 1)

(spacemacs|add-toggle decoration-mode
  :if (configuration-layer/package-usedp 'semantic)
  :mode semantic-decoration-mode
  :documentation "Enable Decoration Mode"
  :evil-leader "otD")

(advice-add 'load-theme :after 'bl-edit/remove-mode-line-box)
(advice-add 'load-theme :before 'bl-edit/disable-themes)

(when window-system
  (bl-edit/remove-mode-line-box))

;; Make more room for the file name in the error list
(with-eval-after-load 'flycheck
  (setq flycheck-error-list-format `[("File" 25)
                                    ("Line" 5 flycheck-error-list-entry-< :right-align t)
                                    ("Col" 3 nil :right-align t)
                                    ("Level" 8 flycheck-error-list-entry-level-<)
                                    ("ID" 6 t)
                                    (#("Message (Checker)" 9 16
                                       (face flycheck-error-list-checker-name))
                                     0 t)]))

;; Theme modifications
(setq-default
 theming-modifications
 '((dracula

    ;; Magit
    (magit-diff-lines-boundary :foreground "#f1fa8c" :background "#f1fa8c" :inherit magit-diff-lines-heading)
    (magit-section-secondary-heading :inherit magit-section-heading)

    ;; Misc
    (sp-show-pair-match-face :underline t)

    ;; lsp mode
    (lsp-face-highlight-textual :background "#464752")
    (lsp-ui-peek-header :box "white")
    )

   (solarized-light
    (company-tooltip-selection :foreground "#d33682")

    ;; Highlight doxygen mode
    (highlight-doxygen-comment :background "#eee8d5")

    ;; Misc
    (sp-show-pair-match-face :underline t))

   (solarized-dark
    (company-tooltip-selection :foreground "#d33682")

    ;; Misc
    (sp-show-pair-match-face :underline t)

    ;; lsp mode
    (lsp-ui-peek-header :box "white")

    ;; Highlight doxygen mode
    (highlight-doxygen-comment :background "#073642")
    )
   ))

;; IBuffer
(setq ibuffer-formats '((mark modified read-only locked " "
                              (name 35 35 :left :elide)
                              " "
                              (size 9 -1 :right)
                              " "
                              (mode 16 16 :left :elide)
                              " " filename-and-process)
                        (mark " "
                              (name 16 -1)
                              " " filename)))

;; Helm
(setq helm-buffer-max-length 45)

;; Magit
(setq magit-refs-primary-column-width '(16 . 52))
(setq magit-process-finish-apply-ansi-colors t)
