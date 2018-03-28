;;; packages.el --- bl-edit layer packages file for Spacemacs.
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
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `bl-edit-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `bl-edit/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `bl-edit/pre-init-PACKAGE' and/or
;;   `bl-edit/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst bl-edit-packages
  '(all-the-icons-dired
    beacon
    drag-stuff
    use-package-chords
    goto-last-change
    yatemplate
    (helm-flycheck :toggle (configuration-layer/package-usedp 'helm))
    (helm-describe-modes :toggle (configuration-layer/package-usedp 'helm))
    org-trello
    visual-regexp-steroids
    visual-regexp)
  "The list of Lisp packages required by the bl-edit layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun bl-edit/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :defer t
    :init (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

(defun bl-edit/init-drag-stuff ()
  (use-package drag-stuff
    :defer t
    :disabled t
    :init (drag-stuff-global-mode 1)
    :config (drag-stuff-define-keys)))

(defun bl-edit/init-use-package-chords ()
  (use-package use-package-chords
    :init  (setq key-chord-two-keys-delay 0.15)
    :config (key-chord-mode t)))

(defun bl-edit/init-org-trello ()
  (use-package org-trello
    :defer t
    :init
    ;; org-trello major mode for all .trello files
    (add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))

    ;; add a hook function to check if this is trello file, then activate the org-trello minor mode.
    (add-hook 'org-mode-hook
              (lambda ()
                (let ((filename (buffer-file-name (current-buffer))))
                  (when (and filename (string= "trello" (file-name-extension filename)))
                    (org-trello-mode)))))
    ))

(defun bl-edit/init-beacon ()
  (use-package beacon
    :defer t
    :init (progn
            (spacemacs|add-toggle beacon
              :mode beacon-mode
              :documentation "Enable Beacon."
              :evil-leader "otb")
            (beacon-mode bl-edit-use-beacon)
            (spacemacs|diminish beacon-mode "" ""))))

(defun bl-edit/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :defer t
    :init (progn
            (global-set-key [remap replace-regexp] #'vr/replace)
            (global-set-key [remap query-replace-regexp] #'vr/query-replace)
            (global-set-key [remap isearch-forward] #'vr/isearch-forward)
            (global-set-key [remap isearch-backward] #'vr/isearch-backward)

            (global-set-key (kbd "C-c r") 'vr/replace)
            (global-set-key (kbd "C-c q") 'vr/query-replace)
            (global-set-key (kbd "C-c m") 'vr/mc-mark))))

(defun bl-edit/init-visual-regexp ()
  (use-package visual-regexp
    :defer t))

(defun bl-edit/init-goto-last-change()
  (use-package goto-last-change
    :defer t
    :bind ("C--" . goto-last-change)))

(defun bl-edit/init-yatemplate ()
  "A package for allowing yasnippets as file skeletons for auto-fill "
  (use-package yatemplate
    :defer t
    :init (progn
              (setq yatemplate-dir "~/.spacemacs.d/snippets/templates")
              (setq auto-insert t))))

(defun bl-edit/init-helm-flycheck ()
  (use-package helm-flycheck
    :ensure t
    :defer t
    :init
    (eval-after-load 'flycheck
      (spacemacs/set-leader-keys "eH" 'helm-flycheck))))

(defun bl-edit/init-helm-describe-modes ()
  (use-package helm-describe-modes
    :defer t
    :init (global-set-key [remap spacemacs/describe-mode] #'helm-describe-modes)))
;;; packages.el ends here
