;;; packages.el --- bl-edit layer packages file for Spacemacs.
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
  '(ace-isearch
    all-the-icons-dired
    beacon
    beginend
    demo-it
    drag-stuff
    deadgrep
    fancy-narrow
    easy-kill
    helm-swoop
    highlight-doxygen
    keyfreq
    lsp-ui
    lsp-mode
    use-package-chords
    goto-last-change
    (helm-flycheck :toggle (configuration-layer/package-usedp 'helm))
    (helm-describe-modes :toggle (configuration-layer/package-usedp 'helm))
    org-tree-slide
    org-make-toc
    visual-regexp-steroids
    visual-regexp
    projectile
    smartscan
    smart-comment
    smart-backspace
    sort-words)
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

(defun bl-edit/init-beginend ()
  (use-package beginend
    :ensure t
    :diminish beginend-global-mode
    :config (beginend-global-mode 1)))

(defun bl-edit/init-highlight-doxygen ()
  (use-package highlight-doxygen
    :ensure t
    :config (highlight-doxygen-global-mode 1)))

(defun bl-edit/init-deadgrep ()
  (use-package deadgrep
    :defer t
    :commands deadgrep
    :bind ("<f12>" . deadgrep)))

(defun bl-edit/init-easy-kill ()
  (use-package easy-kill
    :ensure t
    :config
    (global-set-key [remap kill-ring-save] #'easy-kill)
    (global-set-key [remap mark-sexp] #'easy-mark)))

(defun bl-edit/init-keyfreq ()
  (use-package keyfreq
    :defer t
    :init (progn
            (spacemacs|add-toggle key-freq
              :status (and
                       (bound-and-true-p keyfreq-mode)
                       (bound-and-true-p keyfreq-autosave-mode))
              :on (progn
                    (keyfreq-mode 1)
                    (keyfreq-autosave-mode 1))
              :on-message (message "Enabling Key Frequency")
              :off (progn
                     (keyfreq-mode -1)
                     (keyfreq-autosave-mode -1))
              :off-message (message "Disabling Key Frequency")
              :documentation "Enable Key Frequency Tracking"
              :evil-leader "otk")

            (spacemacs/toggle-key-freq-on)
            (setq keyfreq-file (concat spacemacs-cache-directory "keyfreq.file")
                  keyfreq-file-lock (concat spacemacs-cache-directory "keyfreq.file.lock")
                  keyfreq-excluded-commands bl-edit-key-freq-ignore
                  ))))

(defun bl-edit/post-init-projectile ()
  (setq projectile-git-command "fd --color never --type file --print0")
  (setq projectile-git-submodule-command nil)
  (setq projectile-enable-caching nil)

  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files "compile_commands.json")
    (add-to-list 'projectile-project-root-files ".ccls")))

(defun bl-edit/init-ace-isearch ()
  (use-package ace-isearch
    :defer t
    :init (global-ace-isearch-mode t)
    :config (setq ace-isearch-jump-delay 0.5
                  ace-isearch-function 'avy-goto-word-1)))

(defun bl-edit/post-init-helm-swoop ()
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line))

(defun bl-edit/init-fancy-narrow ()
  "Requirement of demo-it"
  (use-package fancy-narrow
    :defer t))

(defun bl-edit/init-org-tree-slide ()
  "Requirement of demo-it"
  (use-package org-tree-slide
    :defer t))

(defun bl-edit/init-org-make-toc ()
  (use-package org-make-toc
    :defer t
    :hook org-mode))

(defun bl-edit/init-demo-it ()
  (use-package demo-it
    :defer t))

(defun bl-edit/init-sort-words ()
  (use-package sort-words
    :defer t))

(defun bl-edit/post-init-lsp-ui ()
  (spacemacs|add-toggle lsp-ui
    :mode lsp-ui-mode
    :documentation "Enable lsp-ui."
    :evil-leader "otu"))

(defun bl-edit/post-init-lsp-mode ()
  ;; Disable eldoc
  (setq lsp-eldoc-enable-signature-help nil)
  (setq lsp-eldoc-enable-hover nil))

(defun bl-edit/init-smart-backspace ()
  (use-package smart-backspace
    :init (progn
            (global-set-key  [remap backward-delete-char-untabify] 'smart-backspace))))

(defun bl-edit/init-smart-comment ()
  (use-package smart-comment
    :init (global-set-key [remap comment-dwim] 'smart-comment)))

(defun bl-edit/init-smartscan ()
  (use-package smartscan
    :defer t
    :init (global-smartscan-mode 1)))

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

(defun bl-edit/init-beacon ()
  (use-package beacon
    :defer t
    :init (progn
            (spacemacs|add-toggle beacon
              :mode beacon-mode
              :documentation "Enable Beacon."
              :evil-leader "otb")
            (beacon-mode bl-edit-use-beacon)
            (spacemacs|diminish beacon-mode "" "")

            (setq beacon-dont-blink-major-modes '(eshell-mode
                                                  spacemacs-buffer-mode
                                                  term-mode)))))

(defun bl-edit/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :defer t
    :init (progn
            (global-set-key [remap replace-regexp] #'vr/replace)
            (global-set-key [remap query-replace-regexp] #'vr/query-replace)
            ;(global-set-key [remap isearch-forward] #'vr/isearch-forward)
            ;(global-set-key [remap isearch-backward] #'vr/isearch-backward)

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

(defun bl-edit/init-helm-flycheck ()
  (use-package helm-flycheck
    :defer t
    :init
    (eval-after-load 'flycheck
      (spacemacs/set-leader-keys "eH" 'helm-flycheck))))

(defun bl-edit/init-helm-describe-modes ()
  (use-package helm-describe-modes
    :defer t
    :init (global-set-key [remap spacemacs/describe-mode] #'helm-describe-modes)))
;;; packages.el ends here
