;;; packages.el --- bl-edit layer packages file for Spacemacs.
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
  '(all-the-icons
    beacon
    bm
    editorconfig
    drag-stuff
    use-package-chords
    goto-last-change
    yatemplate
    (helm-flycheck :toggle (configuration-layer/package-usedp 'helm))
    (helm-bm :toggle (configuration-layer/package-usedp 'helm))
    (helm-describe-modes :toggle (configuration-layer/package-usedp 'helm))
    org-trello
    visual-regexp-steroids
    visual-regexp
    spaceline-all-the-icons)
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

(defun bl-edit/init-all-the-icons ()
  (use-package all-the-icons
    :defer t))

(defun bl-edit/init-spaceline-all-the-icons ()
  (use-package spaceline-all-the-icons
    :after spaceline
    :defer t
    :config (progn
              (spaceline-all-the-icons-theme)
              (spaceline-all-the-icons--setup-neotree)
              (spaceline-all-the-icons--setup-git-ahead)
              (spaceline-all-the-icons--setup-anzu)

              (setq spaceline-all-the-icons-icon-set-git-stats 'diff-icons)
              (setq spaceline-all-the-icons-icon-set-window-numbering 'square)
              (setq spaceline-all-the-icons-separator-type 'arrow)

              (spaceline-toggle-all-the-icons-buffer-position-on)
              (spaceline-toggle-all-the-icons-which-function-on)
              )))

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
    :if bl-edit-use-beacon
    :init (progn
            (beacon-mode 1)
            (spacemacs|diminish beacon-mode "" ""))))

(defun bl-edit/init-bm ()
  (use-package bm
    :defer t
    :commands (bm-buffer-restore bm-toggle-mouse)
    :bind (("<left-fringe> <S-mouse-1>" . bm-toggle-mouse))
    :init (progn
            ;; Allow cross-buffer 'next'
            (setq bm-cycle-all-buffers t)
            ;; Only show bookmark in fringe
            (setq bm-highlight-style 'bm-highlight-only-fringe)
            ;; save bookmarks
            (setq-default bm-buffer-persistence t)
            ;; where to store persistant files
            (setq bm-repository-file (format "%sbm-repository"
                                             spacemacs-cache-directory))
            (spacemacs|define-transient-state bm
              :title "Visual Bookmarks Transient State"
              :doc "
 Go to bookmark^^^^^^            Toggle^^                  Annotate^^      Global         Other^^
 ──────────────^^^^^^─────     ──────^^────────────   ───────^^────  ─────^^────   ─────^^───
 [_n_/_p_(_N_)] next/previous    [_t_] bookmark at point   [_a_] annotate  [_g_] toggle   [_q_] quit"
              :bindings
              ("q" nil :exit t)
              ;; Go to bookmark
              ("n" bm-next)
              ("N" bm-previous)
              ("p" bm-previous)
              ;; Toggle
              ("t" bm-toggle)
              ;; Annotate
              ("a" bm-bookmark-annotate)
              ("g" ((lambda () (setq bm-cycle-all-buffers (not bm-cycle-all-buffers))
                      (message (format "Global Visual Bookmark state is: %s" (if bm-cycle-all-buffers "On" "Off")))))))

            (evil-leader/set-key
              "ab" 'spacemacs/bm-transient-state/body)
            (advice-add 'spacemacs/bm-transient-state/body
                        :before #'bm-buffer-restore))
    :config (progn
              (bm-load-and-restore)
              ;; Saving bookmarks
              (add-hook 'kill-buffer-hook #'bm-buffer-save)
              ;; Saving the repository to file when on exit.
              ;; kill-buffer-hook is not called when Emacs is killed, so we
              ;; must save all bookmarks first.
              (add-hook 'kill-emacs-hook #'(lambda nil
                                             (bm-buffer-save-all)
                                             (bm-repository-save)))
              ;; Restoring bookmarks
              (add-hook 'find-file-hooks   #'bm-buffer-restore)
              ;; Make sure bookmarks is saved before check-in (and revert-buffer)
              (add-hook 'vc-before-checkin-hook #'bm-buffer-save))))

(defun bl-edit/init-helm-bm ()
  (use-package helm-bm
    :defer t
    :after bm
    :commands (helm-bm)
    :bind (("C-c b" . helm-bm))))

(defun bl-edit/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :defer t
    :after visual-regexp
    :init (progn
            (global-set-key [remap replace-regexp] #'vr/replace)
            (global-set-key [remap query-replace-regexp] #'vr/query-replace)
            (global-set-key [remap isearch-forward] #'vr/isearch-forward)
            (global-set-key [remap isearch-backward] #'vr/isearch-backward))))

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

(defun bl-edit/init-editorconfig ()
  (use-package editorconfig
    :defer t
    :init (progn
             (editorconfig-mode t)
             (spacemacs|diminish editorconfig-mode " EC" " EC"))))

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
