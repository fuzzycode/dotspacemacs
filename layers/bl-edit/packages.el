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
    bm
    comment-dwim-2
    dap-mode
    drag-stuff
    deadgrep
    fancy-narrow
    easy-kill
    helm-swoop
    helpful
    highlight-doxygen
    keyfreq
    lsp-ui
    lsp-origami
    lsp-mode
    use-package-chords
    goto-last-change
    (helm-flycheck :toggle (configuration-layer/package-usedp 'helm))
    (helm-describe-modes :toggle (configuration-layer/package-usedp 'helm))
    visual-regexp-steroids
    visual-regexp
    projectile
    proced
    smartscan
    smart-backspace
    sort-words
    spaceline-all-the-icons
    doom-modeline
    )
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

(defun bl-edit/pre-init-doom-modeline ()
  (spacemacs|use-package-add-hook doom-modeline
    :post-init (progn
                 (when  (eq dotspacemacs-mode-line-theme 'doom)
                   (setq doom-modeline-buffer-file-name-style 'relative-from-project
                         doom-modeline-env-version t
                         doom-modeline-mu4e t
                         doom-modeline-checker-simple-format nil
                         doom-modeline-vcs-max-length 25)))))

(defun bl-edit/pre-init-spaceline-all-the-icons ()
  (spacemacs|use-package-add-hook spaceline-all-the-icons
    :post-init (progn
                 (when  (eq dotspacemacs-mode-line-theme 'all-the-icons)
                   ;; Turn on segments
                   (spaceline-toggle-all-the-icons-buffer-position-on)
                   (spaceline-toggle-all-the-icons-hud-on)
                   (spaceline-toggle-all-the-icons-package-updates-on)

                   ;; Add more segments
                   (spaceline-all-the-icons--setup-git-ahead)
                   (spaceline-all-the-icons--setup-paradox)

                   ;; Define the LSP segment
                   (spaceline-define-segment all-the-icons-lsp
                                             (let* ((workspaces (lsp-workspaces))
                                                    (face (if workspaces 'success 'warning)))

                                               (propertize (all-the-icons-faicon "rocket" :v-adjust -0.0575)
                                                           'face face
                                                           'help-echo
                                                           (if workspaces (concat "LSP Connected "
                                                                                  (string-join (--map (format "[%s]\n" (lsp--workspace-print it))
                                                                                                      workspaces)))
                                                             "lsp disconnected")))

                                             :when (and active (bound-and-true-p lsp-mode)))

                   (spaceline-all-the-icons-theme 'all-the-icons-lsp)
                   ))))

(defun bl-edit/pre-init-bm ()
  (spacemacs|use-package-add-hook bm
    :post-init (progn
                   (setq bm-marker 'bm-marker-left
                         bm-recenter t
                         bm-highlight-style 'bm-highlight-only-fringe)

                   (global-set-key (kbd "<f10>") 'bm-toggle)
                   (global-set-key (kbd "<M-f10>")   'bm-next)
                   (global-set-key (kbd "<A-f10>") 'bm-previous))))

(defun bl-edit/init-lsp-origami ()
  (use-package lsp-origami
    :defer t
    :hook (origami-mode . lsp-origami-mode)))

(defun bl-edit/pre-init-dap-mode ()
  (spacemacs|use-package-add-hook dap-mode
    :post-config
    (progn (require 'dap-gdb-lldb)
           (dolist (hook bl-edit-dap-mode-hooks)
             (add-hook hook
                       (lambda ()
                         (dap-mode 1)
                         (dap-ui-mode 1)))))))

(defun bl-edit/init-helpful ()
  (use-package helpful
    :defer 5
    :bind (([remap describe-variable] . helpful-variable)
           ([remap describe-key] . helpful-key)
           ([remap describe-function] . helpful-callable))

    ; TODO(Björn Larsson): Find a way to not hard-code the keys
    :config (spacemacs/set-leader-keys "hdf" 'helpful-callable)
    (spacemacs/set-leader-keys "hdk" 'helpful-key)
    (spacemacs/set-leader-keys "hdv" 'helpful-variable)))


(defun bl-edit/init-proced ()
  (use-package proced
    :defer t
    :config
    (progn
      (setq proced-auto-update-interval 1)
      (add-hook 'proced-mode-hook (lambda () (proced-toggle-auto-update +1))))))

(defun bl-edit/init-comment-dwim-2 ()
  (use-package comment-dwim-2
    :defer t
    :bind ([remap comment-dwim] . comment-dwim-2)))

(defun bl-edit/init-beginend ()
  (use-package beginend
    :defer 5
    :ensure t
    :diminish beginend-global-mode
    :config (beginend-global-mode 1)))

(defun bl-edit/init-highlight-doxygen ()
  (use-package highlight-doxygen
    :defer 6
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
  (setq projectile-keymap-prefix (kbd "C-x p"))

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

(defun bl-edit/init-sort-words ()
  (use-package sort-words
    :defer t))

(defun bl-edit/post-init-lsp-ui ()
  (setq lsp-ui-sideline-show-hover t)

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
    :defer 2
    :init (progn
            (global-set-key  [remap backward-delete-char-untabify] 'smart-backspace))))

(defun bl-edit/init-smartscan ()
  (use-package smartscan
    :defer 10
    :init (global-smartscan-mode 1)))

(defun bl-edit/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :defer t
    :hook (dired-mode . all-the-icons-dired-mode)))

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
    :defer 5
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
    :after visual-regexp
    :init (progn
            (global-set-key [remap replace-regexp] #'vr/replace)
            (global-set-key [remap query-replace-regexp] #'vr/query-replace)

            (global-set-key (kbd "C-c r") 'vr/replace)
            (global-set-key (kbd "C-c q") 'vr/query-replace)
            (global-set-key (kbd "C-c m") 'vr/mc-mark))))

(defun bl-edit/init-visual-regexp ()
  (use-package visual-regexp
    :defer 10))

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
