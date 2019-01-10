;;; packages.el --- bl-git layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Björn Larsson <bjornlarsson@MacBookPro>
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
;; added to `bl-git-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `bl-git/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `bl-git/pre-init-PACKAGE' and/or
;;   `bl-git/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst bl-git-packages
  '(magit
    magit-imerge
    magit-todos
    rigid-tabs)
  "The list of Lisp packages required by the bl-git layer.

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

(defun bl-git/init-rigid-tabs ()
  (use-package rigid-tabs
    :defer t
    :hook ((diff-mode-hook . rigid-tabs-diff-align)
           (magit-refresh-buffer-hook . rigid-tabs-diff-align))))

(defun bl-git/init-magit-todos ()
  (use-package magit-todos
    :after magit
    :config (progn
              (setq magit-todos-recursive t
                    magit-todos-require-colon nil)
              (magit-todos-mode))))

(defun bl-git/init-magit-imerge ()
  (use-package magit-imerge
    :defer t))

(defun bl-git/post-init-magit ()
  (setq git-commit-summary-max-length 50
        git-commit-fill-column 72
        git-commit-turn-on-flyspell t
        git-commit-turn-on-auto-fill t
        magit-wip-merge-branch t)

  (magit-wip-after-save-mode)

  (add-hook 'magit-refresh-buffer-hook #'vc-refresh-state)

  ;; Remove line highlight when in magit mode
  (add-hook 'magit-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (add-to-list 'magit-no-confirm 'trash)
  (add-to-list 'magit-no-confirm 'safe-with-wip)

  (with-eval-after-load 'magit
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules-unpulled-from-upstream
                            'magit-insert-unpulled-from-upstream)
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules-unpulled-from-pushremote
                            'magit-insert-unpulled-from-upstream)
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules-unpushed-to-upstream
                            'magit-insert-unpulled-from-upstream)
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules-unpushed-to-pushremote
                            'magit-insert-unpulled-from-upstream)
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules-overview
                            'magit-insert-unpulled-from-upstream))

  (magit-define-popup-action 'magit-submodule-popup
	  ?r "Recursive Update" 'bl-git/magit-submodule-update-recursive ?u))

;;; packages.el ends here
