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
  '(editorconfig
    expand-region
    goto-last-change
    yatemplate
    helm-flycheck)
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

(defun bl-edit/init-goto-last-change()
  (use-package goto-last-change
    :defer t
    :bind ("C--" . goto-last-change)))

(defun bl-edit/post-init-expand-region ()
  (global-set-key (kbd "C-+") 'er/expand-region)
  (global-set-key (kbd "M-+") 'er/contract-region))

(defun bl-edit/init-yatemplate ()
  "A package for allowing yasnippets as file skeletons for auto-fill "
  (use-package yatemplate
    :defer t
    :init (progn
              (setq yatemplate-dir "~/.spacemacs.d/snippets/templates")
              (setq auto-insert t)
              )))

(defun bl-edit/init-editorconfig ()
  (use-package editorconfig
    :defer t
    :init (progn
             (editorconfig-mode t)
             (spacemacs|diminish editorconfig-mode " EC" " EC"))))

(defun bl-error/init-helm-flycheck ()
  (use-package helm-flycheck
    :ensure t
    :defer t
    :init
    (eval-after-load 'flycheck
      (spacemacs/set-leader-keys "eH" 'helm-flycheck))
    ))

;;; packages.el ends here
