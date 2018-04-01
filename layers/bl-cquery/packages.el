;;; packages.el --- bl-cquery layer packages file for Spacemacs.
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
;; added to `bl-cquery-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `bl-cquery/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `bl-cquery/pre-init-PACKAGE' and/or
;;   `bl-cquery/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst bl-cquery-packages '(cquery
                               company-lsp
                               projectile)
  "The list of Lisp packages required by the bl-cquery layer.

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


;; See also https://github.com/cquery-project/cquery/wiki/Emacs
(defun bl-cquery/init-cquery ()
  (use-package cquery
    :defer t
    :init
    (add-hook 'c-mode-common-hook #'bl-cquery/enable)
    :config (progn
              (dolist (mode c-c++-modes)
                (bl-cquery/define-keys-for-mode mode))
              (bl-cquery/customise-lsp-ui-peek)
              )
    ))

(defun bl-cquery/post-init-company-lsp ()
  (spacemacs|add-company-backends :backends company-lsp
                                  :modes c-mode-common))

(defun bl-cquery/pre-init-projectile ()
  (spacemacs|use-package-add-hook
      :post-init
    (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index")))
;;; packages.el ends here
