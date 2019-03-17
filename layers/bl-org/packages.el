;;; packages.el --- bl-org layer packages file for Spacemacs.
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
;; added to `bl-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `bl-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `bl-org/pre-init-PACKAGE' and/or
;;   `bl-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst bl-org-packages
  '( demo-it
     org-fancy-priorities
     org-tree-slide
     org-make-toc
     org-projectile
     org-super-agenda)

  "The list of Lisp packages required by the bl-org layer.

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

(defvar bl-org/created-property-string "
  :PROPERTIES:
  :CREATED: %U
  :END:")

(defun bl-org/init-org-tree-slide ()
  "Requirement of demo-it"
  (use-package org-tree-slide
    :after org))

(defun bl-org/init-org-make-toc ()
  (use-package org-make-toc
    :after org))

(defun bl-org/init-demo-it ()
  (use-package demo-it
    :after org))

(defun bl-org/post-init-org-projectile ()
  (with-eval-after-load 'org-projectile
    (setq org-projectile-capture-template (format "%s%s" "* TODO %?" bl-org/created-property-string))
    ))

(defun bl-org/init-org-super-agenda ()
  (use-package org-super-agenda
    :after (org org-agenda)
    :config (org-super-agenda-mode)
    (setq org-super-agenda-groups
          '((:name "Important tasks ":priority "A")))))

(defun bl-org/init-org-fancy-priorities ()
  (use-package org-fancy-priorities
    :after org
    :hook (org-mode . org-fancy-priorities-mode)
    :config (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕"))))

;;; packages.el ends here
