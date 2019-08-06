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
  '(magit-imerge
    magit-todos
    (treemacs-magit :toggle (configuration-layer/package-used-p 'treemacs))
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

(defun bl-git/init-treemacs-magit ()
  (use-package treemacs-magit
    :after treemacs
    :defer t
    :init (require 'treemacs-magit)))


(defun bl-git/init-rigid-tabs ()
  (use-package rigid-tabs
    :defer t
    :hook ((diff-mode-hook . rigid-tabs-diff-align)
           (magit-refresh-buffer-hook . rigid-tabs-diff-align))))

(defun bl-git/init-magit-todos ()
  (use-package magit-todos
    :after magit
    :config
    (setq magit-todos-recursive t
          magit-todos-require-colon nil)
    (custom-set-variables
     '(magit-todos-keywords (list "TODO(Björn Larsson)" "HACK" "FIXME" "XXX" "???")))
    (magit-todos-mode)))

(defun bl-git/init-magit-imerge ()
  (use-package magit-imerge
    :defer t))
;;; packages.el ends here
