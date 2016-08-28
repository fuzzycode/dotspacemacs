;;; packages.el --- bl-c-c++ layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
;; added to `bl-c-c++-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `bl-c-c++/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `bl-c-c++/pre-init-PACKAGE' and/or
;;   `bl-c-c++/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst bl-c-c++-packages
  '(company irony company-irony company-irony-c-headers irony-eldoc)
  "The list of Lisp packages required by the bl-c-c++ layer.

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

(defun bl-c-c++/post-init-company ()
  (setq company-idle-delay 0.7))

(defun bl-c-c++/init-irony ()
  (use-package irony
    :commands (irony-mode)
    :init (progn
              (add-hook 'c++-mode-hook 'irony-mode)
              (add-hook 'c-mode-hook 'irony-mode)
              (add-hook 'objc-mode-hook 'irony-mode)

              (defun irony/irony-mode-hook ()
                (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
                (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))

              (add-hook 'irony-mode-hook 'irony/irony-mode-hook)
              (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
              (spacemacs|diminish irony-mode " Ⓘ" " I"))
    ))

(defun bl-c-c++/init-company-irony ()
  (use-package company-irony
    :commands (company-irony)
    :init
    (progn
      (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
      (push 'company-irony company-backends-c-mode-common))))

(defun bl-c-c++/init-company-irony-c-headers ()
  (use-package company-irony-c-headers
    :commands (company-irony-c-headers)
    :init (push 'company-irony-c-headers company-backends-c-mode-common)))
