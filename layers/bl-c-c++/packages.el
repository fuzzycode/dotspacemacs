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
  '(company rtags (ff-c-style :location local) projectile irony company-irony company-irony-c-headers irony-eldoc smart-tabs-mode)
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

(defun bl-c-c++/init-smart-tabs-mode ()
  (use-package smart-tabs-mode
    :defer t
    :config (progn
              (smart-tabs-insinuate 'c 'c++))))

(defun bl-c-c++/post-init-company ()
  (setq company-idle-delay 0.5)
  (spacemacs|add-company-hook c++-mode)
  (spacemacs|add-company-hook c-mode))

(defun bl-c-c++/post-init-projectile ()
  (with-eval-after-load 'cc-mode
    (define-key c-mode-base-map (kbd "<A-tab>") (function projectile-find-other-file))))

(defun bl-c-c++/init-ff-c-style ()
  (use-package ff-c-style
    :config (ff-add-c-style)))

(defun bl-c-c++/init-irony ()
  (use-package irony
    :defer t
    :init (progn
            (add-hook 'c++-mode-hook 'irony-mode)
            (add-hook 'c-mode-hook 'irony-mode)
            (add-hook 'objc-mode-hook 'irony-mode)

            (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
            (spacemacs|diminish irony-mode " Ⓘ" " I"))))

(defun bl-c-c++/init-company-irony ()
  (use-package company-irony
    :defer t
    :after (irony)
    :init (progn
            (push 'company-irony company-backends-c++-mode)
            (push 'company-irony company-backends-c-mode)
            (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
            )))

(defun bl-c-c++/init-company-irony-c-headers ()
  (use-package company-irony-c-headers
    :defer t
    :after (company-irony)
    :init (progn
            (push 'company-irony-c-headers company-backends-c++-mode)
            (push 'company-irony-c-headers company-backends-c-mode))
    ))

(defun bl-c-c++/init-irony-eldoc ()
  (use-package irony-eldoc
    :defer t
    :after (irony)
    :init (add-hook 'irony-mode-hook 'irony-eldoc)))

(defun bl-c-c++/init-rtags ()
  (use-package rtags
    :defer t
    :init (progn
            (defun rtags/flycheck-rtags-setup ()
              "Configures flycheck with rtags"
              (with-eval-after-load 'flycheck
                (require 'flycheck-rtags)
                (flycheck-select-checker 'rtags))

              (setq-local flycheck-highlighting-mode nil)
              (setq-local flycheck-check-syntax-automatically nil))

            (add-hook 'c-mode-common-hook #'rtags/flycheck-rtags-setup)

            (defun rtags/rtags-c++-hook ()
              (rtags-start-process-unless-running)

              (add-to-list 'spacemacs-jump-handlers-c++-mode 'rtags-find-symbol-at-point)

              (define-key c-mode-base-map (kbd "M-.") (function spacemacs/jump-to-definition))
              (define-key c-mode-base-map (kbd "M-,") (function rtags-find-references-at-point)))
            (add-hook 'c-mode-common-hook #'rtags/rtags-c++-hook)

            (setq rtags-use-helm t)
            (setq rtags-autostart-diagnostics t)
            (add-hook 'rtags-jump-hook 'evil-set-jump)
            )))
