;;; packages.el --- bl-c-c++ layer packages file for Spacemacs.
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
;; added to `bl-c-c++-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `bl-c-c++/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `bl-c-c++/pre-init-PACKAGE' and/or
;;   `bl-c-c++/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst bl-c-c++-packages
  '(cmake-ide
    company-rtags
    company-qml
    company
    flycheck-rtags
    helm-rtags
    modern-cpp-font-lock
    rtags
    (ff-c-style :location local)
    projectile
    smart-tabs-mode)
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

(defun bl-c-c++/init-company-qml ()
  (use-package company-qml
    :defer t
    :ensure company
    :config (add-to-list 'company-backends 'company-qml)))

(defun bl-c-c++/init-smart-tabs-mode ()
  (use-package smart-tabs-mode
    :defer t
    :config (progn
              (smart-tabs-insinuate 'c 'c++))))

(defun bl-c-c++/post-init-company ()
  (setq company-idle-delay 0.2))

(defun bl-c-c++/post-init-projectile ()
  (with-eval-after-load 'cc-mode
    (define-key c-mode-base-map (kbd "<A-tab>") (function projectile-find-other-file))))

(defun bl-c-c++/init-modern-cpp-font-lock ()
  (use-package modern-cpp-font-lock
    :defer t
    :init
    (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
    :config
    (spacemacs|diminish modern-c++-font-lock-mode)
    ))

(defun bl-c-c++/init-ff-c-style ()
  (use-package ff-c-style
    :defer t
    :disabled t
    :init (with-eval-after-load 'cc-mode (ff-add-c-style))))

(defun bl-c-c++/init-cmake-ide ()
  (use-package cmake-ide
    :defer t
    :init (with-eval-after-load 'rtags
            (cmake-ide-setup))))

(defun bl-c-c++/init-company-rtags ()
  (use-package company-rtags
    :ensure rtags
    :if bl-c-c++-enable-rtags
    :config (push 'company-rtags company-backends-c-mode-common)))

(defun bl-c-c++/init-flycheck-rtags ()
  (use-package flycheck-rtags
    :defer t
    :ensure rtags
    :if bl-c-c++-enable-rtags))

(defun bl-c-c++/init-helm-rtags ()
  (use-package helm-rtags
    :defer t
    :if bl-c-c++-enable-rtags
    :config (setq rtags-use-helm t)))

(defun bl-c-c++/init-rtags ()
  (use-package rtags
    :defer t
    :if bl-c-c++-enable-rtags
    :config (progn
              (setq rtags-autostart-diagnostics t
                    rtags-completions-enabled t
                    rtags-display-result-backend 'helm)

              (add-to-list 'spacemacs-jump-handlers-c++-mode 'rtags-find-symbol-at-point)

              (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)

              (add-hook 'c-mode-hook #'bl-c-c++/flycheck-rtags-setup)
              (add-hook 'c++-mode-hook #'bl-c-c++/flycheck-rtags-setup)
              (add-hook 'objc-mode-hook #'bl-c-c++/flycheck-rtags-setup))))
