;;; packages.el --- bl-c-c++ layer packages file for Spacemacs.
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
;; added to `bl-c-c++-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `bl-c-c++/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `bl-c-c++/pre-init-PACKAGE' and/or
;;   `bl-c-c++/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst bl-c-c++-packages
  '(company-qml
    company
    function-args
    (flycheck-clang-analyzer :location (recipe :fetcher github :repo "fuzzycode/flycheck-clang-analyzer"))
    (flycheck-clang-tidy :location (recipe :fetcher github :repo "fuzzycode/flycheck-clang-tidy"))
    (flycheck-rats :location (recipe :fetcher github :repo "fuzzycode/flycheck-rats"))
    flycheck-flawfinder
    modern-cpp-font-lock
    (qmake-mode :location (recipe :fetcher github :repo "fuzzycode/qmake-mode"))
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

(defun bl-c-c++/init-flycheck-clang-analyzer ()
  "Disabled for now. clang-tidy will report the same issues"
  (use-package flycheck-clang-analyzer
    :disabled t
    :after flycheck
    :config (flycheck-clang-analyzer-setup)))

(defun bl-c-c++/init-flycheck-clang-tidy ()
  (use-package flycheck-clang-tidy
    :after lsp-ui-flycheck
    :config (progn
              (flycheck-clang-tidy-setup)
              (flycheck-add-next-checker 'lsp-ui '(t . c/c++-clang-tidy))
              (flycheck-add-next-checker 'c/c++-clang-tidy '(t . c/c++-cppcheck)))))

(defun bl-c-c++/init-flycheck-rats ()
  "Disabled. flawfinder finds similar issues."
  (use-package flycheck-rats
    :disabled t
    :after flycheck-clang-tidy
    :config (progn
              (flycheck-rats-setup)
	            (flycheck-add-next-checker 'c/c++-cppcheck '(t . rats)))))

(defun bl-c-c++/init-flycheck-flawfinder ()
  (use-package flycheck-flawfinder
    :after flycheck-clang-tidy
    :config (progn
	            (flycheck-flawfinder-setup)
	            (flycheck-add-next-checker 'c/c++-cppcheck '(t . flawfinder)))))

(defun bl-c-c++/init-function-args ()
  (use-package function-args
    :defer t
    :init (set-default 'semantic-case-fold t)
    :config (fa-config-default)))

(defun bl-c-c++/init-qmake-mode ()
  (use-package qmake-mode
    :defer t
    :mode ("\\.pr\\(i\\|o\\|f\\)\\'" . qmake-mode)))

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
  (with-eval-after-load 'projectile
    (when c-c++-adopt-subprojects
      ;; https://www.bountysource.com/issues/11208594-use-only-topmost-git-as-project
      (cl-nsubstitute ".git/" ".git" projectile-project-root-files-bottom-up :test #'string=)))

  (with-eval-after-load 'cc-mode
    (define-key c-mode-base-map (kbd "<A-tab>") (function projectile-find-other-file))
    (define-key c-mode-base-map (kbd "<C-tab>") (function projectile-find-other-file-other-window))))

(defun bl-c-c++/init-modern-cpp-font-lock ()
  (use-package modern-cpp-font-lock
    :defer t
    :init (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
    :config (spacemacs|diminish modern-c++-font-lock-mode)))

(defun bl-c-c++/init-ff-c-style ()
  (use-package ff-c-style
    :defer t
    :commands (ff-add-c-style)
    :init (with-eval-after-load 'cc-mode (ff-add-c-style))))
