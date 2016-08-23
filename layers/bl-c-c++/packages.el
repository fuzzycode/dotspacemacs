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
  '(cmake-ide rtags company)
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

(defun bl-c-c++/init-rtags ()
  (use-package rtags
    :ensure company
    :config
    (progn
      (require 'company-rtags)
      (add-to-list 'company-backends 'company-rtags)
      (setq company-rtags-begin-after-member-access t)
      (setq rtags-completions-enabled t)
      )))

(defun bl-c-c++/init-cmake-ide ()
  (use-package cmake-ide
    :init (with-eval-after-load 'rtags
            (cmake-ide-setup))))

;; Taken from https://github.com/syl20bnr/spacemacs/pull/2834/files
(defun bl-c-c++/post-init-rtags ()
  (setq company-rtags-begin-after-member-access nil)
  (setq rtags-completions-enabled t)

  (defun use-rtags (&optional useFileManager)
    (and (rtags-executable-find "rc")
         (cond ((not (gtags-get-rootpath)) t)
               ((and (not (eq major-mode 'c++-mode))
                     (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
               (useFileManager (rtags-has-filemanager))
               (t (rtags-is-indexed)))))

  (defun tags-find-symbol-at-point (&optional prefix)
    (interactive "P")
    (if (and (not (rtags-find-symbol-at-point prefix)) rtags-last-request-not-indexed)
        (helm-gtags-find-tag)))

  (defun tags-find-references-at-point (&optional prefix)
    (interactive "P")
    (if (and (not (rtags-find-references-at-point prefix)) rtags-last-request-not-indexed)
        (helm-gtags-find-rtag)))

  (defun tags-find-symbol ()
    (interactive)
    (call-interactively (if (use-rtags) 'rtags-find-symbol 'helm-gtags-find-symbol)))

  (defun tags-find-references ()
    (interactive)
    (call-interactively (if (use-rtags) 'rtags-find-references 'helm-gtags-find-rtag)))

  (defun tags-find-file ()
    (interactive)
    (call-interactively (if (use-rtags t) 'rtags-find-file 'helm-gtags-find-files)))

  (defun tags-imenu ()
    (interactive)
    (call-interactively (if (use-rtags t) 'rtags-imenu 'idomenu)))

  (dolist (mode '(c-mode c++-mode))
    (evil-leader/set-key-for-mode mode
      "m g ." 'rtags-find-symbol-at-point
      "m g ," 'rtags-find-references-at-point
      "m g v" 'rtags-find-virtuals-at-point
      "m g V" 'rtags-print-enum-value-at-point
      "m g /" 'rtags-find-all-references-at-point
      "m g Y" 'rtags-cycle-overlays-on-screen
      "m g >" 'rtags-find-symbol
      "m g <" 'rtags-find-references
      "m g [" 'rtags-location-stack-back
      "m g ]" 'rtags-location-stack-forward
      "m g D" 'rtags-diagnostics
      "m g G" 'rtags-guess-function-at-point
      "m g p" 'rtags-set-current-project
      "m g P" 'rtags-print-dependencies
      "m g e" 'rtags-reparse-file
      "m g E" 'rtags-preprocess-file
      "m g R" 'rtags-rename-symbol
      "m g M" 'rtags-symbol-info
      "m g S" 'rtags-display-summary
      "m g O" 'rtags-goto-offset
      "m g ;" 'rtags-find-file
      "m g F" 'rtags-fixit
      "m g L" 'rtags-copy-and-print-current-location
      "m g X" 'rtags-fix-fixit-at-point
      "m g B" 'rtags-show-rtags-buffer
      "m g I" 'rtags-imenu
      "m g T" 'rtags-taglist
      "m g h" 'rtags-print-class-hierarchy
      "m g a" 'rtags-print-source-arguments))

  (rtags-enable-standard-keybindings)
  (define-key c-mode-base-map (kbd "M-.") (function tags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,") (function tags-find-references-at-point))
  (define-key c-mode-base-map (kbd "M-;") (function tags-find-file))
  (define-key c-mode-base-map (kbd "C-.") (function tags-find-symbol))
  (define-key c-mode-base-map (kbd "C-,") (function tags-find-references))
  (define-key c-mode-base-map (kbd "C-<") (function rtags-find-virtuals-at-point))
  (define-key c-mode-base-map (kbd "M-i") (function tags-imenu))

  (define-key global-map (kbd "M-.") (function tags-find-symbol-at-point))
  (define-key global-map (kbd "M-,") (function tags-find-references-at-point))
  (define-key global-map (kbd "M-;") (function tags-find-file))
  (define-key global-map (kbd "C-.") (function tags-find-symbol))
  (define-key global-map (kbd "C-,") (function tags-find-references))
  (define-key global-map (kbd "C-<") (function rtags-find-virtuals-at-point))
  (define-key global-map (kbd "M-i") (function tags-imenu)))

;;; packages.el ends here
