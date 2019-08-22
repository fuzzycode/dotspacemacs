;;; packages.el --- bl-latex layer packages file for Spacemacs.
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
;; added to `bl-latex-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `bl-latex/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `bl-latex/pre-init-PACKAGE' and/or
;;   `bl-latex/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst bl-latex-packages
  '(pdf-tools)
  "The list of Lisp packages required by the bl-latex layer.

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


(defun bl-latex/pre-init-pdf-tools ()
  (spacemacs|use-package-add-hook pdf-tools
    :pre-config (progn

                  (setq pdf-view-display-size 'fit-page
                        pdf-view-resize-factor 1.1
                        pdf-sync-backward-display-action t
                        pdf-sync-forward-display-action t)

                  ;; Hooks
                  (add-hook 'pdf-view-mode-hook #'pdf-sync-minor-mode)
                  (add-hook 'pdf-view-mode-hook #'pdf-links-minor-mode)

                  (add-hook 'pdf-view-mode-hook (lambda ()
                                                  (cua-mode 0)
                                                  (linum-mode 0)))

                  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

                  ;; Setup pdf-tools
                  (when (executable-find "epdfinfo")
                    (setq-default TeX-view-program-selection '((output-pdf "PDF Tools")))
                    ; TODO(Björn Larsson): Fix this setup properly, I think it is too aggressive
                    (setq-default TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
                    (setq-default pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")))))

 ;;; packages.el ends here
