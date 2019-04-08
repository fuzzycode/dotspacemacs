;;; funcs.el --- bl-latex layer functions file for Spacemacs.
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
;;; Code:

;; https://github.com/domtronn/emacs/blob/master/init/functions.el
(defun bl-latex/xelatex-make ()
  (interactive)
  (if (string-equal major-mode "latex-mode")
      (progn
        (setq buffer-save-without-query t)
        (if (buffer-modified-p) (save-buffer))
        (let ((f1 (current-frame-configuration))
              (retcode (shell-command (concat "xelatex -interaction=nonstopmode " (buffer-file-name)))))
          (message "Return code ❯ %s" retcode)
          (if (get-buffer (concat (file-name-sans-extension (buffer-name)) ".pdf"))
              (kill-buffer (concat (file-name-sans-extension (buffer-name)) ".pdf")))
          (find-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf"))
          (delete-other-windows)))))

;;; funcs.el ends here
