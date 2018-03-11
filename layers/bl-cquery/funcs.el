
(defun cquery//enable ()
  (condition-case nil
      (lsp-cquery-enable)
    (user-error nil)))

;; (defun cquery/base () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/base"))
;; (defun cquery/callers () (interactive) (lsp-ui-peek-find-custom 'callers "$cquery/callers"))
;; (defun cquery/derived () (interactive) (lsp-ui-peek-find-custom 'derived "$cquery/derived"))
;; (defun cquery/vars () (interactive) (lsp-ui-peek-find-custom 'vars "$cquery/vars"))
