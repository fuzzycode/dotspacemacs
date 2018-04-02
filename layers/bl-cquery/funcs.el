(require 'cl-lib)
(require 'subr-x)

(defun bl-cquery/enable ()
  (condition-case nil
      (lsp-cquery-enable)
    (user-error nil)))


(defun bl-cquery/customise-lsp-ui-peek ()
    (defun text-document/type-definition () (interactive) (lsp-ui-peek-find-custom 'type "textDocument/typeDefinition"))
    (defun cquery/base () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/base"))
    (defun cquery/callers () (interactive) (lsp-ui-peek-find-custom 'callers "$cquery/callers"))
    (defun cquery/derived () (interactive) (lsp-ui-peek-find-custom 'derived "$cquery/derived"))
    (defun cquery/vars () (interactive) (lsp-ui-peek-find-custom 'vars "$cquery/vars"))
    (defun cquery/random () (interactive) (lsp-ui-peek-find-custom 'random "$cquery/random"))

    (defun cquery/references-address ()
      (interactive)
      (lsp-ui-peek-find-custom
       'address "textDocument/references"
       (plist-put (lsp--text-document-position-params) :context
                  '(:role 128))))

    (defun cquery/references-read ()
      (interactive)
      (lsp-ui-peek-find-custom
       'read "textDocument/references"
       (plist-put (lsp--text-document-position-params) :context
                  '(:role 8))))

    (defun cquery/references-write ()
      (interactive)
      (lsp-ui-peek-find-custom
       'write "textDocument/references"
       (plist-put (lsp--text-document-position-params) :context
                  '(:role 16))))

  )

(defun bl-cquery/define-keys-for-mode (mode)
  "Define key bindings for the specific MODE."

  ;; lsp layer uses =,l,t,r as prefixes for format, lsp, toggle and refactor -- extend these
  (spacemacs/declare-prefix-for-mode mode "mh" "hierarchy")

  (spacemacs/set-leader-keys-for-major-mode mode
    ;; hierarchy
    "hb" #'cquery/base
    "hd" #'cquery/derived
    "hc" #'cquery-call-hierarchy
    "hC" (lambda () (interactive) (cquery-call-hierarchy t))
    "hi" #'cquery-inheritance-hierarchy
    "hI" (lambda () (interactive) (cquery-inheritance-hierarchy t))
    "hm" #'cquery-member-hierarchy
    "hM" (lambda () (interactive) (cquery-member-hierarchy t))
    ;; lsp/peek
    "lA" #'cquery/references-address
    "lR" #'cquery/references-read
    "lW" #'cquery/references-write
    "lc" #'cquery/callers
    "lt" #'text-document/type-definition
    "lv" #'cquery/vars
    ;; Refactor
    "mR" #'lsp-rename
    )
  )
