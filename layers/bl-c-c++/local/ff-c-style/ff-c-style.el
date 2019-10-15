;;;###autoload
(defconst ff-c-style
  '((fill-column . 120)
    (whitespace-line-column . 120)
    (indent-tabs-mode . t)
    (c-indent-level . 4)
    (tab-width . 4)
    (c-basic-offset . 4)
    (tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))

    ;; Control the indentation
    (c-offsets-alist .
                     ((access-label . -)
                      (inclass . +)
                      (class-close . 0)
                      (inline-open . 0)
                      (topmost-intro . 0)
                      (member-init-cont . -2) ;; Make space for ", "
                      (member-init-intro . +)

                      (brace-list-intro . +)

                      (defun-block-intro . +)
                      (statement . 0)
                      (statement-block-intro . +)
                      (block-open . 0)

                      (innamespace . [0])
                      (namespace-close . 0)
                      (module-open . 0)
                      (inextern-lang . 0)
                      (extern-lang-close . 0)
                      (case-label . 0)
                      (cpp-macro . -1000)
                      (cpp-macro-cont . +)

                      (arglist-cont-nonempty . +)

                      (substatement-open . 0)))

     ;; Control brace placement
    (c-hanging-braces-alist .
                            ((block-close . c-snug-do-while)
                             (substatement-open . (after))
                             (namespace-open . (after))
                             (namespace-close . (before after))
                             (class-open . (after))
                             (extern-lang-open . (after))))

    (c-hanging-colons-alist .
                            ((case-label)
                             (label after)
                             (access-label after)
                             (member-init-intro before)
                             (inher-intro)))

    (c-cleanup-list .
                    (brace-else-brace
                     brace-elseif-brace
                     brace-catch-brace
                     scope-operator
                     defun-close-semi))

    )
  "FF C Style Guide"
  )

;;;###autoload
(defun ff-add-c-style ()
  "Adds ff-c-style"
  (interactive)
  (c-add-style "FF" ff-c-style nil))

;;;###autoload
(defun ff-set-c-style ()
  "Adds and Sets the current style to ff-c-style"
  (interactive)
  (c-add-style "FF" ff-c-style t))

(provide 'ff-c-style)
