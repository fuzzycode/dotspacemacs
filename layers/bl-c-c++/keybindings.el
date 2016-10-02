(spacemacs/declare-prefix "mr" "rTags")

(dolist (mode '(c-mode c++-mode))
  (evil-leader/set-key-for-mode mode
    "r ." 'rtags-find-symbol-at-point
    "r ," 'rtags-find-references-at-point
    "r v" 'rtags-find-virtuals-at-point
    "r V" 'rtags-print-enum-value-at-point
    "r /" 'rtags-find-all-references-at-point
    "r Y" 'rtags-cycle-overlays-on-screen
    "r >" 'rtags-find-symbol
    "r <" 'rtags-find-references
    "r b" 'rtags-location-stack-back
    "r f" 'rtags-location-stack-forward
    "r D" 'rtags-diagnostics
    "r G" 'rtags-guess-function-at-point
    "r p" 'rtags-set-current-project
    "r P" 'rtags-print-dependencies
    "r e" 'rtags-reparse-file
    "r E" 'rtags-preprocess-file
    "r R" 'rtags-rename-symbol
    "r M" 'rtags-symbol-info
    "r S" 'rtags-display-summary
    "r O" 'rtags-goto-offset
    "r ;" 'rtags-find-file
    "r F" 'rtags-fixit
    "r L" 'rtags-copy-and-print-current-location
    "r X" 'rtags-fix-fixit-at-point
    "r B" 'rtags-show-rtags-buffer
    "r I" 'rtags-imenu
    "r T" 'rtags-taglist
    "r h" 'rtags-print-class-hierarchy
    "r a" 'rtags-print-source-arguments))
