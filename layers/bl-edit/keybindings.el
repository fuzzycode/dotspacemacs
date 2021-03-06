(spacemacs/declare-prefix "ot" "Toggles")
(spacemacs/declare-prefix "oo" "Org")
(spacemacs/declare-prefix "op" "Project")

;; Evil-mode or no Evil mode, I NEED some of the emacs navigation keys
(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
(define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
(define-key evil-normal-state-map "\C-f" 'evil-forward-char)
(define-key evil-normal-state-map "\C-b" 'evil-backward-char)
(define-key evil-visual-state-map "\C-b" 'evil-backward-char)
(define-key evil-normal-state-map "\C-n" 'evil-next-line)
(define-key evil-visual-state-map "\C-n" 'evil-next-line)
(define-key evil-normal-state-map "\C-p" 'evil-previous-line)
(define-key evil-visual-state-map "\C-p" 'evil-previous-line)

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "<f5>") 'projectile-run-project)
            (local-set-key (kbd "<f6>") 'projectile-compile-project)
            (local-set-key (kbd "<f7>") 'projectile-test-project)))

(global-set-key (kbd "<f2>") 'flyspell-correct-wrapper)

;; Setup key chords
(key-chord-define-global "uu" 'undo-tree-undo)
(key-chord-define-global "jj" 'avy-goto-word-or-subword-1)
(key-chord-define-global "kk" 'just-one-space)
(key-chord-define-global "BB" 'helm-buffers-list)

(global-set-key (kbd "<A-return>") 'lsp-ui-sideline-apply-code-actions)

(global-set-key (kbd "<A-up>") 'join-line)
(global-set-key (kbd "<A-down>") 'bl-edit/top-join-line)

(global-set-key (kbd "C-c u") 'undo)

(global-set-key (kbd "S-<return>") 'bl-edit/newline-at-end-of-line)

(define-key prog-mode-map (kbd "C-x C-i") 'imenu)

(spacemacs/set-leader-keys "opC" 'projectile-configure-project)

(dolist (hook '(python-mode-hook sh-mode-hook))
        (add-hook hook
                  (lambda ()
                    (local-set-key (kbd "<f5>") 'bl-edit/run-current-file))))

(dolist (hook bl-edit-dap-mode-hooks)
  (add-hook hook
            (lambda ()
              (local-set-key (kbd "S-<f5>") 'dap-debug)
              (local-set-key (kbd "C-<f5>") 'dap-debug-last)
              (local-set-key (kbd "<f8>") 'dap-continue)
              (local-set-key (kbd "<f9>") 'dap-breakpoint-toggle)
              (local-set-key (kbd "<f10>") 'dap-next)
              (local-set-key (kbd "<f11>") 'dap-step-in)
              (local-set-key (kbd "S-<f11>") 'dap-step-out)
              )))
