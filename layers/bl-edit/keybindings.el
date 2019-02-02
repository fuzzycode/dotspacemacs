(spacemacs/declare-prefix "ot" "Toggles")

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

(spacemacs/set-leader-keys "xws" 'bl-edit/sort-words)

(global-set-key (kbd "<f2>") 'flyspell-correct-previous-word-generic)

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "<f5>") 'projectile-run-project)
            (local-set-key (kbd "<f6>") 'projectile-compile-project)
            (local-set-key (kbd "<f7>") 'projectile-test-project)))

;; Setup key chords
(key-chord-define-global "uu" 'undo-tree-undo)
(key-chord-define-global "jj" 'avy-goto-word-or-subword-1)
(key-chord-define-global "kk" 'just-one-space)
(key-chord-define-global "BB" 'helm-buffers-list)

(global-set-key (kbd "<A-return>") 'lsp-ui-sideline-apply-code-actions)

(add-hook 'prog-mode-hook (lambda ()
                            (key-chord-define-local "ii" 'spacemacs/jump-to-definition)
                            (key-chord-define-local "jb" 'evil-jump-backward)))

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
