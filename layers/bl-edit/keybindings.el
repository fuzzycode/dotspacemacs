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

(spacemacs/set-leader-keys "xws" 'sort-words)

(global-set-key (kbd "<f2>") 'flyspell-correct-previous-word-generic)

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "<f6>") 'bl-edit/maybe-save-and-compile)
            (local-set-key (kbd "<f5>") 'projectile-run-project)))

;; Setup key chords
(key-chord-define-global "uu" 'undo-tree-undo)
(key-chord-define-global "jj" 'avy-goto-word-1)
(key-chord-define-global " 1" 'winum-select-window-1)
(key-chord-define-global " 2" 'winum-select-window-2)
(key-chord-define-global " 3" 'winum-select-window-3)
(key-chord-define-global " 4" 'winum-select-window-4)
