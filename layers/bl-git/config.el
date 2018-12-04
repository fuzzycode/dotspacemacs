(add-hook 'git-commit-mode-hook 'evil-insert-state)

(eval-after-load 'magit (lambda ()
                          (setq git-commit-summary-max-length 50)
                          (setq git-commit-fill-column 72)

                          (setq git-commit-turn-on-flyspell t)
                          (setq git-commit-turn-on-auto-fill t)))

;; Disable arrows in magit buffers
(setq magit-section-visibility-indicator nil)
