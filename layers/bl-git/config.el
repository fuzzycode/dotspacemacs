(add-hook 'git-commit-mode-hook 'evil-insert-state)


(with-eval-after-load 'magit
  (setq git-commit-summary-max-length 50
        git-commit-fill-column 72
        git-commit-turn-on-flyspell t
        git-commit-turn-on-auto-fill t
        magit-section-visibility-indicator nil
        magit-wip-merge-branch t)

  (magit-wip-after-save-mode)

  (add-hook 'magit-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (add-hook 'magit-refresh-buffer-hook #'vc-refresh-state)

  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (add-to-list 'magit-no-confirm 'trash)
  (add-to-list 'magit-no-confirm 'safe-with-wip)

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpulled-from-upstream
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpulled-from-pushremote
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpushed-to-upstream
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpushed-to-pushremote
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-overview
                          'magit-insert-unpulled-from-upstream)

  (transient-bind-q-to-quit)

  (transient-append-suffix 'magit-fetch "-p"
    '("-P" "Prune tags" "--prune-tags"))

  (transient-append-suffix 'magit-fetch "-P"
    '("-t" "Fetch tags" "--tags"))

  (transient-append-suffix 'magit-submodule "u"
    '("R" "Recursive Update" bl-git/magit-submodule-update-recursive))
  )
