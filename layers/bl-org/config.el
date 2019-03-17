;;; packages.el --- bl-org layer config file for Spacemacs.
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

;;; Code:


(with-eval-after-load 'org
  (setq org-src-preserve-indentation t
        org-src-fontify-natively t
        org-log-into-drawer t
        org-log-done 'time
        org-log-redeadline 'time
        org-log-reschedule 'time
        org-treat-insert-todo-heading-as-state-change t
        ; TODO(Björn Larsson): Remove hard-coded path
        org-directory "~/Documents/Org"
        org-id-method 'uuidgen
        org-clone-delete-id t
        org-timeline-show-empty-dates t
        org-enforce-todo-dependencies t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-capture-templates '()
        org-confirm-babel-evaluate nil
        org-insert-heading-respect-content t
        org-archive-mark-done t

        org-highest-priority ?A
        org-lowest-priority ?E
        org-default-priority ?C

        org-refile-targets '((org-agenda-files :maxlevel . 9))
        org-refile-use-outline-path 'file
        org-refile-allow-creating-parent-nodes t

         org-todo-keywords '((sequence "IDEA(i!)" "TODO(t!)" "IN-PROGRESS(p!)"  "BLOCKED(b@/!)" "|" "DONE(d!)" "CANCELED(c@/!)"))))

  ;;; config.el ends here
