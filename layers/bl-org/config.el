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

        org-todo-keywords '((sequence "IDEA(i!)" "TODO(t!)" "IN-PROGRESS(p!)"  "BLOCKED(b@/!)" "|" "DONE(d!)" "CANCELED(c@/!)")))

  (defvar bl-org/todo-file (bl-org/join-paths org-directory "todo.org"))
  (defvar bl-org/notes-file (bl-org/join-paths org-directory "notes.org"))
  (defvar bl-org/archive-file (bl-org/join-paths org-directory "archive.org"))
  (defvar bl-org/inbox-file (bl-org/join-paths org-directory "inbox.org"))
  (defvar bl-org/calendar-file (bl-org/join-paths org-directory "calendar.org"))

  (setq org-archive-location (format "%s::%s" bl-org/archive-file "* From %s" ))
  (add-hook 'org-capture-prepare-finalize-hook 'org-id-store-link)

  (setq org-refile-target-verify-function 'bl-org/verify-refile-target)

  (add-to-list 'org-capture-templates
               `("t" "Todo" entry (file+headline ,bl-org/inbox-file "TODOs")
                 (function bl-org/make-org-todo-template)))

  (add-to-list 'org-capture-templates
               `("n" "Note" entry (file+headline ,bl-org/inbox-file "Notes")
                 (function bl-org/make-org-note-template)))


  )

(with-eval-after-load 'org-agenda
  (unless (boundp 'org-agenda-files)
    (setq org-agenda-files '()))

  (when (file-exists-p bl-org/todo-file)
    (add-to-list 'org-agenda-files bl-org/todo-file))

  (when (file-exists-p bl-org/calendar-file)
    (add-to-list 'org-agenda-files bl-org/calendar-file)))
  ;;; config.el ends here
