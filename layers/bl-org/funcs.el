;;; packages.el --- bl-org layer packages file for Spacemacs.
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

;; http://doc.norang.ca/org-mode.html
(defun bl-org/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun bl-org/org-archive-if (condition-function)
  (if (funcall condition-function)
      (let ((next-point-marker
             (save-excursion (org-forward-heading-same-level 1) (point-marker))))
        (org-archive-subtree)
        (setq org-map-continue-from (marker-position next-point-marker)))))

(defun bl-org/org-archive-if-completed ()
  (interactive)
  (bl-org/org-archive-if 'org-entry-is-done-p))

(defun bl-org/org-archive-completed-in-buffer ()
  (interactive)
  (org-map-entries 'bl-org/org-archive-if-completed))

;; https://ivanmalison.github.io/dotfiles/#org
(defun bl-org/join-paths (root &rest dirs)
  (let ((result root))
    (cl-loop for dir in dirs do
             (setq result (concat (file-name-as-directory result) dir)))
    result))

(defun bl-org/open-inbox ()
  (interactive)
  (find-file bl-org/inbox-file))

;; Capture Templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun bl-org/make-org-todo-template (&key (content "%?") (creation-state "TODO") (project nil))
  (with-temp-buffer
    (org-mode)
    (org-insert-heading)
    (insert content)
    (org-todo creation-state)
    (org-set-property "CREATED"
                      (with-temp-buffer
                        (org-insert-time-stamp
                         (org-current-effective-time) t t)))
    (when project
      (org-set-property "PROJECT" project))
    (remove-hook 'post-command-hook 'org-add-log-note)
    (let ((org-log-note-purpose 'state)
          (org-log-note-return-to (point-marker))
          (org-log-note-marker (progn (goto-char (org-log-beginning t))
                                      (point-marker)))
          (org-log-note-state creation-state))
      (org-add-log-note))
    (buffer-substring-no-properties (point-min) (point-max))))

(cl-defun bl-org/make-org-note-template (&key (content "%?"))
  (with-temp-buffer
    (org-mode)
    (org-insert-heading)
    (insert content)
    (org-set-property "CREATED"
                      (with-temp-buffer
                        (org-insert-time-stamp
                         (org-current-effective-time) t t)))
    (buffer-substring-no-properties (point-min) (point-max))))


;;; funcs.el ends here
