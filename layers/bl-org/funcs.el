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
;;; funcs.el ends here
