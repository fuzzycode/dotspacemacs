;;; funcs.el --- bl-edit layer functions file for Spacemacs.
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

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;; Code:



(defun bl-edit/goto-first-empty-line (&optional from-top)
  "Finds the first empty line and moves point to it. If from-top is true the search will start from the top of the buffer."
  (interactive)
  (when from-top
    (beginning-of-buffer))

  (forward-char)
  (re-search-forward "^$"))

(defun bl-edit/manual-save-hook ()
  "Used in `after-save-hook' to reset the edit mode to normal mode again."
  (when (memq this-command '(save-buffer save-some-buffers))
    (evil-normal-state)))

;; https://emacs.stackexchange.com/questions/62/hide-compilation-window
(defun bl-edit/bury-compile-buffer-if-successful (buffer string)
  (if (and
       (null (string-match ".*exited abnormally.*" string))
       (bound-and-true-p  bl-edit-close-compile-on-success))
      ;;no errors, make the compilation window go away in a few seconds
      (progn
        (run-at-time
         (format "%d sec" bl-edit-compile-auto-close-time) nil 'delete-windows-on
         (get-buffer-create "*compilation*")))))

(defun bl-edit/maybe-notify-compile-finish (buffer string)
  "Show an alert when compilation finished, like XCode does"
  (spacemacs|require 'alert)
  (when bl-edit-notify-compile-finished
    (if (string-match "^finished" string)
        (alert "Compilation finished OK!" :title "Compilation Successful" :category 'compile :id 'compile-ok)
      (alert "Compilation Failed" :title "Compilation Failed" :category 'compile :id 'compile-fail))))

;; http://emacsredux.com/blog/2013/05/30/joining-lines/
(defun bl-edit/top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

;; https://emacs.stackexchange.com/questions/2105/how-do-i-disable-key-chord-mode-in-the-minibuffer
(defun bl-edit/disable-key-chord-mode ()
  (set (make-local-variable 'input-method-function) nil))

;; https://github.com/domtronn/emacs/blob/6a94813c69a9704094778e3f09cb35e834924d9f/init/advice.el
(defun bl-edit/remove-mode-line-box (&rest args)
  (set-face-attribute 'mode-line nil :box nil :underline nil)
  (set-face-attribute 'mode-line-inactive nil :box nil :underline nil))

(defun bl-edit/disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defun bl-edit/dim-lsp-sideline (&rest args)
  (mapcar (lambda (face)
           (when (facep face)
             (set-face-foreground face (face-attribute 'font-lock-comment-face :foreground))))
          '(lsp-ui-sideline-code-action lsp-ui-sideline-current-symbol lsp-ui-sideline-symbol lsp-ui-sideline-symbol-info)))

(defun bl-edit/disable-themes (&rest args)
  (bl-edit/disable-all-themes))

;; Hack to workaround bug
;; https://github.com/syl20bnr/spacemacs/issues/11058
(defun bl-edit/semantic-remove-hooks ()
  (remove-hook 'completion-at-point-functions
               'semantic-analyze-completion-at-point-function)
  (remove-hook 'completion-at-point-functions
               'semantic-analyze-notc-completion-at-point-function)
  (remove-hook 'completion-at-point-functions
               'semantic-analyze-nolongprefix-completion-at-point-function))

(add-hook 'semantic-mode-hook #'bl-edit/semantic-remove-hooks)

(defun bl-edit/newline-at-end-of-line ()
  "Adds a new line at the end of the line"
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))


(defun bl-edit/run-current-file ()
  "http://ergoemacs.org/emacs/elisp_run_current_file.html"
  (interactive)

  (when (not (buffer-file-name)) (save-buffer))
  (when (buffer-modified-p) (save-buffer))

  (let* ((resize-mini-windows nil)
         (_fname (buffer-file-name))
         (_output (get-buffer-create (format "*run: %s*" _fname)))
         (_fsuffix (file-name-extension _fname))
         (_suffix-map '(
                        ("py" . "python")
                        ("sh" . "bash")))
         (_program (cdr (assoc _fsuffix _suffix-map)))
         (_cmd (concat _program " \"" _fname "\"")))

    (cond
     ((string-equal _fsuffix "el")
      (load _fname))
     (t (if _program
            (progn
              (message "Running current file")
              (async-shell-command _cmd _output))
          (error (format "No executor available for file suffix %s" _fsuffix)))))))

;;; funcs.el ends here
