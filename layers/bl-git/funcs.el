;;; funcs.el ---                                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Björn Larsson

;; Keywords:

(defun bl-git/magit-submodule-update-recursive ()
  (interactive)
  (magit-run-git-async "submodule" "update" "--init" "--recursive"))
