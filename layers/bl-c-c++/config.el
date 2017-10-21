(defvar bl-c-c++-enable-rtags nil
  "If non nil rtags will be enabled and setup.")

(configuration-layer/declare-layers '(auto-completion))

(setq semanticdb-default-save-directory "~/.emacs.d/.cache/semanticdb/")
