(defvar bl-c-c++-enable-rtags t
  "If non nil rtags will be enabled and setup.")

(configuration-layer/declare-layers '(auto-completion))

(spacemacs|defvar-company-backends c++-mode)
(spacemacs|defvar-company-backends c-mode)
