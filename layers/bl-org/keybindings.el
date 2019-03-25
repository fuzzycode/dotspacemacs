;;; keybindings.el --- bl-org layer packages file for Spacemacs.
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

(global-set-key (kbd "C-c i") 'bl-org/open-inbox)

(spacemacs/set-leader-keys "ooi" 'bl-org/open-inbox)

;;; keybindings.el ends here
