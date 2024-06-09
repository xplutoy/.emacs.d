;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-08 15:57:39
;; Modified: <>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(require 'consult)

;;;###autoload
(defun yx/consult-info-emacs ()
  "Search through Emacs info pages."
  (interactive)
  (consult-info "emacs" "efaq" "elisp" "cl" "compat"))

(provide 'consult-x)
;;; consult-x.el ends here
