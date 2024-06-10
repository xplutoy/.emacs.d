;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-08 15:57:39
;; Modified: <2024-06-10 10:59:29 yangx>
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

;;;###autoload
(defun yx/consult-outline-insert-heading (target)
  (let* ((marker (plist-get
                  (text-properties-at 0 target)
                  'consult--candidate))
         (headline-name (org-entry-get nil "ITEM")))
    (org-insert-link nil headline-name)))

(provide 'consult-x)
;;; consult-x.el ends here
