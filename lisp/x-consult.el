;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-08 15:57:39
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(require 'consult)

;;;###autoload
(defun x-consult-info-emacs ()
  "Search through Emacs info pages."
  (interactive)
  (consult-info "emacs" "efaq" "elisp" "cl" "compat"))

;;;###autoload
(defun x-consult-outline-insert-heading (target)
  (let* ((marker (plist-get
                  (text-properties-at 0 target)
                  'consult--candidate))
         (headline-name (org-entry-get nil "ITEM")))
    (org-insert-link nil headline-name)))

(provide 'x-consult)
;;; x-consult.el ends here
