;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-08 23:56:42
;; Modified: <2024-06-08 23:58:58 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(require 'common-x)
(require 'skeleton)

(define-skeleton yx/auto-insert-h-header
  ""
  (replace-regexp-in-string
   "[^A-Z0-9]" "_"
   (string-replace "+" "P"
                   (upcase
                    (file-name-nondirectory buffer-file-name))))
  "/**\n***************************************************"
  "\n* @author: " (user-full-name)
  "\n* @date: " (format-time-string "%F %T")
  "\n* @brief: " (skeleton-read "brief: ")
  "\n* @modified: <>"
  "\n**************************************************\n*/"
  "\n\n#ifndef " str \n "#define " str
  "\n\n" @ _
  "\n\n#endif")

(define-skeleton yx/auto-insert-c-header
  ""
  nil
  "/**\n***************************************************"
  "\n* @author: " (user-full-name)
  "\n* @date: " (format-time-string "%F %T")
  "\n* @modified: <>"
  "\n**************************************************\n*/"
  "\n\n" @ _ "\n")

(define-skeleton yx/auto-insert-common-header
  ""
  nil
  "# --------------------------------------------------"
  "\n# Author: " (user-full-name)
  "\n# Date: " (format-time-string "%F %T")
  "\n# Modified: <>\n#"
  "\n# Description: " (skeleton-read "Description: ")
  "\n#\n#\n"
  "# --------------------------------------------------"
  "\n\n" @ _ "\n")

(define-skeleton yx/auto-insert-el-header
  ""
  nil
  ";;; -*- lexical-binding: t -*-"
  "\n\n;; Author: " (user-full-name) " <" (progn user-mail-address) ">"
  "\n;; Copyright (C) " (format-time-string "%Y") ", " (user-full-name) ", all right reserved."
  "\n;; Created: " (format-time-string "%F %T")
  "\n;; Modified: <>"
  "\n;; Licence: GPLv3"
  "\n\n;;; Commentary:\n\n;; " @ _
  "\n\n;;; Code:"
  "\n\n(provide '" (file-name-base (buffer-file-name)) ")"
  "\n;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n")

(defvar yx/auto-header-minor-mode nil
  "Non-nil if Yx/Auto Header Minor Mode is enabled.
See the `yx/auto-header-minor-mode' command to toggle this variable.")

;;;###autoload
(define-minor-mode yx/auto-header-minor-mode
  "Toggle Yx/Auto Header Minor Mode.
With a prefix argument ARG, enable Yx/Auto Header Minor Mode if ARG is positive;
otherwise, disable it.
If called from Lisp, enable the mode if ARG is omitted or nil."
  :global t
  :lighter " YXAH"
  (require 'autoinsert)
  (if yx/auto-header-minor-mode
      (progn
        (setq auto-insert-alist nil)
        (define-auto-insert "\\.el$" #'yx/auto-insert-el-header)
        (define-auto-insert "\\.py$" #'yx/auto-insert-common-header)
        (define-auto-insert "\\.R$"  #'yx/auto-insert-common-header)
        (define-auto-insert "\\.jl$" #'yx/auto-insert-common-header)
        (define-auto-insert
          "\\.\\([Hh]\\|hh\\|hpp\\|hxx\\|h\\+\\+\\)\\'"
          #'yx/auto-insert-h-header)
        (define-auto-insert
          "\\.\\([Cc]\\|cc\\|cpp\\|cxx\\|c\\+\\+\\)\\'"
          #'yx/auto-insert-c-header))))

(provide 'autoinsert-x)
;;; autoinsert-x.el ends here
