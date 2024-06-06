;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 04:02:44
;; Modified: <>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-WIN     (memq system-type '(windows-nt cygwin)))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WSL     (and IS-LINUX (getenv "WSL_DISTRO_NAME")))

(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list (delq ,(if fetcher
                          `(funcall ,fetcher ,elt ,list)
                        elt)
                     ,list)))

(defmacro with-silent (&rest body)
  "Execute BODY with message output inhibited."
  `(let ((inhibit-message t))
     ,@body))

(defun yx/latest-file (path)
  "Get latest file (including directory) in PATH."
  (car (seq-find
        (lambda (x) (not (nth 1 x))) ; non-directory
        (sort (directory-files-and-attributes path 'full nil t)
              (lambda (x y) (time-less-p (nth 5 y) (nth 5 x)))))))

(provide 'yx-core)
;;; yx-core.el ends here
