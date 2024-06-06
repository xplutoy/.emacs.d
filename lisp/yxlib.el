;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-01 19:19:56
;; Modified: <>
;; Licence: GPLv3

;;; Commentary:

;; 公共函数

;;; Code:
(require 's)

;; common
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

(defun yx/pwd-replace-home (pwd)
  "Replace home in PWD with tilde (~) character."
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun yx/latest-file (path)
  "Get latest file (including directory) in PATH."
  (file-name-nondirectory (car (seq-find
                                (lambda (x) (not (nth 1 x))) ; non-directory
                                (sort
                                 (directory-files-and-attributes path 'full nil t)
                                 (lambda (x y) (time-less-p (nth 5 y) (nth 5 x))))))))

;; tab-line+
(defun yx/tab-line-buffer-group (buffer)
  "Use the project.el name for the buffer group"
  (with-current-buffer buffer
    (if (project-current)
        (s-chop-suffix "/" (car (project-roots (project-current))))
      "+++")))

(defun yx/tab-line-tabs-buffer-list ()
  (seq-filter (lambda (b) (and (buffer-live-p b)
                               (/= (aref (buffer-name b) 0) ?\s)
                               (with-current-buffer b
                                 (not (or (minibufferp)
                                          (string-match-p "\\` " (buffer-name))
                                          (string-match-p "\\*" (buffer-name))
                                          (memq major-mode tab-line-exclude-modes)
                                          (get major-mode 'tab-line-exclude)
                                          (buffer-local-value 'tab-line-exclude (current-buffer)))))))
              (seq-uniq (append (list (current-buffer))
                                (mapcar #'car (window-prev-buffers))
                                (buffer-list)))))

(provide 'yxlib)
;;; utils.el ends here
