;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 04:02:44
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(defgroup x-common ()
  "Auxiliary functions for my dotemacs."
  :group 'editing)

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-WIN     (memq system-type '(windows-nt cygwin)))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WSL     (and IS-LINUX (getenv "WSL_DISTRO_NAME")))

;;;; Utils

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

;;;; Common Functions

(defun x-common-latest-file (path)
  "Get latest file (including directory) in PATH."
  (car (seq-find
        (lambda (x) (not (nth 1 x)))    ; non-directory
        (sort (directory-files-and-attributes path 'full nil t)
              (lambda (x y) (time-less-p (nth 5 y) (nth 5 x)))))))

(defun x-common-collect-autoloads (dir target-file)
  (unless (string= (file-name-nondirectory (x-common-latest-file dir))
                   (file-name-nondirectory target-file))
    (require 'loaddefs-gen nil t)
    (loaddefs-generate dir target-file nil nil nil t))
  (load target-file nil t))

(defun x-common-pwd-replace-home (pwd)
  "Replace home in PWD with tilde (~) character."
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun x-common-empty-buffer-p ()
  "Test whether the buffer is empty."
  (or (= (point-min) (point-max))
      (save-excursion
        (goto-char (point-min))
        (while (and (looking-at "^\\([a-zA-Z]+: ?\\)?$")
                    (zerop (forward-line 1))))
        (eobp))))

(defun x-common-window-small-p ()
  "Return non-nil if window is small.
Check if the `window-width' or `window-height' is less than
`split-width-threshold' and `split-height-threshold',
respectively."
  (or (and (numberp split-width-threshold)
           (< (window-total-width) split-width-threshold))
      (and (numberp split-height-threshold)
           (< (window-total-height) split-height-threshold))))

(defun x-common-window-narrow-p ()
  "Return non-nil if window is narrow.
Check if the `window-width' is less than `split-width-threshold'."
  (and (numberp split-width-threshold)
       (< (window-total-width) split-width-threshold)))

(defun x-common-three-or-more-windows-p (&optional frame)
  "Return non-nil if three or more windows occupy FRAME.
If FRAME is non-nil, inspect the current frame."
  (>= (length (window-list frame :no-minibuffer)) 3))

(autoload 'auth-source-search "auth-source")

(defun x-common-auth-get-field (host prop)
  "Find PROP in `auth-sources' for HOST entry."
  (when-let ((source (auth-source-search :host host)))
    (if (eq prop :secret)
        (funcall (plist-get (car source) prop))
      (plist-get (flatten-list source) prop))))

(defvar x-common--line-regexp-alist
  '((empty . "[\s\t]*$")
    (indent . "^[\s\t]+")
    (non-empty . "^.+$")
    (list . "^\\([\s\t#*+]+\\|[0-9]+[^\s]?[).]+\\)")
    (heading . "^[=-]+"))
  "Alist of regexp types used by `x-common-line-regexp-p'.")

(defun x-common-line-regexp-p (type &optional n)
  "Test for TYPE on line.
TYPE is the car of a cons cell in
`x-common--line-regexp-alist'.  It matches a regular
expression.

With optional N, search in the Nth line from point."
  (save-excursion
    (goto-char (line-beginning-position))
    (and (or (beginning-of-line n) t)
         (save-match-data
           (looking-at
            (alist-get type x-common--line-regexp-alist))))))



(provide 'x-common)
;;; x-common.el ends here
