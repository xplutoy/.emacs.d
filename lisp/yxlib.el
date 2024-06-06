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
;;;###autoload
(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

;;;###autoload
(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))

;;;###autoload
(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list (delq ,(if fetcher
                          `(funcall ,fetcher ,elt ,list)
                        elt)
                     ,list)))

;;;###autoload
(defmacro with-silent (&rest body)
  "Execute BODY with message output inhibited."
  `(let ((inhibit-message t))
     ,@body))

;;;###autoload
(defun yx/pwd-replace-home (pwd)
  "Replace home in PWD with tilde (~) character."
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

;; tab-line+
;;;###autoload
(defun yx/tab-line-buffer-group (buffer)
  "Use the project.el name for the buffer group"
  (with-current-buffer buffer
    (if (project-current)
        (s-chop-suffix "/" (car (project-roots (project-current))))
      "+++")))

;;;###autoload
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

;; org+
;;;###autoload
(defun yx/org-mode-setup ()
  (auto-fill-mode -1)
  (variable-pitch-mode 1)
  (push 'cape-tex completion-at-point-functions)
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

;;;###autoload
(defun yx/org-reformat-buffer ()
  (interactive)
  (when (y-or-n-p "Really format current buffer? ")
    (let ((document (org-element-interpret-data (org-element-parse-buffer))))
      (erase-buffer)
      (insert document)
      (goto-char (point-min)))))

;;;###autoload
(defun yx/org-check-latex-fragment ()
  (let ((datum (org-element-context)))
    (when (memq (org-element-type datum) '(latex-environment latex-fragment))
      (org-latex-preview)
      t)))

;;;###autoload
(defun yx/org-agenda-finalize-setup ()
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;;;###autoload
(defun yx/org-babel-display-image ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

;;;###autoload
(defun yx/insert-fixed-org-link ()
  "从 minibuffer 读取链接地址，在当前光标位置插入固定的 Org 链接。"
  (interactive)
  (let* ((link-address (read-string "Input Link: "))
         (link-text "🔗")
         (org-link-format (format "[[%s][%s]]" link-address link-text)))
    (insert org-link-format)))

;;;###autoload
(defun yx/org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-china-x)
  (let* ((dayname (aref cal-china-x-days
                        (calendar-day-of-week date)))
         (day (cadr date))
         (month (car date))
         (year (nth 2 date))
         (cn-date (calendar-chinese-from-absolute (calendar-absolute-from-gregorian date)))
         (cn-month (cl-caddr cn-date))
         (cn-day (cl-cadddr cn-date))
         (cn-month-string (concat (aref cal-china-x-month-name
                                        (1- (floor cn-month)))
                                  (if (integerp cn-month)
                                      ""
                                    "[闰]")))
         (cn-day-string (aref cal-china-x-day-name
                              (1- cn-day))))
    (format "%04d-%02d-%02d 周%-8s 农历%s%s" year month
            day dayname cn-month-string cn-day-string)))

(provide 'yxlib)
;;; utils.el ends here
