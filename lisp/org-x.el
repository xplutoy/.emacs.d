;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-08 23:50:10
;; Modified: <2024-06-12 12:16:55 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

(defgroup org-x ()
  "Extensions for org.el."
  :group 'org)

;;;###autoload
(defun yx/org-check-latex-fragment ()
  (let ((datum (org-element-context)))
    (when (memq (org-element-type datum) '(latex-environment latex-fragment))
      (org-latex-preview)
      t)))

;;;###autoload
(defun yx/org-agenda-to-appt ()
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;;;###autoload
(defun yx/org-babel-display-image ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

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

;;;###autoload
(defun yx/org-link-copy (&optional arg)
  "Extract URL from org-mode link and add it to kill ring."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
	 (type (org-element-property :type link))
	 (url (org-element-property :path link))
	 (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied URL: " url))))

;;;###autoload
(defun yx/org-show-current-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (interactive)
  (if (save-excursion
        (end-of-line)
        (outline-invisible-p))
      (progn
        (org-fold-show-entry)
        (outline-show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (outline-hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-fold-show-entry)
    (outline-show-children)))

;;;###autoload
(defun yx/org-insert-fixed-link ()
  "从 minibuffer 读取链接地址，在当前光标位置插入固定的 Org 链接。"
  (interactive)
  (let* ((link-address (read-string "Input Link: "))
         (link-text "🔗")
         (org-link-format (format "[[%s][%s]]" link-address link-text)))
    (insert org-link-format)))

;;;###autoload
(defun yx/org-toggle-inline-images-in-subtree (&optional refresh beg end)
  "Refresh inline image previews in the current heading/tree."
  (interactive)
  (let* ((beg (or beg
                  (if (org-before-first-heading-p)
                      (save-excursion (point-min))
                    (save-excursion (org-back-to-heading) (point)))))
         (end (or end
                  (if (org-before-first-heading-p)
                      (save-excursion (org-next-visible-heading 1) (point))
                    (save-excursion (org-end-of-subtree) (point)))))
         (overlays (cl-remove-if-not (lambda (ov) (overlay-get ov 'org-image-overlay))
                                     (ignore-errors (overlays-in beg end)))))
    (if (or refresh (not overlays))
        (org-display-inline-images t t beg end)
      (dolist (ov overlays nil)
        (delete-overlay ov)
        (setq org-inline-image-overlays (delete ov org-inline-image-overlays)))
      )))

;;;###autoload
(defun yx/org-fix-blank-lines (&optional prefix)
  "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
  (interactive "P")
  (org-map-entries (lambda ()
                     (org-with-wide-buffer
                      ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                      ;; newlines before the current heading, so we do this part widened.
                      (while (not (looking-back "\n\n" nil))
                        ;; Insert blank lines before heading.
                        (insert "\n")))
                     (let ((end (org-entry-end-position)))
                       ;; Insert blank lines before entry content
                       (forward-line)
                       (while (and (org-at-planning-p)
                                   (< (point) (point-max)))
                         ;; Skip planning lines
                         (forward-line))
                       (while (re-search-forward org-drawer-regexp end t)
                         ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                         ;; for some reason it doesn't work correctly when operating on hidden text.
                         ;; This works, taken from `org-agenda-get-some-entry-text'.
                         (re-search-forward "^[ \t]*:END:.*\n?" end t)
                         (goto-char (match-end 0)))
                       (unless (or (= (point) (point-max))
                                   (org-at-heading-p)
                                   (looking-at-p "\n"))
                         (insert "\n"))))
                   t (if prefix
                         nil
                       'tree)))

;;;###autoload
(defun yx/org-reformat-buffer ()
  (interactive)
  (let ((document (org-element-interpret-data (org-element-parse-buffer))))
    (erase-buffer)
    (insert document)
    (goto-char (point-min))))

;;;###autoload
(defun yx/org-format-buffer-dwim ()
  (interactive)
  (when (and (eq major-mode 'org-mode))
    (call-interactively #'yx/org-reformat-buffer)
    (let ((current-prefix-arg 4))
      (call-interactively #'yx/org-fix-blank-lines))))

(provide 'org-x)
;;; org-x.el ends here
