;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-01 19:19:56
;; Modified: <>
;; Licence: GPLv3

;;; Commentary:

;; 公共函数

;;; Code:
(require 'skeleton)

;; common
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

;;;###autoload
(defun yx/insert-date ()
  "Insert a date according to `%F' format."
  (interactive)
  (insert (format-time-string "%F" (current-time))))

;;;###autoload
(defun yx/comment-dwim (n)
  "Comment N lines, defaulting to the current one.
When the region is active, comment its lines instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line n)))

;;;###autoload
(defun yx/fill-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command #'yx/fill-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

;;;###autoload
(defun yx/open-line (&optional above)
  "在当前行上方或下方插入新行。
如果带有前缀参数，则在上方插入；否则，在下方插入。"
  (interactive "P")
  (if above
      (progn
        (beginning-of-line)
        (insert "\n")
        (backward-char)
        (indent-according-to-mode))
    (end-of-line)
    (newline-and-indent)))

;;;###autoload
(defun yx/begin-of-line-dwim ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

;;;###autoload
(defun yx/scratch-buffer ()
  "Switch to '*scratch*' buffer and maximize it"
  (interactive)
  (scratch-buffer)
  (delete-other-windows))

;;;###autoload
(defun yx/kill-buffer-dwim (&optional arg)
  (interactive "P")
  (if arg
      (call-interactively 'kill-buffer)
    (kill-current-buffer)))

;;;###autoload
(defun yx/delete-window-dwim ()
  "Delete the current window or bury its buffer.
If the current window is alone in its frame, bury the buffer
instead."
  (interactive)
  (unless (ignore-errors (delete-window) t)
    (bury-buffer)))

;;;###autoload
(defun yx/new-empty-buffer ()
  "Create a new empty buffer
Warning: new buffer is not prompted for save when killed, see `kill-buffer'."
  (interactive)
  (let ((xbuf (generate-new-buffer "untitled")))
    (set-buffer-major-mode xbuf)
    (switch-to-buffer xbuf)))

;;;###autoload
(defun yx/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;;;###autoload
(defun yx/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond ((region-active-p)
         (keyboard-quit))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ((derived-mode-p 'completion-list-mode)
         (delete-completion-window))
        ((> (minibuffer-depth) 0)
         (abort-recursive-edit))
        (t
         (keyboard-quit))))

;;;###autoload
(defun yx/smarter-selective-display (&optional level)
  "Fold text indented same of more than the cursor.

This function toggles folding according to the level of
indentation at point. It's convenient not having to specify a
number nor move point to the desired column.
"
  (interactive "P")
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (or level (1+ (current-column))))))

;;;###autoload
(defun yx/tab-line-buffer-group (buffer)
  "Use the project.el name for the buffer group"
  (require 's)
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
(defun yx/org-reformat-buffer ()
  (interactive)
  (when (y-or-n-p "Really format current buffer? ")
    (let ((document (org-element-interpret-data (org-element-parse-buffer))))
      (erase-buffer)
      (insert document)
      (goto-char (point-min)))))

;;;###autoload
(defun yx/insert-fixed-org-link ()
  "从 minibuffer 读取链接地址，在当前光标位置插入固定的 Org 链接。"
  (interactive)
  (let* ((link-address (read-string "Input Link: "))
         (link-text "🔗")
         (org-link-format (format "[[%s][%s]]" link-address link-text)))
    (insert org-link-format)))

;;;###autoload
(defun yx/org-toggle-inline-images-in-subtree (&optional beg end refresh)
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
    (dolist (ov overlays nil)
      (delete-overlay ov)
      (setq org-inline-image-overlays (delete ov org-inline-image-overlays)))
    (when (or refresh (not overlays))
      (org-display-inline-images t t beg end)
      t)))

;;; other-window+
;;;###autoload
(defmacro yx/with-other-window (&rest body)
  "Execute forms in BODY in the other-window."
  `(unless (one-window-p)
     (with-selected-window (other-window-for-scrolling)
       ,@body)))

;;;###autoload
(defun yx/isearch-other-window-forward (regexp-p)
  (interactive "P")
  (yx/with-other-window (isearch-forward regexp-p)))

;;;###autoload
(defun yx/isearch-other-window-backward (regexp-p)
  (interactive "P")
  (yx/with-other-window (isearch-backward regexp-p)))

;;;###autoload
(defun yx/ff-other-window ()
  "Find file in other window."
  (interactive)
  (cond
   ((one-window-p t)
    (call-interactively #'find-file-other-window))
   (t
    (yx/with-other-window (call-interactively #'find-file)))))

;;;###autoload
(defun yx/other-window-mru ()
  "Select the most recently used window on this frame."
  (interactive)
  (when-let ((mru-window
              (get-mru-window
               nil nil 'not-this-one-dummy)))
    (select-window mru-window)))

;;; skeleton+
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

(provide 'yx-extra)
;;; yx-extra.el ends here
