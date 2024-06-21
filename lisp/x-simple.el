;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-01 19:19:56
;; Modified: <2024-06-21 10:52:20 yangx>
;; Licence: GPLv3

;;; Commentary:

;; 大部分借自 Protesilaos Stavrou.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'x-common)
(require 'skeleton)

(defgroup x-simple ()
  "Generic utilities."
  :group 'editing)

(defcustom x-simple-date-specifier "%F"
  "Date specifier for `format-time-string'."
  :type 'string
  :group 'x-simple)

(defcustom x-simple-time-specifier "%R"
  "Time specifier for `format-time-string'."
  :type 'string
  :group 'x-simple)

;;;###autoload
(defun x-simple-new-line-below (n)
  "Create N empty lines below the current one.
When called interactively without a prefix numeric argument, N is
1."
  (interactive "p")
  (goto-char (line-end-position))
  (dotimes (_ n) (insert "\n")))

;;;###autoload
(defun x-simple-new-line-above (n)
  "Create N empty lines above the current one.
When called interactively without a prefix numeric argument, N is
1."
  (interactive "p")
  (let ((point-min (point-min)))
    (if (or (bobp)
            (eq (point) point-min)
            (eq (line-number-at-pos point-min) 1))
        (progn
          (goto-char (line-beginning-position))
          (dotimes (_ n) (insert "\n"))
          (forward-line (- n)))
      (forward-line (- n))
      (x-simple-new-line-below n))))

;;;###autoload
(defun x-simple-copy-line ()
  "Copy the current line to the `kill-ring'."
  (interactive)
  (copy-region-as-kill (line-beginning-position) (line-end-position)))

;;;###autoload
(defun x-simple-replace-line-or-region ()
  "Replace line or region with latest kill.
This command can then be followed by the standard
`yank-pop' (default is bound to \\[yank-pop])."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (line-beginning-position) (line-end-position)))
  (yank))

;;;###autoload
(defun x-simple-insert-date (&optional arg)
  "Insert the current date as `x-simple-date-specifier'.

With optional prefix ARG (\\[universal-argument]) also append the
current time understood as `x-simple-time-specifier'.

When region is active, delete the highlighted text and replace it
with the specified date."
  (interactive "P")
  (let* ((date x-simple-date-specifier)
         (time x-simple-time-specifier)
         (format (if arg (format "%s %s" date time) date)))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (insert (format-time-string format))))

;;;###autoload
(defun x-simple-copy-current-buffer-name ()
  "Add the current buffer's name to the `kill-ring'."
  (declare (interactive-only t))
  (interactive)
  (kill-new (buffer-name (current-buffer))))

;;;###autoload
(defun x-simple-copy-current-buffer-file ()
  "Add the current buffer's file path to the `kill-ring'."
  (declare (interactive-only t))
  (interactive)
  (if buffer-file-name
      (kill-new buffer-file-name)
    (user-error "%s is not associated with a file"
                (buffer-name (current-buffer)))))

;;;###autoload
(defun x-simple-kill-buffer-current (&optional arg)
  "Kill current buffer.
With optional prefix ARG (\\[universal-argument]) delete the
buffer's window as well.  Kill the window regardless of ARG if it
satisfies `x-common-window-small-p' or it has no previous
buffers in its history."
  (interactive "P")
  (let ((kill-buffer-query-functions nil))
    (if (or (and (x-common-window-small-p)
                 (null (window-prev-buffers)))
            (and (not (one-window-p)) arg))
        (kill-buffer-and-window)
      (kill-buffer))))

;;;###autoload
(defun x-simple-comment-dwim (n)
  "Comment N lines, defaulting to the current one.
When the region is active, comment its lines instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line n)))

;;;###autoload
(defun x-simple-fill-dwim ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command #'x-simple-fill-dwim)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

;;;###autoload
(defun x-simple-begin-of-line-dwim ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

;;;###autoload
(defun x-simple-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename delete-by-moving-to-trash)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

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
(defun x-simple-selective-display (&optional level)
  "Fold text indented same of more than the cursor.

This function toggles folding according to the level of
indentation at point. It's convenient not having to specify a
number nor move point to the desired column.
"
  (interactive "P")
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (or level (1+ (current-column))))))

(provide 'x-simple)
;;; x-simple.el ends here
