;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-09 00:40:03
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

(require 'x-common)

(defgroup x-scratch ()
  "Scratch buffers for editable major mode of choice."
  :group 'editing)

(defcustom yx-scratch-default-mode 'text-mode
  "Default major mode for `x-scratch-buffer'."
  :type 'symbol
  :group 'x-scratch)

(defun x-scratch--list-modes ()
  "List known major modes."
  (let (symbols)
    (mapatoms
     (lambda (symbol)
       (when (and (functionp symbol)
                  (or (provided-mode-derived-p symbol 'text-mode)
                      (provided-mode-derived-p symbol 'prog-mode)))
         (push symbol symbols))))
    symbols))

(defun x-scratch--insert-comment ()
  "Insert comment for major mode, if appropriate.
Insert a comment if `comment-start' is non-nil and the buffer is
empty."
  (when (and (x-common-empty-buffer-p) comment-start)
    (insert (format "Scratch buffer for: %s\n\n" major-mode))
    (goto-char (point-min))
    (comment-region (line-beginning-position) (line-end-position))))

(defun x-scratch--prepare-buffer (region &optional mode)
  "Add contents to scratch buffer and name it accordingly.

REGION is added to the contents to the new buffer.

Use the current buffer's major mode by default.  With optional
MODE use that major mode instead."
  (let ((major (or mode major-mode)))
    (with-current-buffer (pop-to-buffer (format "*scratch-%s*" major))
      (funcall major)
      (x-scratch--insert-comment)
      (goto-char (point-max))
      (unless (string-empty-p region)
        (when (x-common-line-regexp-p 'non-empty)
          (insert "\n\n"))
        (insert region)))))

(defvar yx-scratch--major-mode-history nil
  "Minibuffer history of `x-scratch--major-mode-prompt'.")

(defun x-scratch--major-mode-prompt ()
  "Prompt for major mode and return the choice as a symbol."
  (intern
   (completing-read "Select major mode: "
                    (x-scratch--list-modes)
                    nil
                    :require-match
                    nil
                    'yx-scratch--major-mode-history)))

(defun x-scratch--capture-region ()
  "Capture active region, else return empty string."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    ""))

;;;###autoload
(defun x-scratch-buffer (&optional arg)
  "Produce a scratch buffer matching the current major mode.

With optional ARG as a prefix argument (\\[universal-argument]),
use `yx-scratch-default-mode'.

With ARG as a double prefix argument, prompt for a major mode
with completion.  Candidates are derivatives of `text-mode' or
`prog-mode'.

If region is active, copy its contents to the new scratch
buffer.

Buffers are named as *MAJOR-MODE scratch*.  If one already exists
for the given MAJOR-MODE, any text is appended to it."
  (interactive "P")
  (let ((region (x-scratch--capture-region)))
    (pcase (prefix-numeric-value arg)
      (16 (x-scratch--prepare-buffer region (x-scratch--major-mode-prompt)))
      (4 (x-scratch--prepare-buffer region yx-scratch-default-mode))
      (_ (x-scratch--prepare-buffer region)))))

(provide 'x-scratch)
;;; x-scratch.el ends here
