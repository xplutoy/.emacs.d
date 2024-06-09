;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-08 23:21:39
;; Modified: <2024-06-09 16:40:49 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(require 'common-x)

;;;###autoload
(defun yx/window-delete-dwim ()
  "Delete the current window or bury its buffer.
If the current window is alone in its frame, bury the buffer
instead."
  (interactive)
  (unless (ignore-errors (delete-window) t)
    (bury-buffer)))

;;;###autoload
(defmacro yx/window-with-other (&rest body)
  "Execute forms in BODY in the other-window."
  `(unless (one-window-p)
     (with-selected-window (other-window-for-scrolling)
       ,@body)))

;;;###autoload
(defun yx/window-other-isearch-forward (regexp-p)
  (interactive "P")
  (yx/window-with-other (isearch-forward regexp-p)))

;;;###autoload
(defun yx/window-other-isearch-backward (regexp-p)
  (interactive "P")
  (yx/window-with-other (isearch-backward regexp-p)))

;;;###autoload
(defun yx/window-other-find-file ()
  "Find file in other window."
  (interactive)
  (cond
   ((one-window-p t)
    (call-interactively #'find-file-other-window))
   (t
    (yx/window-with-other (call-interactively #'find-file)))))

;;;###autoload
(defun yx/window-other-mru ()
  "Select the most recently used window on this frame."
  (interactive)
  (when-let ((mru-window
              (get-mru-window
               nil nil 'not-this-one-dummy)))
    (select-window mru-window)))

;;;###autoload
(defun yx/window-shell-or-term-p (buffer &rest _)
  "Check if BUFFER is a shell or terminal.
This is a predicate function for `buffer-match-p', intended for
use in `display-buffer-alist'."
  (when (string-match-p "\\*.*\\(e?shell\\|v?term\\).*" (buffer-name (get-buffer buffer)))
    (with-current-buffer buffer
      (and (not (derived-mode-p 'message-mode 'text-mode))
           (derived-mode-p 'eshell-mode 'shell-mode 'comint-mode 'fundamental-mode)))))

;;;###autoload
(defun yx/window-display-buffer-below-or-pop (&rest args)
  "Display buffer below current window or pop a new window.
The criterion for choosing to display the buffer below the
current one is a non-nil return value for
`yx/common-window-small-p' or `yx/common-three-or-more-windows-p'.

Apply ARGS expected by the underlying `display-buffer' functions.

This as the action function in a `display-buffer-alist' entry."
  (let ((functions (list
                    #'display-buffer-reuse-mode-window
                    (if (or (yx/common-window-small-p)
                            (yx/common-three-or-more-windows-p))
                        #'display-buffer-below-selected
                      #'display-buffer-pop-up-window))))
    (catch 'success
      (dolist (fn functions)
        (when (apply fn args)
          (throw 'success fn))))))

(provide 'window-x)
;;; window-x.el ends here
