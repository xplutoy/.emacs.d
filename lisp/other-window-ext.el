;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-05-30 09:38:50
;; Modified: <>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

;;;###autoload
(defmacro with-other-window (&rest body)
  "Execute forms in BODY in the other-window."
  `(unless (one-window-p)
     (with-selected-window (other-window-for-scrolling)
       ,@body)))

;;;###autoload
(defun owe-isearch-other-window-forward (regexp-p)
  (interactive "P")
  (with-other-window (isearch-forward regexp-p)))

;;;###autoload
(defun owe-isearch-other-window-backward (regexp-p)
  (interactive "P")
  (with-other-window (isearch-backward regexp-p)))

;;;###autoload
(defun owe-ff-other-window ()
  "Find file in other window."
  (interactive)
  (cond
   ((one-window-p t)
    (call-interactively #'find-file-other-window))
   (t
    (with-other-window (call-interactively #'find-file)))))

;;;###autoload
(defun owe-other-window-mru ()
  "Select the most recently used window on this frame."
  (interactive)
  (when-let ((mru-window
              (get-mru-window
               nil nil 'not-this-one-dummy)))
    (select-window mru-window)))

(provide 'other-window-ext)
;;; other-window-ext.el ends here
