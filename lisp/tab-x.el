;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-09 00:00:34
;; Modified: <2024-06-09 00:02:15 yangx>
;; Licence: GPLv3

;;; Commentary:

;; Functions for tabbar and tab-line.

;;; Code:
(require 'common-x)

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

(provide 'tab-x)
;;; tab-x.el ends here
