;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 01:23:27
;; Modified: <2024-06-09 00:23:07 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(require 'common-x)
(require 'elfeed nil t)

(defgroup elfeed-x ()
  "Personal extensions for Elfeed."
  :group 'elfeed)

;;;###autoload
(defun yx/elfeed-search-print-entry (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (date-width (car (cdr elfeed-search-date-format)))
         (title (concat (or (elfeed-meta entry :title)
                            (elfeed-entry-title entry) "")
                        ;; NOTE: insert " " for overlay to swallow
                        " "))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title (when feed (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat (lambda (s) (propertize s 'face 'elfeed-search-tag-face)) tags ","))
         (title-width (- (frame-width)
                         ;; (window-width (get-buffer-window (elfeed-search-buffer) t))
                         date-width elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width) :left))


         ;; Title/Feed ALIGNMENT
         (align-to-feed-pixel (+ date-width
                                 (max elfeed-search-title-min-width
                                      (min title-width elfeed-search-title-max-width)))))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (insert (propertize title-column 'face title-faces 'kbd-help title))
    (put-text-property (1- (point)) (point) 'display `(space :align-to ,align-to-feed-pixel))
    (when feed-title (insert " " (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when tags (insert "(" tags-str ")"))))

;;;###autoload
(defun yx/elfeed-kill-entry ()
  "Like `elfeed-kill-entry' but pop elfeed search"
  (interactive)
  (elfeed-kill-buffer)
  (switch-to-buffer "*elfeed-search*"))

;;;###autoload
(defun yx/elfeed-tag-as (tag)
  (lambda ()
    "Toggle a tag on an Elfeed search selection"
    (interactive)
    (elfeed-search-toggle-all tag)))

;;;###autoload
(defun yx/elfeed-show-in-xwidget (&optional generic)
  (interactive "P")
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (if generic
          (browse-url-generic link)
        (xwidget-webkit-browse-url link)))))

(provide 'elfeed-x)
;;; elfeed-x.el ends here
