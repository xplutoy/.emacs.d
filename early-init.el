;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:15:44
;; Modified: <2024-03-11 18:02:58 yx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(setq package-enable-at-startup nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(undecorated-round . t) default-frame-alist)
(push '(fullscreen . maximized) initial-frame-alist)
(when (featurep 'ns) (push '(ns-transparent-titlebar . t) default-frame-alist))

(tooltip-mode    -1)
(fringe-mode '(8 . 12))

(setq mode-line-format nil
      byte-compile-warnings nil
      ffap-machine-p-known 'reject
      read-process-output-max (* 4 1024 1024)
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0
      native-compile-prune-cache t
      native-comp-async-report-warnings-errors 'silent)

(let ((old-file-name-handler-alist file-name-handler-alist))
  (setq-default file-name-handler-alist nil)
  (defun yx/emacs-startup-post-h ()
    (setq gc-cons-threshold (* 16 1024 1024)
          gc-cons-percentage 0.1
          file-name-handler-alist (delete-dups (append file-name-handler-alist old-file-name-handler-alist)))
    (run-with-idle-timer 5 t #'garbage-collect)
    (message "Ready in %s with %d garbage collections."
             (format "%.2f seconds" (float-time
                                     (time-subtract after-init-time before-init-time)))
             gcs-done))
  (add-hook 'emacs-startup-hook #'yx/emacs-startup-post-h))

;; %% 设置eln缓存目录
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name ".local/eln-cache" user-emacs-directory))))

(advice-add #'display-startup-screen :override #'ignore)
(advice-add #'display-startup-echo-area-message :override #'ignore)

;;; early-init.el ends here
