;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:15:44
;; Modified: <2024-03-11 18:02:58 yx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(setq load-prefer-newer t
      inhibit-default-init t
      byte-compile-warnings nil
      package-native-compile t
      package-enable-at-startup nil
      native-comp-async-report-warnings-errors nil)

(push '(undecorated-round . t) default-frame-alist)
(push '(fullscreen . maximized) initial-frame-alist)
(when (featurep 'ns) (push '(ns-transparent-titlebar . t) default-frame-alist))

(tooltip-mode    -1)
(tool-bar-mode   -1)
(menu-bar-mode   -1)
(scroll-bar-mode -1)
(fringe-mode '(8 . 12))

(setq frame-inhibit-implied-resize t
      ffap-machine-p-known 'reject
      read-process-output-max (* 4 1024 1024)
      gc-cons-threshold most-positive-fixnum
      redisplay-skip-fontification-on-input t)

;; %% 文件句柄
(let ((old-file-name-handler-alist file-name-handler-alist))
  (setq-default file-name-handler-alist nil)
  (defun yx/restore-file-name-handler-alist ()
    (setq file-name-handler-alist
          (delete-dups (append file-name-handler-alist old-file-name-handler-alist))
          inhibit-trace nil))
  (add-hook #'emacs-startup-hook #'yx/restore-file-name-handler-alist))

;; %% 设置eln缓存目录
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name ".local/eln-cache" user-emacs-directory))))

(defun display-startup-echo-area-message ()
  (message nil))

;;; early-init.el ends here
