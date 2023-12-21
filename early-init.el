;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:15:44
;; Modified: <2023-12-21 00:26:14 yx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(setq
 inhibit-x-resources t
 inhibit-splash-screen t
 inhibit-startup-message t
 inhibit-compacting-font-caches t
 ffap-machine-p-known 'reject
 read-process-output-max (* 1024 1024)
 redisplay-skip-fontification-on-input t)

(setq
 load-prefer-newer t
 byte-compile-warnings nil
 package-native-compile t
 package-enable-at-startup nil
 native-compile-prune-cache t
 native-comp-jit-compilation t
 native-comp-async-report-warnings-errors nil)

(dolist (param '((width . 90)
                 (height . 36)
                 (undecorated-round . t)))
  (push param default-frame-alist))

(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(tooltip-mode    -1)
(tool-bar-mode   -1)
(menu-bar-mode   -1)
(scroll-bar-mode -1)
(fringe-mode '(8 . 12))

;; %% 文件句柄
(let ((old-file-name-handler-alist file-name-handler-alist))
  (setq-default file-name-handler-alist nil)
  (defun yx/restore-file-name-handler-alist ()
    (setq
     file-name-handler-alist
     (delete-dups (append file-name-handler-alist old-file-name-handler-alist))
     inhibit-trace nil))
  (add-hook #'emacs-startup-hook #'yx/restore-file-name-handler-alist))

(setq
 gc-cons-percentage 0.6
 gc-cons-threshold (* 128 1024 1024))
(add-hook
 'after-init-hook
 (lambda ()
   (setq
    gc-cons-percentage 0.1
    gc-cons-threshold (* 8 1024 1024))
   (garbage-collect)))

(setq-default
 inhibit-message t
 inhibit-redisplay t)
(add-hook
 'window-setup-hook
 (lambda ()
   (setq-default
    inhibit-message nil
    inhibit-redisplay nil)
   (redisplay)))

;; %% 设置eln缓存目录
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name ".cache/eln-cache" user-emacs-directory))))

(defun display-startup-echo-area-message ()
  (message nil))

;; %%
;;; early-init.el ends here
