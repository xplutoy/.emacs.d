;;; -*- coding: utf-8; lexical-binding: t; -*-
(setq
 use-dialog-box nil
 use-file-dialog nil
 inhibit-x-resources t
 inhibit-default-init t
 inhibit-splash-screen t
 inhibit-startup-message t
 inhibit-compacting-font-caches t
 frame-resize-pixelwise t
 frame-inhibit-implied-resize t
 ffap-machine-p-known 'reject
 read-process-output-max (* 1024 1024))

(setq
 byte-compile-warnings nil
 package-native-compile t
 package-enable-at-startup nil
 native-comp-jit-compilation t
 native-comp-async-report-warnings-errors nil)

(setq
 default-frame-alist
 '((width . 90)
   (height . 36)
   (tool-bar-lines . 0)
   (menu-bar-lines . 0)
   (undecorated-round . t)
   (vertical-scroll-bars . nil)))

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
    gc-cons-threshold (* 16 1024 1024))))

(tooltip-mode -1)

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name ".cache/eln-cache" user-emacs-directory))))

(defun display-startup-echo-area-message ()
  (message nil))
;;; early-init.el ends here
