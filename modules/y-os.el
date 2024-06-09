;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 14:53:38
;; Modified: <2024-06-09 17:37:41 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(prefer-coding-system 'utf-8)
(set-language-environment 'UTF-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)

(cond
 (IS-MAC
  (setq ns-pop-up-frames nil
        ns-use-thin-smoothing t
        ns-use-native-fullscreen nil)
  (setq ns-command-modifier 'super
        ns-alternate-modifier 'meta
        ns-function-modifier  'hyper)
  (push '(fullscreen . maximized) initial-frame-alist)
  (push '(undecorated-round . t) default-frame-alist)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
 (IS-WIN
  (setq file-name-coding-system 'gbk)
  (set-selection-coding-system  'utf-16-le)
  (w32-set-system-coding-system 'utf-8)
  (setq w32-apps-modifier    'hyper
        w32-lwindow-modifier 'super
        w32-pass-apps-to-system nil
        w32-pass-lwindow-to-system nil
        w32-get-true-file-attributes nil
        w32-pipe-read-delay 0
        w32-pipe-buffer-size  (* 64 1024))
  (w32-register-hot-key [s-])
  (w32-register-hot-key [H-])
  (modify-coding-system-alist 'process "cmdproxy" '(gbk-dos . gbk-dos)))
 (IS-WSL
  (set-clipboard-coding-system 'gbk-dos)
  (appendq! exec-path '("/mnt/c/Windows/System32"))))

(provide 'y-os)
;;; y-os.el ends here
