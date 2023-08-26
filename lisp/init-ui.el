;;; init-ui.el --- ui  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:08:08
;; Modified: <2023-08-27 04:58:19 yx>
;; Licence: GPLv3

;;; Commentary:

;; ui

;;; Code:

;; %% font
(defvar yx/default-font "Cascadia Code PL")
(defvar yx/fixed-pitch-font "JetBrains Mono NL")
(defvar yx/fixed-pitch-serif-font "Latin Modern Mono")
(defvar yx/variable-pitch-font "Latin Modern Roman")

(defun yx/setup-fonts ()
  (set-face-attribute 'default nil :family yx/default-font :height 150)
  (set-face-attribute 'fixed-pitch nil :family yx/fixed-pitch-font)
  (set-face-attribute 'fixed-pitch-serif nil :family yx/fixed-pitch-serif-font)
  (set-face-attribute 'variable-pitch nil :family yx/variable-pitch-font)
  (setq face-font-rescale-alist '(("LXGW WenKai Mono" . 1.0)))
  (set-fontset-font t '(#x4e00 . #x9fff) "LXGW WenKai Mono")
  )

(if (daemonp)
    (add-hook
     'after-make-frame-functions
     (lambda (frame)
       (with-selected-frame frame (yx/setup-fonts))))
  (yx/setup-fonts))

;; %% theme
(add-hook
 'prog-mode-hook
 (lambda ()
   (setq buffer-face-mode-face 'fixed-pitch)
   (buffer-face-mode))
 )

(setq
 window-divider-default-bottom-width 1
 window-divider-default-places 'bottom-only)
(add-hook 'after-init-hook 'window-divider-mode)

(use-package ef-themes
  :init
  (setq
   ef-themes-headings
   '((0 . (variable-pitch 1.3))
     (1 . (regular 1.25))
     (2 . (variable-pitch 1.15))
     (3 . (1.1))
     (4 . (1.05))
     (agenda-date . (variable-pitch 1.25))
     (agenda-structure . (variable-pitch 1.25))
     (t . (t))))
  (ef-themes-select 'ef-light)
  )

;; %% modeline
(use-package minions
  :demand t
  :hook (after-init . minions-mode)
  )

(use-package awesome-tray
  :demand t
  :load-path "site-lisp/awesome-tray"
  :config
  (setq
   awesome-tray-active-modules
   '("evil" "belong" "location-or-page" "buffer-name" "mode-name" "git"))
  (awesome-tray-mode 1)
  )

;; %% end
(provide 'init-ui)
;;; init-ui.el ends here
