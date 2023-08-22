;;; -*- coding: utf-8; lexical-binding: t; -*-
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

(add-hook
 'prog-mode-hook
 (lambda ()
   (setq buffer-face-mode-face 'fixed-pitch)
   (buffer-face-mode))
 )

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

(use-package minions
  :hook (after-init . minions-mode)
  )

(use-package awesome-tray
  :load-path "site-lisp/awesome-tray"
  :demand
  :config
  (setq
   awesome-tray-active-modules
   '("evil" "belong" "location-or-page" "buffer-name" "mode-name" "git"))
  (awesome-tray-mode 1)
  )

(provide 'init-ui)
