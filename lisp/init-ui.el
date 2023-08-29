;;; init-ui.el --- ui  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:08:08
;; Modified: <2023-08-29 06:47:57 yx>
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

(setq
 modus-themes-headings
 '((agenda-date . (variable-pitch 1.3))
   (agenda-structure . (variable-pitch 1.8))
   (t . (regular 1.15)))
 modus-themes-italic-constructs t
 modus-themes-themes-common-palette-overrides
 '((fringe unspecifield)
   (fg-line-number-active fg-main)
   (bg-line-number-active unspecifield)
   (fg-line-number-inactive "gray50")
   (bg-line-number-inactive unspecifield))
 )

(custom-set-faces
 '(mode-line ((t :box (:style released-button)))))

(require-theme 'modus-themes)

(use-package circadian
  :demand t
  :custom
  (circadian-themes '(("19:30" . modus-vivendi)
                      ("06:39" . modus-operandi)))
  :config
  (add-hook 'circadian-after-load-theme-hook
            (lambda (dummy)
              (awesome-tray-enable)))
  (circadian-setup)
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
