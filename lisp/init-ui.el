;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:08:08
;; Modified: <2024-01-28 07:10:25 yx>
;; Licence: GPLv3

;;; Commentary:

;; ui

;;; Code:
(setq
 x-stretch-cursor nil
 x-underline-at-descent-line t)

(custom-set-faces
 '(mode-line ((t :box (:style released-button)))))

(when IS-MAC (menu-bar-mode 1))

;; %% font
(defvar yx/font-height 160)
(defvar yx/font "JetBrains Mono NL")
(defvar yx/fixed-font "JetBrains Mono NL")
(defvar yx/serif-font "Latin Modern Mono")
(defvar yx/variable-font "Latin Modern Roman")

(defun yx/setup-fonts ()
  (set-face-attribute 'default nil :family yx/font :height yx/font-height)
  (set-face-attribute 'fixed-pitch nil :family yx/fixed-font)
  (set-face-attribute 'fixed-pitch-serif nil :family yx/serif-font)
  (set-face-attribute 'variable-pitch nil :family yx/variable-font)
  (setq face-font-rescale-alist '(("LXGW WenKai Mono" . 1.0)))
  (set-fontset-font t '(#x4e00 . #x9fff) "LXGW WenKai Mono")

  (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
           when (member font (font-family-list))
           return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
  (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
           when (member font (font-family-list))
           return (set-fontset-font t 'emoji  (font-spec :family font) nil 'prepend))
  )

(add-hook 'window-setup-hook 'yx/setup-fonts)
(add-hook 'server-after-make-frame-hook 'yx/setup-fonts)

(setq-default fringes-outside-margins t)
(setq window-divider-default-bottom-width 1
      window-divider-default-places 'bottom-only)
(add-hook 'after-init-hook 'window-divider-mode)

(setq
 modus-themes-mixed-fonts t
 modus-themes-variable-pitch-ui t
 modus-themes-italic-constructs t
 modus-themes-common-palette-overrides
 '((fringe unspecifield)
   (bg-heading-1 bg-dim)
   (fg-heading-1 fg-main)
   (fg-line-number-active fg-main)
   (bg-line-number-active unspecifield)
   (fg-line-number-inactive "gray50")
   (bg-line-number-inactive unspecifield))
 )

(use-package ef-themes
  :init
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t))

(load-theme 'modus-operandi t)

(use-package lin
  :hook (after-init . lin-global-mode)
  :custom
  (lin-face 'lin-magenta))

(use-package minions
  :demand t
  :hook (after-init . minions-mode))

(use-package breadcrumb
  :hook ((prog-mode org-mode) . breadcrumb-local-mode))


(provide 'init-ui)
;;; init-ui.el ends here
