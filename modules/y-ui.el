;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 12:17:53
;; Licence: GPLv3

;;; Commentary:

;; 字体、主题等

;;; Code:

(defvar yx/font-h 140)
(defvar yx/d-font "JetBrains Mono")
(defvar yx/f-font "IBM Plex Mono")
(defvar yx/s-font "IBM Plex Serif")
(defvar yx/v-font "IBM Plex Sans")

(defun yx/font-and-theme-setup ()
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'default nil :family yx/d-font :height yx/font-h)
        (set-face-attribute 'fixed-pitch nil :family yx/f-font)
        (set-face-attribute 'fixed-pitch-serif nil :family yx/s-font)
        (set-face-attribute 'variable-pitch nil :family yx/v-font)
        (setq face-font-rescale-alist '(("LXGW WenKai Mono"  . 1.0)))
        (cl-loop for font in '("LXGW WenKai Mono" "Microsoft Yahei" "PingFang SC")
                 when (x-list-fonts font)
                 return (set-fontset-font t 'han (font-spec :family font)))
        (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
                 when (x-list-fonts font)
                 return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
        (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
                 when (x-list-fonts font)
                 return (set-fontset-font t 'emoji  (font-spec :family font) nil 'prepend))
        ;; (load-theme 'ef-melissa-light t))
        (load-theme 'modus-operandi t))
    (load-theme 'modus-vivendi t)))

(add-hook 'after-init-hook #'yx/font-and-theme-setup -100)
(add-hook 'server-after-make-frame-hook #'yx/font-and-theme-setup -100)

(use-package modus
  :ensure nil
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-fringes 'subtle)
  (modus-themes-subtle-line-numbers t)
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-variable-pitch-ui t))

(use-package ef-themes
  :init
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t))

(use-package x-modeline
  :ensure nil
  :demand t
  :config
  (setq mode-line-right-align-edge 'right-margin)
  (setq-default mode-line-format
                '("%e"
                  x-modeline-kbd-macro
                  x-modeline-narrow
                  x-modeline-buffer-status
                  x-modeline-window-dedicated-status
                  x-modeline-input-method
                  "  "
                  x-modeline-buffer-identification
                  "  "
                  x-modeline-major-mode
                  x-modeline-process
                  "  "
                  x-modeline-vc-branch
                  "  "
                  x-modeline-eglot
                  "  "
                  x-modeline-flymake
                  "  "
                  mode-line-format-right-align
                  "  "
                  x-modeline-misc-info
                  "  "
                  mode-line-end-spaces)))

(use-package breadcrumb
  :demand t
  :hook (after-init . breadcrumb-mode))

(use-package spacious-padding
  :hook (after-init . spacious-padding-mode)
  :custom
  (spacious-padding-subtle-mode-line t)
  (spacious-padding-widths '( :internal-border-width 4
                              :header-line-width 2
                              :mode-line-width 2
                              :tab-width 4
                              :right-divider-width 16
                              :fringe-width 8)))

(use-package lin
  :defer 5
  :custom
  (lin-face 'lin-magenta)
  :config (lin-global-mode +1))

(use-package minions
  :disabled
  :hook (after-init . minions-mode))

(use-package mlscroll
  :hook ((after-init . mlscroll-mode)
         (server-after-make-frame . mlscroll-mode)))

(provide 'y-ui)
;;; y-ui.el ends here
