;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:08:08
;; Modified: <2023-12-04 09:24:16 yx>
;; Licence: GPLv3

;;; Commentary:

;; ui

;;; Code:
(menu-bar-mode -1)
(when IS-MAC
  (add-hook 'after-init-hook 'menu-bar-mode))

;; %% font
(defvar yx/default-font "JetBrains Mono NL")
(defvar yx/fixed-pitch-font "JetBrains Mono NL")
(defvar yx/fixed-pitch-serif-font "Latin Modern Mono")
(defvar yx/variable-pitch-font "Latin Modern Roman")

(defun yx/setup-fonts ()
  (set-face-attribute 'default nil :family yx/default-font :height 160)
  (set-face-attribute 'fixed-pitch nil :family yx/fixed-pitch-font)
  (set-face-attribute 'fixed-pitch-serif nil :family yx/fixed-pitch-serif-font)
  (set-face-attribute 'variable-pitch nil :family yx/variable-pitch-font)
  (setq face-font-rescale-alist '(("LXGW WenKai Mono" . 1.0)))
  (set-fontset-font t '(#x4e00 . #x9fff) "LXGW WenKai Mono")
  )

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (yx/setup-fonts))))
  (yx/setup-fonts))

(setq-default fringes-outside-margins t)
(setq window-divider-default-bottom-width 1
      window-divider-default-places 'bottom-only)
(add-hook 'after-init-hook 'window-divider-mode)

(setq x-underline-at-descent-line t)

(setq
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

(use-package ef-themes)

(custom-set-faces
 '(mode-line ((t :box (:style released-button)))))

(use-package circadian
  :after solar
  :demand t
  :config
  (setq
   circadian-themes
   '((:sunset  . modus-vivendi)
     (:sunrise . modus-operandi)))
  (circadian-setup)
  )

;; %% modeline
(use-package minions
  :disabled
  :demand t
  :hook (after-init . minions-mode)
  )

(use-package mini-echo
  :demand t
  :custom
  (mini-echo-separator "|")
  (mini-echo-default-segments
   (quote (:short
           ("buffer-name-short" "profiler"
            "selection-info" "narrow" "macro" "evil")
           :long
           ("major-mode" "buffer-name-short" "vcs" "flymake"
            "selection-info" "narrow" "macro" "profiler" "elgot" "evil"))))
  :config
  (mini-echo-mode 1)
  )

(use-package breadcrumb
  :hook ((prog-mode org-mode) . breadcrumb-local-mode))


(provide 'init-ui)
;;; init-ui.el ends here
