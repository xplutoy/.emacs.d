;;; init-misc.el --- misc  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 22:55:50
;; Modified: <2023-08-26 23:22:54 yx>
;; Licence: GPLv3

;;; Commentary:

;; misc

;;; Code:

;; %% misc
(use-package gcmh
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-high-cons-threshold (* 128 1024 1024))
  )

(use-package no-littering
  :init
  (setq no-littering-var-directory yx/var-dir
        no-littering-etc-directory yx/etc-dir
        )
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-var-directory))
    )
  :config
  (no-littering-theme-backups)
  (setq auto-insert-directory
        (expand-file-name "templates/" no-littering-etc-directory))
  )

(use-package server
  :defer 2
  :ensure nil
  :commands (server-running-p)
  :config (or (server-running-p) (server-mode)))

(when (featurep 'xwidget-internal)
  (use-package xwidget
    :ensure nil
    :bind (:map xwidget-webkit-mode-map
                ("W" . xwidget-webkit-fit-width))
    )
  )

(use-package posframe)
(use-package emacsql-sqlite-builtin)

;; %% auxiliary tool
(use-package crux-yx
  :defer 2
  :load-path "site-lisp/crux-yx"
  :config
  (add-hook
   'eww-mode-hook
   (lambda ()
     (setq-local
      imenu-create-index-function
      'unpackaged/imenu-eww-headings))
   )
  )

(use-package which-key
  :defer 2
  :init
  (setq
   which-key-idle-delay 1.5
   which-key-show-early-on-C-h t
   which-key-show-remaining-keys t
   which-key-idle-secondary-delay 0.05)
  :config
  (which-key-setup-minibuffer)
  (which-key-mode 1)
  )

(use-package goggles
  :hook ((text-mode prog-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)
  )


;; %% edit enhencement
(use-package avy
  :init
  (setq avy-style 'at
        avy-timeout-seconds 0.8)
  :bind (:map isearch-mode-map
              ("C-'" . avy-isearch))
  )

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark))
  )

(use-package vundo)

;; %% spell
(use-package jinx
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (setq jinx-languages "en_US")
  )

(use-package flyspell-correct
  :after flyspell
  )

;; %% chinese
(use-package cal-china-x
  :defer 2
  :config
  (setq cal-china-x-general-holidays
        '((holiday-lunar 1 15   "元宵节")
          (holiday-fixed 3 8    "妇女节")
          (holiday-fixed 3 12   "植树节")
          (holiday-fixed 5 4    "青年节")
          (holiday-fixed 6 1    "儿童节")
          (holiday-lunar 7 7    "七夕节")
          (holiday-lunar 9 9    "重阳节")
          (holiday-fixed 9 10   "教师节"))
        holiday-other-holidays
        '((holiday-fixed 2 14   "情人节")
          (holiday-fixed 4 1    "愚人节")
          (holiday-fixed 12 25  "圣诞节")
          (holiday-float 5 0 2  "母亲节")
          (holiday-float 6 0 3  "父亲节")
          (holiday-float 11 4 4 "感恩节"))
        )
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq calendar-holidays (append
                           holiday-other-holidays
                           cal-china-x-general-holidays
                           cal-china-x-important-holidays))
  )

(use-package osx-dictionary
  :if -is-mac
  :bind (("C-c D" . osx-dictionary-search-input)
         ("C-c d" . osx-dictionary-search-pointer))
  )

(use-package fanyi
  :unless -is-mac
  :bind (("C-c D" . fanyi-dwim)
         ("C-c d" . fanyi-dwim2))
  :custom
  (fanyi-providers '(fanyi-haici-provider))
  )

;; %% end
(provide 'init-misc)
;;; init-misc.el ends here
