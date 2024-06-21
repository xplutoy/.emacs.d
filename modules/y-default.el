;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 14:32:39
;; Modified: <2024-06-21 10:46:00 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package emacs
  :ensure nil
  :custom
  (max-lisp-eval-depth 10000)
  (load-prefer-newer t)
  (case-fold-search nil)
  (user-full-name "yangxue")
  (system-time-locale "C")
  (use-dialog-box nil)
  (use-file-dialog nil)
  (tab-width 8)
  (visible-bell nil)
  (fill-column 89)
  (resize-mini-windows t)
  (enable-recursive-minibuffers t)
  (long-line-threshold 1000)
  (large-hscroll-threshold 1000)
  (syntax-wholeline-max 1000)
  (cursor-in-non-selected-windows nil)
  (x-stretch-cursor nil)
  (x-underline-at-descent-line t)
  (auto-save-no-message t)
  (word-wrap t)
  (word-wrap-by-category t)
  (truncate-lines t)
  (abbrev-mode t)
  (use-short-answers t)
  (delete-by-moving-to-trash t)
  (window-resize-pixelwise t)
  (window-combination-resize t)
  (frame-resize-pixelwise t)
  (frame-inhibit-implied-resize t)
  (redisplay-skip-fontification-on-input t)
  (kill-buffer-delete-auto-save-files t)
  (bidi-inhibit-bpa t)
  (bidi-display-reordering 'left-to-right)
  (bidi-paragraph-direction 'left-to-right)
  (hscroll-margin 2)
  (hscroll-step 1)
  (auto-window-vscroll nil)
  (auto-hscroll-mode 'current-line)
  (scroll-step 1)
  (scroll-margin 0)
  (scroll-conservatively 10)
  (fast-but-imprecise-scrolling t)
  (scroll-preserve-screen-position t)
  (inhibit-compacting-font-caches t)
  (debug-on-error init-file-debug) ;; --debug-init
  (other-window-scroll-default
   (lambda ()
     (or (get-mru-window nil nil 'not-this-one-dummy)
         (next-window)
         (next-window nil nil 'visible))))
  :init
  (setq disabled-command-function nil)
  (setq sentence-end-double-space nil
        sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
  (setq custom-file (nol-expand-etc "custom.el"))
  (setq duplicate-line-final-position -1)
  (setq duplicate-region-final-position -1)
  (setq-default tab-always-indent 'complete))

(use-package simple
  :ensure nil
  :custom
  (save-interprogram-paste-before-kill t)
  (completion-show-help nil)
  (idle-update-delay 1.0)
  (shell-command-prompt-show-cwd t)
  (async-shell-command-display-buffer nil)
  (kill-whole-line nil)
  (track-eol t)
  (line-move-visual nil)
  (indent-tabs-mode nil)
  (blink-matching-paren nil)
  (truncate-partial-width-windows 78)
  (set-mark-command-repeat-pop t)
  (remote-file-name-inhibit-auto-save t)
  (read-extended-command-predicate 'command-completion-default-include-p)
  (backward-delete-char-untabify-method 'hungry))

(use-package files
  :ensure nil
  :custom
  (enable-local-variables :all)
  (view-read-only t)
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 5)
  (save-silently t)
  (auto-save-default t)
  (auto-save-timeout 10)
  (auto-save-visited-interval 15)
  (find-file-visit-truename t)
  (confirm-kill-processes nil)
  (confirm-nonexistent-file-or-buffer nil)
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-cache nil)
  (remote-file-name-access-timeout 2)
  (user-mail-address "yangxue.cs@foxmail.com"))

(use-package startup
  :ensure nil
  :custom
  (initial-scratch-message nil)
  (initial-major-mode 'fundamental-mode)
  (inhibit-default-init t)
  (inhibit-splash-screen t)
  (inhibit-startup-message t)
  (inhibit-startup-buffer-menu t))

(use-package hl-line
  :ensure nil
  :hook ((prog-mode
          occur-mode
          dired-mode
          ibuffer-mode
          org-agenda-mode) . hl-line-mode)
  :custom
  (hl-line-sticky-flag nil)
  (global-hl-line-sticky-flag nil))

(use-package paren
  :ensure nil
  :hook (prog-mode . show-paren-mode)
  :custom
  (show-paren-style 'parenthesis)
  (show-paren-context-when-offscreen t)
  (show-paren-when-point-inside-paren t))

(use-package elec-pair
  :defer 5
  :custom
  (electric-pair-preserve-balance t)
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :config
  (electric-pair-mode +1))

(use-package jit-lock
  :ensure nil
  :custom
  (jit-lock-defer-time 0)
  (jit-lock-stealth-time 2.0)
  (jit-lock-stealth-nice 0.2)
  (jit-lock-chunk-size (* (window-body-height) fill-column)))

(use-package autoinsert
  :ensure nil
  :custom
  (auto-insert-query nil)
  (auto-insert-alist nil)
  (auto-insert-directory (nol-expand-etc "templates/"))
  :config
  (x-auto-header-minor-mode +1))
(use-package time
  :ensure nil
  :defer 3
  :custom
  (display-time-24hr-format t)
  (display-time-format "%a %e %b, %H:%M")
  (display-time-default-load-average nil)
  (zoneinfo-style-world-list '(("UTC" "UTC")
                               ("Asia/Tokyo" "Tokyo")
                               ("Asia/Shanghai" "Shanghai")
                               ("Asia/Singapore" "Singapore")
                               ("Europe/Paris" "Paris")
                               ("Europe/London" "London")
                               ("America/New_York" "New York")))
  :config
  (display-time-mode +1))

(use-package world-clock
  :ensure nil
  :custom
  (world-clock-time-format "%R %z (%Z)	%a %d %B"))

(use-package time-stamp
  :ensure nil
  :hook (before-save . time-stamp)
  :init
  (setopt time-stamp-pattern "10/^[@#;\*].*[Mm]odified: <%%>$"))

(use-package tempo
  :ensure nil
  :commands (tempo-define-template)
  :custom
  (tempo-interactive t))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-idle-delay 0.3)
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-display-truncation-message nil))

(use-package help
  :ensure nil
  :custom
  (help-enable-symbol-autoload t)
  (help-window-select t)
  (help-window-keep-selected t)
  (help-enable-variable-value-editing t)
  :config
  ;; (add-hook 'help-fns-describe-function-functions
  ;; #'shortdoc-help-fns-examples-function))
  )

(use-package flyspell
  :ensure nil
  :custom
  (ispell-dictionary "en_US")
  (ispell-program-name "aspell")
  (ispell-following-word t)
  (ispell-alternate-dictionary
   (nol-expand-etc "google-10000-english-no-swears.txt"))
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
  (flyspell-issue-message-flag nil)
  :config
  (keymap-unset flyspell-mode-map "C-,")
  (keymap-unset flyspell-mode-map "C-.")
  (keymap-unset flyspell-mode-map "C-;"))

(use-package dictionary
  :ensure nil
  :custom
  (dictionary-server "dict.org")
  (dictionary-default-popup-strategy "lev")
  (dictionary-create-buttons nil)
  (dictionary-display-definition-function #'dictionary-display-definition-in-help-buffer))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-strip-common-suffix t)
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-dirname-transform 'project-uniquify-dirname-transform))

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode conf-mode) . display-line-numbers-mode)
  :custom
  (display-line-numbers-width 3)
  (display-line-numbers-widen t)
  (display-line-numbers-major-tick 20)
  (display-line-numbers-width-start t))

(use-package re-builder
  :ensure nil
  :custom (reb-re-syntax 'string))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup 300)
  (recentf-max-saved-items 50)
  (recentf-exclude '("\\.?cache.*" "^/.*" "^/ssh:" "\\.git/.+$"
                     "COMMIT_MSG" "COMMIT_EDITMSG" "/Downloads/" "/elpa/"
                     "\\.\\(?:gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                     "\\.\\(?:gz\\|zip\\|gpg\\)$"
                     file-remote-p))
  :config
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  ;; Text properties inflate the size of recentf's files
  (add-to-list 'recentf-filename-handlers #'substring-no-properties))

(use-package subword
  :ensure nil
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

(use-package whitespace
  :ensure nil
  :hook ((prog-mode conf-mode) . whitespace-mode)
  :custom
  (whitespace-style '(face trailing space-before-tab space-after-tab)))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package savehist
  :ensure nil
  :demand t
  :custom
  (history-delete-duplicates t)
  (savehist-autosave-interval nil)
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables '(kill-ring
                                   mark-ring
                                   global-mark-ring
                                   register-alist
                                   command-history))
  :config
  (savehist-mode +1))

(use-package desktop
  :ensure nil
  :custom
  (desktop-save t)
  (desktop-restore-eager 5)
  (desktop-auto-save-timeout 60)
  :config
  (appendq! desktop-modes-not-to-save '(dired-mode
                                        eww-mode
                                        comint-mode
                                        elfeed-search-mode
                                        doc-view-mode
                                        Info-mode
                                        info-lookup-mode
                                        magit-mode
                                        magit-log-mode))
  (desktop-save-mode +1))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil)
  (revert-without-query '(".*"))
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t))

(use-package so-long
  :ensure nil
  :defer 5
  :config
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  (appendq! so-long-minor-modes '(eldoc-mode
                                  highlight-numbers-mode
                                  ws-butler-mode
                                  indent-guide-mode)))

(use-package grep
  :ensure nil
  :custom
  (grep-use-headings t)
  :config
  (when-let ((rg (executable-find "rg")))
    (setq grep-program rg))
  (appendq! grep-find-ignored-directories '("var" "node_modules"))
  (appendq! grep-find-ignored-files '("*.mp3" "*.mp4" "*.jpg" "*.gz")))

(use-package minibuffer
  :ensure nil
  :custom
  (completions-format 'one-column)
  (completions-max-height 30)
  (completions-header-format nil)
  (completion-auto-help 'visible)
  (completion-show-inline-help nil)
  (completion-auto-select 'second-tab)
  :config
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (file-name-shadow-mode +1)
  (minibuffer-depth-indicate-mode +1)
  (minibuffer-electric-default-mode +1))

(use-package completion-preview
  :when (>= emacs-major-version 30)
  :ensure nil
  :hook ((prog-mode text-mode comint-mode) . completion-preview-mode)
  :config
  (setopt completion-preview-minimum-symbol-length 2)
  (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
  (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert))

(use-package abbrev
  :ensure nil
  :custom
  (abbrev-suggest t)
  (save-abbrevs 'silently)
  (abbrev-suggest-hint-threshold 2))

(use-package dabbrev
  :ensure nil
  :custom
  (dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (dabbrev-upcase-means-case-search t)
  (dabbrev-ignored-buffer-modes '(archive-mode
                                  image-mode
                                  docview-mode
                                  pdf-view-mode)))

(use-package hippie-exp
  :ensure nil
  :custom
  (hippie-expand-max-buffers 5)
  (hippie-expand-try-functions-list '(try-complete-file-name-partially
                                      try-expand-dabbrev
                                      try-expand-dabbrev-from-kill
                                      try-expand-dabbrev-all-buffers
                                      try-expand-all-abbrevs
                                      try-expand-list
                                      try-expand-line
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol)))

(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
              ([remap isearch-delete-char] . isearch-del-char)
              ([remap isearch-abort] . isearch-cancel))
  :custom
  (isearch-lazy-count t)
  (isearch-lazy-highlight t)
  (isearch-allow-motion t)
  (isearch-motion-changes-direction t)
  (isearch-repeat-on-direction-change t)
  :config
  (keymap-set isearch-mode-map "M->" 'isearch-end-of-buffer)
  (keymap-set isearch-mode-map "M-<" 'isearch-beginning-of-buffer)

  (if IS-MAC
      (keymap-set isearch-mode-map "s-v" 'isearch-yank-kill)))

(use-package epa
  :ensure nil
  :custom
  (epa-pinentry-mode 'loopback)
  (epa-file-select-keys nil)
  (epa-keys-select-method 'minibuffer)
  (epa-file-encrypt-to user-mail-address)
  :config
  (epa-file-enable))

(use-package auth-source
  :ensure nil
  :autoload (auth-source-search)
  :custom
  (auth-source-debug t)
  (auth-source-cache-expiry 7200)
  :config
  (add-to-list 'auth-sources
               (nol-expand-etc "authinfo.gpg")))

(use-package mouse
  :ensure nil
  :custom
  (mouse-yank-at-point t)
  (mouse-drag-mode-line-buffer t)
  (mouse-drag-and-drop-region-cross-program t))

(use-package mwheel
  :ensure nil
  :custom
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-scroll-amount-horizontal 2)
  (mouse-wheel-scroll-amount '(2 ((shift) . hscroll))))

(use-package transient
  :ensure nil
  :commands (transient-define-prefix)
  :custom
  (transient-detect-key-conflicts nil)
  (transient-highlight-mismatched-keys nil)
  :config
  (transient-bind-q-to-quit))

(use-package bookmark
  :ensure nil
  :hook (bookmark-bmenu-mode . hl-line-mode)
  :custom
  (bookmark-save-flag 1)
  (bookmark-fringe-mark nil)
  (bookmark-use-annotations nil)
  (bookmark-automatically-show-annotations nil))

(use-package proced
  :ensure nil
  :custom
  (proced-descend t)
  (proced-filter 'user)
  (proced-enable-color-flag t)
  (proced-auto-update-flag nil))

(use-package server
  :ensure nil
  :defer 1
  :custom
  (server-client-instructions nil)
  :config
  (unless (server-running-p)
    (server-mode)))

(use-package midnight-mode
  :ensure nil
  :defer 10
  :config
  (setq midnight-period 7200)
  (midnight-mode +1))

(use-package appt
  :ensure nil
  :defer 10
  :custom
  (appt-audible t)
  (appt-display-interval 10)
  (appt-display-duration 5)
  (appt-display-format 'window)
  (appt-message-warning-time 2)
  :config
  (with-silent
   (appt-activate)))

(use-package calendar
  :ensure nil
  :defer 10
  :hook (calendar-today-visible . calendar-mark-today)
  :custom
  (calendar-latitude +30.67)
  (calendar-longitude +104.07)
  (calendar-date-style 'iso)
  (calendar-week-start-day 1)
  (calendar-mark-holidays-flag t)
  (calendar-mark-diary-entries-flag nil))

(use-package reveal-mode
  :ensure nil
  :hook (after-init . global-reveal-mode)
  :init
  (setq reveal-mode-map nil))

(autoload 'vaper-ex "vaper")

(defun yx/text-mode-setup ()
  (setq-local word-wrap t
              line-spacing 0.15)
  (superword-mode          1)
  (visual-line-mode        1)
  (variable-pitch-mode     1)
  (toggle-truncate-lines  -1))

(add-hook 'text-mode #'yx/text-mode-setup)

(add-to-list 'auto-mode-alist
             '("\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'" . text-mode))

(defun yx/global-mirror-mode-toggle ()
  (with-silent
   (repeat-mode +1))
  (context-menu-mode +1)
  (blink-cursor-mode -1)
  (auto-compression-mode +1)
  (delete-selection-mode +1)
  (auto-save-visited-mode +1)
  (unless (display-graphic-p)
    (xterm-mouse-mode +1))
  (pixel-scroll-precision-mode +1))

(run-with-idle-timer 3 nil #'yx/global-mirror-mode-toggle)

(provide 'y-default)
;;; y-default.el ends here
