;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:13:09
;; Modified: <2024-03-13 15:53:25 yx>
;; Licence: GPLv3

;;; Init
(defvar yx/etc-dir "~/.emacs.d/etc/")
(defvar yx/var-dir "~/.emacs.d/var/")
(defvar yx/lib-dir "~/.emacs.d/lisp/")
(defvar yx/org-dir "~/yxdocs/org-notes/")
(defvar yx/zotero-root "~/Zotero/")

(require 'package)
(require 'use-package-ensure)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archives
      '(("gnu"          . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu"       . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"        . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")))

(setq package-quickstart nil
      package-native-compile t
      package-install-upgrade-built-in t
      package-user-dir (expand-file-name "elpa" yx/var-dir)
      package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))

(setq use-package-verbose t
      use-package-always-defer t
      use-package-always-ensure t
      use-package-expand-minimally t
      use-package-vc-prefer-newest t
      use-package-enable-imenu-support t)

(when (daemonp)
  (setq use-package-always-demand t))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(add-to-list 'load-path yx/lib-dir)
(require 'yx-core)

(defun yx/collect-lib-autoloads ()
  (unless (string= (file-name-nondirectory (yx/latest-file yx/lib-dir))
                   "lisp-autoloads.el")
    (require 'loaddefs-gen nil t)
    (loaddefs-generate yx/lib-dir
                       (expand-file-name "lisp-autoloads.el" yx/lib-dir)
                       nil nil nil t))
  (load (expand-file-name "lisp-autoloads.el" yx/lib-dir) nil t))

(yx/collect-lib-autoloads)

;; %% benchmark
(use-package benchmark-init
  :init
  (benchmark-init/activate)
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; %% no-littering
(use-package no-littering
  :demand t
  :init
  (setq no-littering-var-directory yx/var-dir
        no-littering-etc-directory yx/etc-dir)
  :config
  (defalias 'nol-expand-etc #'no-littering-expand-etc-file-name)
  (defalias 'nol-expand-var #'no-littering-expand-var-file-name)
  (no-littering-theme-backups))

;;; Defaults
(prefer-coding-system 'utf-8)
(set-language-environment 'UTF-8)
(set-selection-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

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
  (set-selection-coding-system 'utf-16-le)
  (prependq! exec-path '("C:/Program Files/Git/bin"
                         "C:/Program Files/Git/usr/bin"))
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
  (setq-default tab-always-indent 'complete))

(use-package simple
  :ensure nil
  :custom
  (save-interprogram-paste-before-kill t)
  (completion-show-help nil)
  (idle-update-delay 1.0)
  (shell-command-prompt-show-cwd t)
  (async-shell-command-display-buffer nil)
  (kill-whole-line t)
  (track-eol t)
  (line-move-visual nil)
  (indent-tabs-mode nil)
  (blink-matching-paren nil)
  (truncate-partial-width-windows 78)
  (set-mark-command-repeat-pop t)
  (remote-file-name-inhibit-auto-save t)
  (read-extended-command-predicate 'command-completion-default-include-p)
  (backward-delete-char-untabify-method 'hungry))

(use-package misc
  :ensure nil
  :custom
  (duplicate-line-final-position -1)
  (duplicate-region-final-position -1))

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
  :hook ((prog-mode  . hl-line-mode)
         (occur-mode . hl-line-mode)
         (dired-mode . hl-line-mode)
         (ibuffer-mode . hl-line-mode)
         (org-agenda-mode . hl-line-mode))
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
  (yx/auto-header-minor-mode +1))

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
  :custom
  (time-stamp-pattern "10/^[@#;\*].*[Mm]odified: <%%>$"))

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
  (add-hook 'help-fns-describe-function-functions #'shortdoc-help-fns-examples-function))

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
  (auth-source-cache-expiry 300)
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

(use-package windmove
  :ensure nil
  :defer 2
  :custom
  (windmove-wrap-around t)
  (windmove-create-window t)
  :config
  (windmove-delete-default-keybindings) ; C-x S-left
  (windmove-default-keybindings 'control)
  ;; (windmove-display-default-keybindings '(shift control)) # dont work
  (windmove-swap-states-default-keybindings '(shift control)))

(use-package browse-url
  :ensure nil
  :custom
  (browse-url-browser-function #'eww-browse-url)
  (browse-url-secondary-browser-function 'browse-url-default-browser)
  :config
  (setq browse-url-generic-program
        (cond (IS-MAC "open")
              (IS-LINUX "xdg-open")
              (t "powershell.exe")))
  (setq browse-url-generic-args
        (cond (IS-WIN '("start"))
              (IS-WSL '("start"))
              (t ""))))

(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

(use-package eww
  :ensure nil
  :init
  (setq shr-max-image-proportion 0.6
        shr-use-xwidgets-for-media t)
  (setq eww-restore-desktop t
        eww-header-line-format nil
        eww-browse-url-new-window-is-tab nil
        eww-search-prefix "http://www.google.com/search?q="
        eww-auto-rename-buffer
        (lambda () (format "*eww: %s*" (or (plist-get eww-data :title) "..."))))
  :config
  (defun yx/eww-quit-all()
    "Kill all EWW buffers, then quit window."
    (interactive)
    (let ((list-buffers (buffer-list)))
      (dolist (buffer list-buffers)
        (when (with-current-buffer buffer
                (eq major-mode 'eww-mode))
          (kill-buffer buffer))))
    (quit-window))
  (add-hook 'eww-mode-hook #'tab-line-mode)
  (add-hook 'eww-after-render-hook #'eww-readable)
  (bind-keys :map eww-mode-map
             ("Q" . yx/eww-quit-all))
  )

(use-package webjump
  :ensure nil
  :custom
  (webjump-sites '(("Org"        . "https://orgmode.org")
                   ("Dicalab"    . "https://center.dicalab.cn")
                   ("Google"     . [simple-query "www.google.com" "www.google.com/search?q=" ""])
                   ("DuckDuckGo" . [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
                   ("Wikipedia"  . [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""]))))

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

(use-package dictionary
  :ensure nil
  :custom
  (dictionary-server "dict.org")
  (dictionary-default-popup-strategy "lev")
  (dictionary-create-buttons nil)
  (dictionary-display-definition-function #'dictionary-display-definition-in-help-buffer))

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

(use-package server
  :ensure nil
  :defer 1
  :custom (server-client-instructions nil)
  :config
  (unless (server-running-p)
    (server-mode)))

(use-package midnight-mode
  :ensure nil
  :defer 10
  :config
  (setq midnight-period 7200)
  (midnight-mode +1))


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

(use-package tramp
  :ensure nil
  :custom
  (tramp-verbose 1)
  (tramp-chunksize 2000)
  (tramp-default-method "ssh")
  (tramp-default-remote-shell "/bin/bash")
  :config
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil)))

(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git))
  (vc-git-diff-switches '("--histogram"))
  (vc-ignore-dir-regexp (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp)))

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

(use-package outline
  :ensure nil
  :custom
  (outline-minor-mode-cycle t)
  (outline-minor-mode-highlight t))

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

(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

;;; Keymaps
(keymap-global-unset "M-m")   ; as major mode transient
(keymap-global-unset "M-c")   ; as programing leader-key
(keymap-global-unset "M-l")   ; as major mode leader-key

;; unset boring keybindings
(keymap-global-unset "C-<wheel-up>")
(keymap-global-unset "C-<wheel-down>")

(defvar-keymap yx/file-prefix-map
  :doc "Prefix map for file."
  "f"   #'find-file
  "M-f" #'ffap
  "d"   #'consult-dir
  "o"   #'yx/ff-other-window
  "p"   #'find-file-at-point
  "t"   #'find-file-other-tab
  "r"   #'consult-recent-file
  "R"   #'rename-visited-file
  "D"   #'yx/delete-file-and-buffer)

(keymap-global-set "C-c f" yx/file-prefix-map)

(defvar-keymap yx/buffer-prefix-map
  :doc "Prefix map for buffer"
  "b" #'tabspaces-switch-to-buffer
  "s" #'scratch-buffer
  "n" #'yx/new-empty-buffer
  "k" #'kill-buffer-and-window
  "C" #'desktop-clear)

(keymap-global-set "C-c b" yx/buffer-prefix-map)

(defvar-keymap yx/window-prefix-map
  :doc "Prefix map for window and workspace"
  "u"   #'winner-undo
  "r"   #'winner-redo
  "C-b" #'burly-open-bookmark
  "C-t" #'burly-reset-tab
  "M-w" #'burly-bookmark-windows
  "M-f" #'burly-bookmark-frames)

(keymap-global-set "C-c w" yx/window-prefix-map)

(defvar-keymap yx/note-prefix-map
  :doc "Prefix map for note taking"
  "c"   #'denote
  "t"   #'denote-template
  "n"   #'denote-open-or-create
  "i"   #'denote-link-or-create
  "m"   #'denote-menu-list-notes
  "C-l" #'denote-backlinks
  "C-f" #'denote-find-link
  "C-b" #'denote-find-backlink
  "M-f" #'denote-org-dblock-insert-links
  "M-b" #'denote-org-dblock-insert-backlinks
  "C-c" #'citar-create-note
  "C-d" #'citar-denote-dwim)

(keymap-global-set "C-c n" yx/note-prefix-map)

(defvar-keymap yx/ctrl-c-q-prefix-map
  :doc "Prefix map for `C-c q'"
  "a"   #'org-ql-find-in-agenda
  "d"   #'org-ql-find-in-org-directory
  "s"   #'org-ql-search
  "v"   #'org-ql-view
  "k"   #'which-key-show-full-major-mode
  "C-a" #'ace-link-addr
  "C-l" #'ace-link)

(keymap-global-set "C-c q" yx/ctrl-c-q-prefix-map)

(defvar-keymap yx/ctrl-c-t-prefix-map
  :doc "Prefix map for toggle mirror mode or others"
  "f" #'follow-mode
  "d" #'drag-stuff-mode
  "l" #'flymake-mode
  "s" #'flyspell-mode
  "L" #'clm/toggle-command-log-buffer
  "F" #'toggle-frame-maximized)

(keymap-global-set "C-c t" yx/ctrl-c-t-prefix-map)

;; the most commonly used shortcut
(bind-keys :map global-map
           :prefix-map yx/ctrl-z-prefix-map
           :prefix "C-z"
           ("."   . repeat)
           (":"   . vaper-ex)
           ("f"   . follow-delete-other-windows-and-split)
           ("a"   . org-agenda-list)
           ("C-a" . org-agenda)
           ("c"   . org-capture)
           ("l"   . org-store-link)
           ("d"   . dirvish-side)
           ("C-d" . sr-speedbar-toggle)
           ("z"   . zoom)
           ("g"   . gptel)
           ("C-c" . gptel-send)
           ("/ /" . webjump)
           ("/ o" . browse-url-at-point))

;; 全局菜单
(bind-keys :map global-map
           :prefix-map yx/ctrl-m-prefix-map
           :prefix "<f9>"
           ("a"   . casual-avy-tmenu)
           ("e"   . yx/transient-emms))

(bind-keys ([remap move-beginning-of-line]        . yx/begin-of-line-dwim)   ; C-a
           ([remap goto-line]                     . consult-goto-line)           ;M-g g
           ([remap switch-to-buffer]              . consult-buffer)              ; C-x b
           ([remap delete-window]                 . yx/delete-window-dwim)       ; C-x 0
           ([remap list-buffers]                  . ibuffer)                 ; C-x C-b
           ([dabbrev-expand]                      . hippie-expand)           ; M-/
           ([remap repeat-complex-command]        . consult-complex-command) ; C-x M-:
           ([remap switch-to-buffer-other-window] . consult-buffer-other-window) ; C-x 4 b
           ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame) ; C-x 5 b
           ([remap project-switch-to-buffer]      . consult-project-buffer)     ; C-x p b
           ([remap yank-pop]                      . consult-yank-pop)           ;M-y
           ([remap bookmark-jump]                 . consult-bookmark)           ;C-x r b
           ([remap imenu]                         . consult-imenu)              ;M-g i
           ([remap describe-function]             . helpful-callable)           ; C-h f
           ([remap describe-key]                  . helpful-key)                ; C-h k
           ([remap describe-command]              . helpful-command)            ; C-h x
           ([remap describe-variable]             . helpful-variable)           ; C-h v
           ([remap list-directory]                . dirvish)                    ; C-x C-d
           ([remap dired-at-point]                . consult-dir)                ; C-x d
           ([remap comment-dwim]                  . yx/comment-dwim)            ; M-;
           ([remap keyboard-quit]                 . yx/keyboard-quit-dwim)      ; C-g
           ([remap kill-buffer]                   . yx/kill-buffer-dwim)        ; C-x k
           ([remap save-buffers-kill-emacs]       . delete-frame)               ; s-q
           ([remap open-line]                     . yx/open-line)               ; C-o
           ([remap fill-paragraph]                . yx/fill-unfill)             ; M-q
           ([remap upcase-word]                   . upcase-dwim)                ; M-u
           ([remap downcase-word]                 . downcase-dwim)              ; M-l
           ([remap capitalize-word]               . capitalize-dwim)            ; M-c
           ([remap default-indent-new-line]       . avy-goto-word-1)            ; M-j
           ([remap goto-char]                     . avy-goto-char-timer)        ; M-g c
           ([remap text-scale-adjust]             . global-text-scale-adjust)   ; C-x C-+
           ([remap global-text-scale-adjust]      . text-scale-adjust) ; C-x C-M-+
           ([remap isearch-forward-regexp]        . yx/isearch-other-window-forward) ; C-M-s
           ([remap isearch-backward-regexp]       . yx/isearch-other-window-backward) ; C-M-s
           ([set-selective-display]               . yx/smarter-selective-display)) ; C-x $

(bind-keys ("C-;"       . iedit-mode)
           ("C-."       . embark-act)
           ("C-,"       . embark-dwim)
           ("C-/"       . undo-only)
           ("C-M-/"     . vundo)
           ("M-o"       . ace-window)
           ("M-O"       . yx/other-window-mru)
           ("M-r"       . consult-recent-file)
           ("C-#"       . consult-register-load)
           ("M-#"       . consult-register-store)
           ("C-M-#"     . consult-register)
           ("C-c #"     . consult-register)
           ("M-z"       . avy-zap-up-to-char-dwim)
           ("M-Z"       . avy-zap-to-char-dwim)
           ("M-g ;"     . goto-last-change)
           ("M-g M-;"   . goto-last-change-reverse)
           ("M-g a"     . consult-org-agenda)
           ("M-g M"     . consult-man)
           ("M-g I"     . consult-info)
           ("M-g M-i"   . consult-imenu-multi)
           ("M-g M-e"   . consult-compile-error)
           ("M-g o"     . consult-outline)
           ("M-g k"     . consult-kmacro)
           ("M-g m"     . consult-mark)
           ("M-g M-m"   . consult-global-mark)
           ("M-g l"     . avy-goto-line)
           ("M-s t"     . yx/hl-todo-rg-project)
           ("M-s M-t"   . hl-todo-occur)
           ("M-s f"     . consult-fd)
           ("M-s M-f"   . dirvish-fd)
           ("M-s l"     . consult-line)
           ("M-s M l"   . consult-line-multi)
           ("M-s k"     . consult-focus-lines)
           ("M-s M-k"   . consult-keep-lines)
           ("M-s g"     . consult-grep)
           ("M-s M-g"   . consult-git-grep)
           ("M-s r"     . consult-ripgrep)
           ("M-s s"     . color-rg-search-input)
           ("M-s M-s"   . color-rg-search-symbol)
           ("M-s p"     . color-rg-search-input-in-project)
           ("M-s M-p"   . color-rg-search-symbol-in-project)
           ("M-s d"     . bing-dict-brief)
           ("M-s M-d"   . stardict-define-at-point)
           ("C-c v"     . magit-file-dispatch)
           ("C-c C-v"   . magit-dispatch)
           ("C-c C-d"   . helpful-at-point)
           ("C-c d"     . duplicate-dwim)
           ("C-c r"     . query-replace-regexp)
           ("C-c z"     . hs-toggle-hiding)
           ("C-c C-z"   . hs-show-all)
           ("C-x a a"   . align)
           ("C-x a r"   . align-regexp)
           ("C-x w t"   . tab-window-detach)
           ("C-x w f"   . tear-off-window)
           ("C-x t R"   . burly-reset-tab)
           ("C-h b"     . embark-bindings)
           ("C-h C-m"   . which-key-show-full-major-mode)
           ("C-h B"     . embark-bindings-at-point)
           :map prog-mode-map
           ("<f5> "   . quickrun)
           ("M-<f5>"  . dape)
           ("M-c f"   . consult-flymake)
           ("M-c d"   . devdocs-lookup)
           ("M-c h"   . symbol-overlay-put)
           ("M-c ."   . citre-jump)
           ("M-c ,"   . citre-jump-back)
           ("M-c p"   . citre-peek)
           ("M-c M-s" . symbols-outline-show)
           ("M-c M-e" . consult-compile-error))

;;; Ui
;; %% font
(defvar yx/font-height 150)
(defvar yx/font "JetBrains Mono")
(defvar yx/fixed-font "IBM Plex Mono")
(defvar yx/serif-font "IBM Plex Serif")
(defvar yx/variable-font "IBM Plex Sans")

(defun yx/font-and-theme-setup ()
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'default nil :family yx/font :height yx/font-height)
        (set-face-attribute 'fixed-pitch nil :family yx/fixed-font)
        (set-face-attribute 'fixed-pitch-serif nil :family yx/serif-font)
        (set-face-attribute 'variable-pitch nil :family yx/variable-font)
        (setq face-font-rescale-alist '(("LXGW WenKai Mono"  . 1.0)
                                        ("Apple Color Emoji" . 0.8)))
        (cl-loop for font in '("LXGW WenKai Mono" "Microsoft Yahei" "PingFang SC")
                 when (x-list-fonts font)
                 return (set-fontset-font t '(#x4e00 . #x9fff) font))
        (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
                 when (x-list-fonts font)
                 return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
        (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
                 when (x-list-fonts font)
                 return (set-fontset-font t 'emoji  (font-spec :family font) nil 'prepend))
        ;; (load-theme 'modus-operandi t)
        (load-theme 'ef-melissa-light t))
    (load-theme 'modus-vivendi t)))

(add-hook 'after-init-hook #'yx/font-and-theme-setup -100)
(add-hook 'server-after-make-frame-hook #'yx/font-and-theme-setup -100)

(use-package prot-modeline
  :ensure nil
  :demand t
  :config
  (setq mode-line-right-align-edge 'right-margin)
  (setq-default mode-line-format
                '("%e"
                  prot-modeline-kbd-macro
                  prot-modeline-narrow
                  prot-modeline-buffer-status
                  prot-modeline-window-dedicated-status
                  prot-modeline-input-method
                  "  "
                  prot-modeline-buffer-identification
                  "  "
                  prot-modeline-major-mode
                  prot-modeline-process
                  "  "
                  prot-modeline-vc-branch
                  "  "
                  prot-modeline-eglot
                  "  "
                  prot-modeline-flymake
                  "  "
                  mode-line-format-right-align
                  "  "
                  prot-modeline-misc-info
                  "  "
                  mode-line-end-spaces)))

(use-package mlscroll
  :hook ((after-init . mlscroll-mode)
         (server-after-make-frame . mlscroll-mode)))

(use-package modus
  :disabled
  :ensure nil
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-variable-pitch-ui t))

(use-package ef-themes
  :init
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t))

(use-package lin
  :defer 5
  :custom
  (lin-face 'lin-magenta)
  :config
  (lin-global-mode +1))

(use-package minions
  :disabled
  :hook (after-init . minions-mode))

(use-package spacious-padding
  :hook (after-init . spacious-padding-mode)
  :custom
  (spacious-padding-subtle-mode-line nil)
  (spacious-padding-widths '( :internal-border-width 4
                              :header-line-width 1
                              :mode-line-width 2
                              :tab-width 4
                              :right-divider-width 12
                              :fringe-width 8)))

(use-package breadcrumb
  :demand t
  :hook (after-init . breadcrumb-mode))

;;; Layout
(use-package window
  :ensure nil
  :custom
  (even-window-sizes t)
  (window-sides-vertical nil)
  (split-width-threshold 80)
  (split-height-threshold nil)
  (switch-to-buffer-obey-display-actions t)
  (switch-to-buffer-in-dedicated-window nil)
  (switch-to-buffer-preserve-window-point t)
  (switch-to-prev-buffer-skip 'visible)
  (switch-to-prev-buffer-skip-regexp "^\\*\\|^magit.*")
  (display-comint-buffer-action '(display-buffer-at-bottom
                                  (inhibit-same-window . nil)))
  :config
  (setq display-buffer-alist
        `((,(rx (| "*Org Select"
                   "*Org Note"
                   "*Agenda Commands"
                   "*tldr*"
                   "*quickrun*"
                   "*diff-hl"
                   "*Dictionary*"
                   "*wclock*"))
           (display-buffer-in-side-window)
           (window-height . 0.45))
          ("\\(\\*Capture\\*\\|CAPTURE-.*\\)"
           (display-buffer-reuse-mode-window display-buffer-below-selected))
          (,(rx (| "*Messages*"
                   "Output*$"
                   "*Backtrace*"
                   "*Async Shell Command*"))
           (display-buffer-reuse-window display-buffer-at-bottom)
           (dedicated . t)
           (window . root) (window-height . 0.45))
          ("\\`\\(\\*Calendar\\|\\*Bookmark\\|\\*stardict\\*\\)"
           (display-buffer-below-selected)
           (dedicated . t)
           (window-height . fit-window-to-buffer))
          ((or (major-mode . help-mode)
               (major-mode . helpful-mode)
               (major-mode . apropos-mode))
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (mode . (help-mode helpful-mode apropos-mode))
           (window-height . 0.45))
          ((or (derived-mode . occur-mode)
               (derived-mode . grep-mode)
               (derived-mode . color-rg-mode)
               (derived-mode . log-view-mode)
               (derived-mode . Buffer-menu-mode))
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (mode . (occur-mode grep-mode color-rg-mode))
           (window-height . 0.45))
          ((or (major-mode . shell-mode)
               (major-mode . eshell-mode)
               (major-mode . term-mode))
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (dedicated . t)
           (window . root) (window-height . 0.45))
          (,(rx (| "*vc-git"
                   "*Warnings*"
                   "*Compile-Log*"))
           (display-buffer-no-window)
           (allow-no-window . t))
          (,(rx (| "*Ibuffer*"
                   "*Org Agenda*"
                   "*Proced*"
                   "*info*"))
           (display-buffer-full-frame)))))

(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :custom
  (winner-dont-bind-my-keys t)
  (winner-boring-buffers-regexp "^\\*")
  :config
  (setq buffer-quit-function 'winner-undo))

(use-package popper
  :hook (after-init . popper-mode)
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-echo-lines 1)
  (popper-display-control nil)
  (popper-window-height 0.4)
  (popper-group-function #'popper-group-by-project)
  (popper-reference-buffers '("\\*quickrun\\*$"
                              "\\*Embark Collect.*\\*"
                              "\\*Embark Export.*\\*"
                              "\\*Flymake diagnostics.*\\*"
                              "^\\*shell.*\\*$"  shell-mode
                              "^\\*term.*\\*$"   term-mode
                              "^\\*vterm.*\\*$"  vterm-mode
                              grep-mode
                              occur-mode
                              color-rg-mode
                              bookmark-bmenu-mode
                              comint-mode
                              compilation-mode
                              devdocs-mode))
  :config
  (popper-echo-mode +1))

(use-package zoom
  :custom
  (zoom-size '(0.618 . 0.618)))

(use-package burly
  :hook (after-init . burly-tabs-mode))

(use-package ibuffer
  :ensure nil
  :hook ((ibuffer-mode . ibuffer-auto-mode)
         (ibuffer-mode . ibuffer-do-sort-by-recency))
  :custom
  (ibuffer-expert t)
  (ibuffer-display-summary nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-default-sorting-mode 'major-mode))

(use-package ibuffer-vc
  :init
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))

(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-mode)
  :custom
  (tab-bar-show 1)
  (tab-bar-auto-width nil)
  (tab-bar-format '(tab-bar-format-menu-bar
                    tab-bar-format-tabs-groups
                    tab-bar-format-align-right
                    tab-bar-format-global))
  (tab-bar-tab-hints t)
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-tab-name-truncated-max 20)
  (tab-bar-select-tab-modifiers '(super))
  :config
  (tab-bar-history-mode +1)
  (keymap-unset tab-bar-map "<wheel-up>")
  (keymap-unset tab-bar-map "<wheel-down>"))

(use-package tab-line
  :ensure nil
  :custom
  (tab-line-switch-cycling t)
  (tab-line-close-button-show 'selected)
  (tab-line-close-tab-function 'kill-buffer)
  (tab-line-tab-name-function #'tab-line-tab-name-truncated-buffer)
  (tab-line-tab-name-truncated-max 25)
  (tab-line-tabs-function #'tab-line-tabs-buffer-groups)
  (tab-line-tabs-buffer-group-function #'yx/tab-line-buffer-group)
  (tab-line-tabs-buffer-list-function #'yx/tab-line-tabs-buffer-list)
  (tab-line-exclude-modes '(completion-list-mode
                            special-mode
                            lisp-interaction-mode
                            messages-buffer-mode)))

(use-package sr-speedbar
  :ensure nil
  :custom
  (speedbar-use-images nil)
  (sr-speedbar-width 28)
  (sr-speedbar-max-width 40)
  (sr-speedbar-skip-other-window-p t)
  (sr-speedbar-use-frame-root-window t)
  :config
  (keymap-set speedbar-mode-map "q" #'sr-speedbar-close))

(use-package ace-window
  :custom
  (aw-scope 'frame)
  (aw-background nil)
  (aw-dispatch-always t)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; %% tabspaces
(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :custom
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-use-filtered-buffers-as-default nil)
  (tabspaces-default-tab "Main")
  (tabspaces-session t)
  (tabspaces-session-auto-restore nil)
  (tabspaces-session-file (nol-expand-var "tabsession.el")))

;;; Completion
(use-package vertico
  :hook (after-init . vertico-mode)
  :bind (:map vertico-map
              ("M-q" . vertico-quick-insert)
              ("M-r" . vertico-repeat-select)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char))
  :custom
  (vertico-resize nil)
  (vertico-preselect 'directory)
  :config
  (vertico-mouse-mode +1)
  (vertico-indexed-mode +1)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (add-hook 'rfn-eshadow-update-overlay #'vertico-directory-tidy))

(use-package orderless
  :demand t
  :custom
  (orderless-component-separator  #'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
  :config
  (orderless-define-completion-style yx/orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  (setq completion-styles '(basic yx/orderless-with-initialism)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))
                                        (eglot (styles basic yx/orderless-with-initialism))
                                        (eglot-capf (styles basic yx/orderless-with-initialism)))))

;; %% embark
(use-package embark
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-e" . embark-export)
         ("C-c C-c" . embark-collect)
         ("C-SPC" . (lambda () (interactive) (embark-select) (vertico-next)))
         :map  embark-general-map
         ("h" . yx/consult-outline-insert-heading))
  :custom
  (embark-help-key "?")
  (embark-cycle-key ".")
  (embark-confirm-act-all nil)
  (embark-selection-indicator nil)
  (prefix-help-command 'embark-prefix-help-command)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :commands embark-open-externally
  :config
  (defun yx/consult-outline-insert-heading (target)
    (let* ((marker (plist-get
                    (text-properties-at 0 target)
                    'consult--candidate))
           (headline-name (org-entry-get nil "ITEM")))
      (org-insert-link nil headline-name)))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after embark
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; %% consult
(use-package consult
  :bind (:map minibuffer-local-map
              ("M-h" . consult-history))
  :custom
  (consult-narrow-key "<")
  (consult-line-start-from-top t)
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq consult-ripgrep-args (concat consult-ripgrep-args " --hidden"))
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-bookmark
   consult-recent-file
   consult--source-buffer
   consult--source-recent-file :preview-key "M-."))

(use-package consult-dir
  :after consult
  :bind (:map vertico-map
              ("C-x C-d" . consult-dir)
              ("C-x C-j" . consult-dir-jump-file)))

(use-package corfu
  :hook ((text-mode prog-mode) . corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-cycle t)
  (corfu-popupinfo-delay nil)
  :config
  (corfu-echo-mode 1)
  (corfu-history-mode 1)
  (corfu-indexed-mode 1)
  (corfu-popupinfo-mode 1)
  (keymap-set corfu-map "M-q" #'corfu-quick-insert))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :after corfu
  :demand t
  :config (corfu-terminal-mode +1))

(use-package cape
  :init
  (setq cape-dabbrev-min-length 3)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  :config
  (cape-wrap-prefix-length #'cape-dict 4)
  (cape-wrap-prefix-length #'cape-line 4))

(use-package marginalia
  :defer 3
  :custom
  (marginalia-align 'left)
  (marginalia-align-offset 10)
  :config
  (marginalia-mode +1))

;;; Misc
(use-package gcmh
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold #x1000000)) ; 16MB

(use-package proxy-mode
  :defer 3
  :custom
  (proxy-mode-env-http-service "127.0.0.1:7890")
  :config
  (let ((proxy-mode-proxy-type 'env-http-proxy))
    (with-silent (proxy-mode +1))))

(use-package engine-mode
  :defer 3
  :custom
  (engine/keybinding-prefix "C-z /")
  (engine/browser-function 'browse-url-generic)
  :config
  (defun yx/extract-name-from-url (url)
    (let* ((host (url-host (url-generic-parse-url url)))
           (host-trimmed (split-string host  "\\.")))
      (car (last host-trimmed 2))))
  (defun yx/define-enines (engines)
    (dolist (engi engines)
      (let* ((key  (car engi))
             (url  (cadr engi))
             (name (yx/extract-name-from-url url))
             (symn (make-symbol name)))
        (eval `(defengine ,symn ,url :keybinding ,key)))))
  (yx/define-enines
   '(("c" "https://github.com/search?q=%s")
     ("g" "https://www.google.com/search?q=%s")
     ("b" "https://cn.bing.com/search?q=%s&ensearch=1")
     ("w" "https://zh.wikipedia.org/w/index.php?search=%s")
     ("a" "https://www.wolframalpha.com/input/?i=%s")
     ("z" "https://www.zhihu.com/search?q=%s")
     ("d" "https://search.douban.com/book/subject_search?search_text=%s")))
  (engine-mode +1))

;; (use-package consult-web
;; :vc (:url "https://github.com/armindarvish/consult-web")
;; :after consult)

(use-package posframe)

(use-package which-key
  :defer 5
  :custom
  (which-key-idle-delay 0.8)
  (which-key-show-early-on-C-h t)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode +1))

(use-package ace-link
  :defer 5
  :config (ace-link-setup-default))

(use-package helpful
  :custom
  (helpful-max-buffers 2)
  :config
  (keymap-set helpful-mode-map "q" #'kill-buffer-and-window))

(use-package command-log-mode
  :custom
  (command-log-mode-key-binding-open-log nil))

;; %% edit enhencement
(use-package vundo)
(use-package goto-chg)

(use-package avy
  :custom
  (avy-style 'at)
  (avy-highlight-first t)
  (avy-timeout-seconds 0.8)
  :config
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?  avy-dispatch-alist) #'avy-action-mark-to-char)
  (setf (alist-get ?. avy-dispatch-alist) #'avy-action-embark)
  (keymap-set isearch-mode-map "M-j" #'avy-isearch))

(use-package avy-zap)

(use-package casual-avy)

(use-package speedrect
  :vc (:url "https://github.com/jdtsmith/speedrect")
  :defer 3)

(use-package hungry-delete
  :hook (after-init . global-hungry-delete-mode)
  :custom
  (hungry-delete-join-reluctantly t))

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(use-package drag-stuff
  :autoload drag-stuff-define-keys
  :hook ((prog-mode conf-mode) . turn-on-drag-stuff-mode)
  :config
  (setq drag-stuff-modifier '(meta shift)
        drag-stuff-except-modes '(org-mode))
  (drag-stuff-define-keys))

(use-package selected
  :ensure t
  :commands selected-minor-mode
  :hook ((prog-mode text-mode) . selected-minor-mode)
  :bind (:map selected-keymap
              ("q" . selected-off)
              (";" . comment-dwim)
              ("\\" . indent-region)
              ("<" . surround-insert)
	      (">" . surround-delete)
              ("=" . puni-expand-region)
              ("-" . puni-contract-region)
              ("!" . shell-command-on-region)
              ("@" . apply-macro-to-region-lines)
              ("*" . pyim-create-word-from-selection)
              ("U" . upcase-region)
              ("D" . downcase-region)
              ("S" . sort-lines)
              ("N" . narrow-to-region)
              :map selected-org-mode-map
              ("t" . org-table-convert-region))
  :init
  (setq selected-org-mode-map (make-sparse-keymap)))

(use-package pulsar
  :defer 3
  :custom
  (pulsar-delay 0.05)
  (pulsar-iterations 8)
  :config
  (add-hook 'next-error-hook         #'pulsar-pulse-line-red)
  (add-hook 'imenu-after-jump-hook   #'pulsar-recenter-center)
  (add-hook 'imenu-after-jump-hook   #'pulsar-reveal-entry)
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-center)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)
  (pulsar-global-mode 1))

(use-package goggles
  :hook ((text-mode prog-mode) . goggles-mode)
  :custom (goggles-pulse t))

;; %% erc
(use-package erc
  :ensure nil
  :custom
  (erc-nick "yxzz")
  (erc-join-buffer 'bury)
  (erc-interpret-mirc-color t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 15)
  (erc-prompt-for-password nil)
  (erc-prompt-for-nickserv-password nil)
  (erc-use-auth-source-for-nickserv-password t)
  (erc-sasl-user "xplutoy")
  (erc-sasl-auth-source-function #'erc-auth-source-search)
  (erc-hide-list '("JOIN" "NICK" "PART" "QUIT"))
  (erc-autojoin-channels-alist '(("#emacs" "#org-mode")))
  :config
  (appendq! erc-modules '(sasl services))
  (erc-update-modules)
  (erc-services-mode 1)
  (defalias 'erc 'erc-tls))

;; %% emms
(use-package emms
  :defer 5
  :hook ((emms-browser-mode . hl-line-mode)
         (emms-browser-mode . turn-on-follow-mode))
  :custom
  (emms-lyrics-dir "~/Music/lyrics/")
  (emms-source-file-default-directory "~/Music/")
  (emms-source-playlist-default-format 'm3u)
  (emms-player-list '(emms-player-mpv))
  (emms-player-mpv-update-metadata t)
  (emms-info-asynchronously t)
  (emms-info-functions '(emms-info-native))
  (emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (emms-browser-thumbnail-small-size 64)
  (emms-browser-thumbnail-medium-size 128)
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-history-load)
  (emms-playing-time-display-mode -1)

  (transient-define-prefix yx/transient-emms ()
    "EMMS music"
    :transient-non-suffix 'transient--do-quit-one
    ["EMMS"
     ["Controls"
      ("p" "Play/Pause" emms-pause)
      ("s" "Stop" emms-stop)
      ("S" "Seek to time" emms-seek-to)
      ("n" "Next" emms-next)
      ("B" "Back (Previous)" emms-previous)
      ("b" "Back rewind" emms-seek-backward :transient transient--do-stay)
      ("f" "Fast-Forward" emms-seek-forward :transient transient--do-stay)]
     ["Playlist"
      ("N" "Cue Next" emms-cue-previous)
      ("P" "Cue Previous" emms-cue-previous)
      ("r" "Play Random" emms-random)
      ("R" "Toggle shuffle" emms-toggle-random-playlist)]
     ["Global/External"
      ("d" "Emms Dired" emms-play-dired)
      ("m" "Modeline" emms-mode-line-mode)
      ("M" "Current info" emms-show)
      ("e" "Emms" emms)]]))

;; %% spell
(use-package jinx
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (setq jinx-languages "en_US"))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("M-$" . flyspell-correct-wrapper)))

(use-package pyim
  :defer 5
  :commands (pyim-create-word-from-selection)
  :custom
  (pyim-outcome-trigger  nil)
  (pyim-enable-shortcode nil)
  (pyim-page-tooltip 'posframe)
  (pyim-candidates-search-buffer-p nil)
  (pyim-dcache-backend 'pyim-dregcach)
  (pyim-indicator-list '(pyim-indicator-with-modeline))
  (pyim-english-input-switch-functions '(pyim-probe-auto-english
                                         pyim-probe-program-mode
                                         pyim-probe-isearch-mode
                                         pyim-probe-org-latex-mode
                                         pyim-probe-org-structure-template))
  :config
  (require 'pyim-dregcache)
  (require 'pyim-cstring-utils)
  (setq default-input-method "pyim")
  (with-silent
   (pyim-default-scheme 'xiaohe-shuangpin))
  (keymap-set org-mode-map "M-f" #'pyim-forward-word)
  (keymap-set org-mode-map "M-b" #'pyim-backward-word)
  (let ((yx-dict (nol-expand-etc "yx-dict.pyim")))
    (add-hook #'kill-emacs-hook
              (lambda ()
                (pyim-export-words-and-counts yx-dict)))
    (setq pyim-dicts `(,(list :name "yx-dict" :file yx-dict)))))

(use-package pyim-tsinghua-dict
  :vc (:url "https://github.com/redguardtoo/pyim-tsinghua-dict")
  :after pyim
  :demand t
  :config (pyim-tsinghua-dict-enable))

(use-package stardict
  :ensure nil
  :init
  (setq stardict-name "langdao-ec-gb"
        stardict-dir (nol-expand-etc "stardict-langdao-ec-gb-2.4.2")))

(use-package bing-dict
  :init
  (setq bing-dict-vocabulary-save t
        bing-dict-cache-auto-save nil
        bing-dict-show-thesaurus 'synonym
        bing-dict-vocabulary-file (nol-expand-var "bing-dict-vocabulary.org")))

(use-package cal-china-x
  :defer 5
  :config
  (setq mark-holidays-in-calendar t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")
                                       (holiday-lunar 7 7  "七夕节")
                                       (holiday-fixed 3 8  "妇女节")
                                       (holiday-fixed 3 12 "植树节")
                                       (holiday-fixed 5 4  "青年节")
                                       (holiday-fixed 6 1  "儿童节")
                                       (holiday-fixed 9 10 "教师节")))
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays)))

;; %% Tools
(use-package tldr)

(use-package dwim-shell-command
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command)))

(use-package outli
  :vc (:url "https://github.com/jdtsmith/outli")
  :hook ((prog-mode text-mode) . outli-mode)
  :bind (:map outli-mode-map
              ("C-c C-u" . (lambda () (interactive) (outline-back-to-heading)))))

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-org-branching-context t)
  :config
  (let* ((auth-info (car (auth-source-search :user "moonshot-apikey")))
         (host (plist-get auth-info :host))
         (key (plist-get auth-info :secret)))
    (setq-default gptel-model "moonshot-v1-32k"
                  gptel-backend (gptel-make-openai "Moonshot"
                                  :host host
                                  :key key
                                  :models '("moonshot-v1-32k"
                                            "moonshot-v1-128k"))))
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(use-package casual
  :after calc
  :demand t
  :config
  (keymap-set calc-mode-map "M-m" #'casual-main-menu))

;;; Dired
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-<return>" . dired-do-open)
              ("C-+" . dired-create-empty-file))
  :custom
  (dired-dwim-target t)
  (dired-vc-rename-file t)
  (dired-mouse-drag-files t)
  (dired-movement-style 'cycle)
  (dired-ls-F-marks-symlinks t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask)
  (dired-auto-revert-buffer 'dired-buffer-stale-p)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-laFGgh")
  (wdired-create-parent-directories t)
  (wdired-allow-to-change-permissions t)
  :config
  (defun yx/dired-setup ()
    (setq dired-omit-files
          (concat dired-omit-files "\\|^\\..*$"))
    (setq-local mouse-1-click-follows-link 'double)
    (dired-omit-mode 1)
    (dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook 'yx/dired-setup)
  (add-hook 'wdired-mode-hook 'highlight-changes-mode)
  (put 'dired-find-alternate-file 'disabled nil))

;; %% dired+
(use-package casual-dired
  :after dired
  :bind (:map dired-mode-map ("M-m" . casual-dired-tmenu)))

(use-package diredfl
  :hook ((dired-mode . diredfl-mode)
         (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package dirvish
  :hook (after-init . dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"           "Home")
     ("d" "~/yxdocs/"    "yxdocs")
     ("m" "/mnt/"        "Drives")
     ("w" "~/workspace/" "workspace")
     ("c" "~/Codes")
     ("D" "~/Downloads/" "Downloads")))
  :config
  (setq dirvish-side-width 30
        dirvish-use-mode-line t
        dirvish-default-layout '(0 0.4 0.6))
  (let ((height (/ yx/font-height 10.0)))
    (setq dirvish-mode-line-height height
          dirvish-header-line-height height))
  (setq dirvish-attributes
        '(file-time file-size collapse subtree-state vc-state))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-peek-mode -1)
  (dirvish-side-follow-mode 1)
  :bind (:map dirvish-mode-map
              ("a"   . dirvish-quick-access)
              ("f"   . dirvish-file-info-menu)
              ("y"   . dirvish-yank-menu)
              ("N"   . dirvish-narrow)
              ("h"   . dirvish-history-jump)
              ("s"   . dirvish-quicksort)
              ("v"   . dirvish-vc-menu)
              ("TAB" . dirvish-subtree-toggle)
              ("M-f" . dirvish-history-go-forward)
              ("M-b" . dirvish-history-go-backward)
              ("M-t" . dirvish-layout-toggle)
              ("M-s" . dirvish-setup-menu)
              ("M-e" . dirvish-emerge-menu)
              ("M-j" . dirvish-fd-jump)))

;;; Gnus
(setq read-mail-command 'gnus
      message-confirm-send t
      message-kill-buffer-on-exit t
      mail-user-agent 'gnus-user-agent
      mail-envelope-from 'header
      mail-specify-envelope-from t
      send-mail-function 'message-send-mail-with-sendmail)

(setq mml-default-sign-method "pgpmime"
      mml-secure-openpgp-sign-with-sender t)

(setq gnus-home-directory no-littering-var-directory
      gnus-default-directory gnus-home-directory
      gnus-startup-file (nol-expand-var "newsrc"))

(use-package gnus
  :ensure nil
  :config
  (setq gnus-select-method '(nnnil "")
        gnus-secondary-select-methods
        '((nnimap "foxmail.cs"
                  (nnimap-address "imap.qq.com"))
          (nnimap "outlook.cs"
                  (nnimap-address "outlook.office365.com"))))

  (setq gnus-asynchronous t
        gnus-use-header-prefetch t
        gnus-use-cache t
        gnus-use-scoring t
        gnus-suppress-duplicates t
        gnus-novice-user nil
        gnus-expert-user t
        gnus-interactive-exit 'quiet
        gnus-inhibit-startup-message t)

  (setq gnus-save-newsrc-file nil
        gnus-read-newsrc-file nil
        gnus-save-killed-list nil
        gnus-read-active-file nil
        gnus-always-read-dribble-file t
        gnus-message-archive-group nil
        gnus-article-browse-delete-temp t
        gnus-mime-display-multipart-related-as-mixed t)

  (setq nnmail-expiry-wait '30
        nnmail-resplit-incoming t
        nnmail-split-fancy-match-partial-words t
        nnmail-split-methods 'nnmail-split-fancy
        nnmail-split-fancy '(| (: nnmail-split-fancy-with-parent)
                               (to  "yangxue.cs@foxmail.com" "INBOX.foxmail.cs")
                               (to  "yangxue.cs@outlook.com" "INBOX.outlook.cs")
                               (any "emacs-devel@gnu.org"    "INBOX.emacs-devel")
                               (any "emacs-orgmode@gnu.org"  "INBOX.emacs-orgmode")
                               (any "help-gnu-emacs@gnu.org" "INBOX.emacs-help")
                               "INBOX.Misc"))

  (setq nnrss-ignore-article-fields '(description guid pubData dc:creator link))

  (gnus-demon-add-handler 'gnus-demon-scan-mail nil 10))

(use-package gnus-group
  :ensure nil
  :hook (gnus-select-group . gnus-group-set-timestamp)
  :config
  (setq gnus-sum-thread-tree-root            "◉ "
        gnus-sum-thread-tree-false-root      "◎ "
        gnus-sum-thread-tree-single-indent   "◌ "
        gnus-sum-thread-tree-vertical        "| "
        gnus-sum-thread-tree-indent          "  "
        gnus-sum-thread-tree-leaf-with-other "+-> "
        gnus-sum-thread-tree-single-leaf     "`-> "
        gnus-summary-line-format "%U%R%z%B%[%4L: %-10,10f%] %s\n")

  (setq gnus-summary-make-false-root 'adopt
        gnus-summary-ignore-duplicates t
        gnus-newsgroup-maximum-articles 1500
        gnus-summary-gather-subject-limit 'fuzzy
        gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
        gnus-simplify-subject-functions '(gnus-simplify-subject-re gnus-simplify-whitespace))

  (setq gnus-use-trees t
        gnus-show-threads t
        gnus-fetch-old-headers t
        gnus-build-sparse-threads 'some
        gnus-thread-indent-level 2
        gnus-generate-tree-function #'gnus-generate-horizontal-tree
        gnus-thread-sort-functions '(gnus-thread-sort-by-subject
                                     gnus-thread-sort-by-most-recent-number))

  (setq gnus-group-sort-function '(gnus-group-sort-by-method)
        gnus-group-line-format "%M%S%p%P %0{%5y%} %B%{%G%}\n")

  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode))

;;; Shell

(use-package comint
  :ensure nil
  :bind (:map comint-mode-map
	      ([remap kill-region] . comint-kill-regio)
	      ([remap kill-whole-line] . comint-kill-whole-line))
  :custom
  (comint-input-ignoredups t)
  (comint-prompt-read-only t)
  (comint-input-autoexpand 'input)
  (comint-completion-autolist t)
  (comint-scroll-to-bottom-on-input t)
  (comint-scroll-to-bottom-on-output t))

(use-package shell
  :ensure nil
  :custom
  (shell-kill-buffer-on-exit t)
  (shell-highlight-undef-enable t))

(use-package eshell
  :custom
  (eshell-kill-on-exit t)
  (eshell-history-append t)
  (eshell-save-history-on-exit t)
  (eshell-hist-ignoredups t)
  (eshell-error-if-no-glob t)
  (eshell-prefer-lisp-functions t)
  (eshell-scroll-to-bottom-on-input  'all)
  (eshell-scroll-to-bottom-on-output 'all)
  (eshell-prompt-function 'yx/eshell-prompt)
  :config
  (setenv "PAGER" "cat")
  (add-to-list 'eshell-modules-list #'eshell-tramp)
  (add-to-list 'eshell-modules-list #'eshell-rebind)
  (add-to-list 'eshell-modules-list #'eshell-elecslash)

  (with-eval-after-load 'em-alias
    (eshell/alias "q"    "exit")
    (eshell/alias "r"    "consult-recent-file")
    (eshell/alias "d"    "dired $1")
    (eshell/alias "f"    "find-file $1")
    (eshell/alias "gs"   "magit-status")
    (eshell/alias "gd"   "magit-diff-unstaged")
    (eshell/alias "gds"  "magit-diff-staged")
    (eshell/alias "gv"   "magit-dispatch")
    (eshell/alias "la"   "ls -laAFh $*")
    (eshell/alias "ll"   "ls -AlohG --color=always"))

  (with-eval-after-load 'em-term
    (appendq! eshell-visual-subcommands '(("git" "log" "diff" "show"))))

  (defun yx/eshell-setup ()
    (keymap-set eshell-mode-map "C-l" #'yx/eshell-clear)
    (keymap-set eshell-mode-map "M-h" #'consult-history)
    (set-window-fringes nil 0 0)
    (set-window-margins nil 1 nil)
    (visual-line-mode +1)
    (add-to-list 'completion-at-point-functions 'cape-elisp-symbol)
    (add-to-list 'completion-at-point-functions 'cape-file)
    (setq-local corfu-auto nil
                corfu-quit-at-boundary t
                corfu-quit-no-match t)
    (corfu-mode +1))

  (add-hook 'eshell-mode-hook #'yx/eshell-setup)

  (defun yx/eshell-clear ()
    "Clear the current Eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (defun yx/eshell-prompt()
    (setq eshell-prompt-regexp "^[^#$\n]*[#$] ")
    (concat (yx/pwd-replace-home (eshell/pwd))
            (if (fboundp 'magit-get-current-branch)
                (if-let ((branch (magit-get-current-branch)))
                    (format " [git:%s]" branch)
                  "")
              "")
            (if (= (user-uid) 0) "\n# " "\n$ "))))

(use-package pcmpl-args
  :after eshell
  :demand t)

(use-package eshell-syntax-highlighting
  :after eshell-mode
  :demand t
  :config
  (eshell-syntax-highlighting-global-mode +1))

;; %% eat
(use-package eat
  :vc (:url "https://codeberg.org/akib/emacs-eat")
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-enable-yank-to-terminal t)
  :hook
  ((eshell-load . eat-eshell-mode)
   (eshell-load . eat-eshell-visual-command-mode)))

;; %% vterm
(use-package vterm
  :unless IS-WIN
  :custom (vterm-always-compile-module t))

;;; Reading
(use-package doc-view
  :ensure nil
  :custom
  (doc-view-continuous t)
  (doc-view-resolution 300))

(use-package pdf-tools
  :hook (pdf-tools-enabled . pdf-isearch-minor-mode)
  :init
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (setq-default pdf-view-display-size 'fit-width)
  (pdf-loader-install))

(use-package olivetti
  :hook ((org-mode . olivetti-mode)
         (org-agenda-mode . olivetti-mode))
  :init
  (setq olivetti-style nil
        olivetti-mode-map nil)
  :config
  (keymap-local-unset "<left-margin> <mouse-1>")
  (keymap-local-unset "<right-margin> <mouse-1>"))

(use-package elfeed
  :init
  (setq elfeed-feeds
        '(("https://spaces.ac.cn/feed" ai)
          ("https://www.inference.vc/rss" ai)
          ("https://liduos.com/atom.xml" ai)
          ("https://lilianweng.github.io/index.xml" ai)
          ("https://tech.youzan.com/rss/" tech)
          ("https://tech.meituan.com/feed/" tech)
          ("https://ameow.xyz/feed/categories/weekly.xml" tech)
          ("https://www.ruanyifeng.com/blog/atom.xml" tech)
          ("https://wingolog.org/feed/atom" lang)
          ("https://www.juliabloggers.com/feed/" julia)
          ;; ("https://planet.scheme.org/atom.xml" scheme)
          ;; ("https://planet.haskell.org/rss20.xml" haskell)
          ("https://karthinks.com/index.xml" emacs)
          ("https://planet.emacslife.com/atom.xml" emacs)
          ("https://manateelazycat.github.io/feed.xml" emacs)))
  (setq elfeed-search-filter "@3-months-ago +unread")
  (setq elfeed-search-print-entry-function #'yx/elfeed-search-print-entry)
  :hook (elfeed-show . olivetti-mode)
  :config
  (require 'elfeed-ext)
  (run-at-time nil (* 4 60 60) 'elfeed-update)
  (keymap-set elfeed-show-mode-map "w" #'elfeed-show-yank)
  (keymap-set elfeed-show-mode-map "%" #'yx/elfeed-show-in-xwidget)
  (keymap-set elfeed-show-mode-map "q" #'yx/elfeed-kill-entry)
  (keymap-set elfeed-search-mode-map "m" (yx/elfeed-tag-as 'star))
  (keymap-set elfeed-search-mode-map "l" (yx/elfeed-tag-as 'readlater)))

;;; Writing
(use-package org
  :ensure nil
  :defer 5
  :bind (:map org-mode-map
              ("M-g h"   . consult-org-heading)
              ("M-l i"   . org-clock-in)
              ("M-l o"   . org-clock-out)
              ("M-l h"   . org-toggle-heading)
              ("M-l r"   . denote-refs-mode)
              ("M-l y"   . yx/org-link-copy)
              ("M-l p"   . org-download-clipboard)
              ("M-l C-p" . org-download-screenshot)
              ("M-l C-i" . org-web-tools-insert-link-for-url)
              ("M-l q f" . org-ql-find)
              ("M-l q r" . org-ql-refile)
              ("M-l t"   . org-transclusion-add)
              ("M-l C-t" . org-transclusion-add-all)
              ("M-l M-t" . org-transclusion-remove)
              ("M-l TAB" . yx/org-show-current-heading-tidily)
              ("M-l l"   . yx/insert-fixed-org-link)
              ("M-l C-l" . org-latex-preview)
              ("M-l C-v" . yx/org-toggle-inline-images-in-subtree)
              ("M-l /"   . transform-previous-char)
              :repeat-map org-heading-navigate-repeat-map
              ("u" . outline-up-heading)
              ("p" . org-previous-visible-heading)
              ("n" . org-next-visible-heading)
              ("f" . org-forward-heading-same-level)
              ("b" . org-backward-heading-same-level))
  :autoload (org-calendar-holiday)
  :hook
  (org-trigger . save-buffer)
  (org-agenda-finalize . yx/org-agenda-to-appt)
  (org-babel-after-execute . yx/org-babel-display-image)
  :custom
  (org-directory yx/org-dir)
  (org-ellipsis nil)
  (org-num-max-level 2)
  (org-reverse-note-order t)
  (org-list-allow-alphabetical t)
  (org-return-follows-link t)
  (org-mouse-1-follows-link nil)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-cycle-separator-lines 0)
  (org-use-sub-superscripts '{})
  (org-use-speed-commands t)
  (org-special-ctrl-o t)
  (org-special-ctrl-k t)
  (org-special-ctrl-a/e t)
  (org-support-shift-select t)
  (org-ctrl-k-protect-subtree nil)
  (org-M-RET-may-split-line nil)
  (org-link-file-path-type 'relative)
  (org-link-use-indirect-buffer-for-internals t)
  (org-ascii-headline-spacing '(0 . 1))
  (org-yank-adjusted-subtrees t)
  (org-insert-heading-respect-content t)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-image-actual-width '(600))
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (org-lowest-priority ?D)
  (org-priority-default ?C)

  (org-element-use-cache t)
  (org-element-cache-persistent nil)

  (org-blank-before-new-entry '((heading . nil)
                                (plain-list-item . nil)))


  (org-startup-folded t)
  (org-startup-indented nil)
  (org-adapt-indentation nil)
  (org-hide-block-startup t)
  (org-hide-drawer-startup t)
  (org-startup-align-all-tables t)
  (org-startup-with-inline-images nil)

  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-redeadline 'time)
  (org-log-reschedule 'time)
  (org-log-note-clock-out nil)
  (org-read-date-prefer-future 'time)

  (org-pretty-entities nil)
  (org-pretty-entities-include-sub-superscripts nil)

  (org-fontify-done-headline t)
  (org-fontify-todo-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-block-delimiter-line t)

  (org-refile-targets '((nil :maxlevel . 3)
                        (org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)

  (org-goto-max-level 3)
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)


  (org-preview-latex-process-alist '((dvisvgm :programs ("xelatex" "dvisvgm")
                                              :description "xdv > svg"
                                              :message "you need to install the programs: xelatex and dvisvgm."
                                              :image-input-type "xdv" :image-output-type "svg" :image-size-adjust (1.7 . 1.5)
                                              :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                                              :image-converter ("dvisvgm %o/%b.xdv --no-fonts --exact-bbox --scale=%S --output=%O"))))

  (org-footnote-auto-adjust t)
  (org-footnote-define-inline nil)

  (org-src-tab-acts-natively t)
  (org-src-fontify-natively t)
  (org-confirm-babel-evaluate nil)
  (org-src-window-setup 'split-window-right)
  (org-src-ask-before-returning-to-edit-buffer nil)

  (org-babel-default-header-args '((:evel    . "never-export")
                                   (:session . "none")
                                   (:results . "replace")
                                   (:exports . "both")
                                   (:cache   . "no")
                                   (:noweb   . "no")
                                   (:hlines  . "no")
                                   (:wrap    . "results")
                                   (:tangle  . "no")))

  (org-modules '(ol-info ol-eww))

  ;; tag
  (org-tags-column 0)
  (org-auto-align-tags t)
  (org-tags-exclude-from-inheritance '(project crypt))
  (org-tag-persistent-alist '((:startgroup . nil)
                              ("@me" . ?m) ("@work" . ?t) ("@life" . ?l)
                              (:endgroup . nil)
                              ("crypt" . ?c) ("project" . ?p)))
  (org-fast-tag-selection-single-key t)

  ;; todo
  (org-todo-keywords
   '((sequence "TODO(t!)" "SOMEDAY(s!)" "NEXT(n!)" "HOLD(h@/!)" "WAITING(w@/!)" "|" "CANCELED(c@/!)" "DONE(d!)")))
  (org-todo-repeat-to-state "NEXT")
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-treat-S-cursor-todo-selection-as-state-change nil)

  (org-default-notes-file (expand-file-name "inbox.org" org-directory))

  (org-capture-bookmark nil)
  (org-capture-templates
   '(("t" "个人事务" entry (file+headline org-default-notes-file "个人事务") "* TODO [#B] %?" :prepend t)
     ("w" "工作任务" entry (file+headline org-default-notes-file "工作任务") "* TODO [#B] %?" :prepend t)
     ("s" "未来想做" entry (file+headline org-default-notes-file "未来想做") "* SOMEDAY %?"   :prepend t)
     ("h" "习惯养成" entry (file+headline org-default-notes-file "习惯养成") "* NEXT %?"      :prepend t)))

  (org-stuck-projects '("+project/-DONE-CANCELED"
                        ("NEXT")
                        nil ""))

  (org-scheduled-past-days 365)
  (org-deadline-warning-days 365)

  (org-habit-show-all-today nil)

  (org-agenda-span 'day)
  (org-agenda-files `(,org-default-notes-file))
  (org-agenda-inhibit-startup t)
  (org-agenda-sticky nil)
  (org-agenda-compact-blocks t)
  (org-agenda-remove-tags t)
  (org-agenda-show-all-dates t)
  (org-agenda-mouse-1-follows-link nil)
  (org-agenda-time-leading-zero t)
  (org-agenda-include-deadlines t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-scheduled-delay-if-deadline 't)
  (org-agenda-skip-timestamp-if-deadline-is-shown nil)
  (org-agenda-skip-scheduled-if-deadline-is-shown nil)
  (org-agenda-skip-deadline-prewarning-if-scheduled 't)
  (org-agenda-todo-ignore-deadlines 'near)
  (org-agenda-todo-ignore-scheduled 'future)
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-window-setup 'only-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-use-tag-inheritance nil)
  (org-agenda-use-time-grid t)
  (org-agenda-time-grid (quote ((daily today require-timed)
                                (300 600 900 1200 1500 1800 2100 2400)
                                "......"
                                "---------------------------------")))
  (org-agenda-current-time-string "Now - - - - - - - - - - - - - - - - - - - - -")
  (org-agenda-diary-file
   (expand-file-name "diary.org" yx/org-dir))
  (org-agenda-include-diary t)
  (org-agenda-show-future-repeats 'next)
  (org-agenda-format-date 'yx/org-agenda-format-date-aligned)
  (org-agenda-scheduled-leaders '("计划@  " "拖%03d  "))
  (org-agenda-deadline-leaders  '("截止@  " "剩%03d  " "逾%03d  "))

  (org-clock-idle-time 60)
  (org-clock-into-drawer t)
  (org-clock-persist t)
  (org-clock-in-resume t)
  (org-clock-out-when-done t)
  (org-clock-in-switch-to-state "NEXT")
  (org-clock-persist-query-resume nil)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-report-include-clocking-task t)
  (org-use-last-clock-out-time-as-effective-time t)

  (org-global-properties '(("STYLE_ALL"  . "habit")
                           ("Score_ALL"  . "1 2 3 5 8")
                           ("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00")))
  (org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")

  (org-cite-csl-styles-dir (expand-file-name "styles/" yx/zotero-root))
  (org-cite-global-bibliography (list (expand-file-name "bibliography.bib" yx/org-dir)))
  (org-cite-export-processors '((latex biblatex)
                                (t . (csl "ieee.csl"))))

  (org-attach-id-dir (expand-file-name "data/" yx/org-dir))
  (org-attach-dir-relative t)
  (org-attach-store-link-p 'attach)
  (org-attach-sync-delete-empty-dir t)

  (org-export-with-broken-links t)
  (org-export-with-section-numbers nil)
  (org-export-with-sub-superscripts '{})

  :custom-face
  (org-level-1 ((t (:height 1.3))))
  (org-level-2 ((t (:height 1.2))))
  (org-level-3 ((t (:height 1.1))))
  (org-document-title ((t (:height 1.5))))
  (org-drawer ((t (:height 0.85))))
  (org-special-keyword ((t (:height 0.85))))

  :config
  (require 'org-habit)
  (require 'org-tempo)

  (defun yx/org-mode-setup ()
    (auto-fill-mode -1)
    (variable-pitch-mode 1)
    (push 'cape-tex completion-at-point-functions)
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table))
  (add-hook 'org-mode-hook #'yx/org-mode-setup)

  (cond
   (IS-MAC (plist-put org-format-latex-options :scale 1.2))
   (IS-LINUX (plist-put org-format-latex-options :scale 0.8))
   (t (plist-put org-format-latex-options :scale 1.0)))
  (plist-put org-format-latex-options :background "Transparent")

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (python . t)
                                 (C . t)
                                 (R . t)
                                 (julia . t)
                                 (org . t)
                                 (shell . t)
                                 (latex . t)
                                 (dot . t)
                                 (gnuplot . t)
                                 (lisp . t)
                                 (scheme . t)
                                 (jupyter . t)))
  (org-crypt-use-before-save-magic)
  (org-clock-persistence-insinuate)
  (org-clock-auto-clockout-insinuate)
  (run-at-time t 7200 'org-agenda-to-appt)

  (font-lock-add-keywords
   'org-mode
   '(("\\(\\(?:\\\\\\(?:label\\|ref\\|eqref\\)\\)\\){\\(.+?\\)}"
      (1 font-lock-keyword-face)
      (2 font-lock-constant-face))))

  (add-hook 'org-ctrl-c-ctrl-c-hook 'yx/org-check-latex-fragment)
  (add-hook 'org-cycle-hook (lambda (state)
                              (yx/org-toggle-inline-images-in-subtree))))

(use-package ox-latex
  :ensure nil
  :custom
  (org-latex-compiler "xelatex")
  (org-latex-prefer-user-labels t)
  (org-latex-default-class "ctexart")
  (org-latex-packages-alist '(("" "bm")
                              ("" "amsthm")
                              ("" "amsfonts")
                              ("" "xcolor" t)
                              ("cache=false" "minted" t)))
  (org-latex-src-block-backend 'minted)
  (org-latex-minted-options '(("breaklines")
                              ("bgcolor" "bg")))
  (org-latex-pdf-process '("latexmk -f -xelatex -shell-escape -output-directory=%o %f" ))

  (org-startup-with-latex-preview nil)
  (org-highlight-latex-and-related '(native entities))
  (org-preview-latex-image-directory (nol-expand-var "ltximg/"))
  (org-preview-latex-default-process 'dvisvgm)
  (org-preview-latex-process-alist'((dvisvgm :programs
                                             ("xelatex" "dvisvgm")
                                             :description "xdv > svg"
                                             :message "you need to install the programs: xelatex and dvisvgm."
                                             :use-xcolor t
                                             :image-input-type "xdv"
                                             :image-output-type "svg"
                                             :image-size-adjust (1.7 . 1.5)
                                             :latex-compiler
                                             ("xelatex -no-pdf -interaction nonstopmode -shell-escape -output-directory %o %f")
                                             :image-converter
                                             ("dvisvgm %f -e -n -b min -c %S -o %O"))))
  :config
  (add-to-list 'org-latex-classes
               '("ctexart"
                 "\\documentclass[11pt]{ctexart}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (appendq! org-latex-logfiles-extensions '("toc" "dvi" "tex" "bbl" "aux" "fls" "fdb_latexmk" "log")))

;; %% org+
(use-package org-ql
  :after org
  :bind (:map org-mode-map
              ("C-c q f" . org-ql-find)
              ("C-c q r" . org-ql-refile)))

(use-package org-super-agenda
  :hook (org-agenda-mode . org-super-agenda-mode)
  :init
  (setq org-super-agenda-groups
        '((:name "今天要做"
                 :time-grid t
                 :deadline today
                 :scheduled today)
          (:name "重要紧急"
                 :tag "urgent"
                 :priority>= "A")
          (:name "过期事务"
                 :deadline past)
          (:name "阻塞事务"
                 :todo "HOLD"
                 :todo "WAITING")
          (:name "其他待办"
                 :anything))))

(use-package org-modern
  :after org
  :demand t
  :custom-face
  (org-modern-block-name ((t (:height 0.85))))
  :config
  (setq org-modern-block-fringe 0)
  (global-org-modern-mode 1))

(use-package valign
  :hook (org-mode . valign-mode)
  :custom
  (valign-fancy-bar 1))

(use-package mixed-pitch
  :hook ((org-mode
          eww-mode
          elfeed-show-mode) . mixed-pitch-mode))

(use-package org-download
  :after org
  :demand t
  :hook (dired-mode . org-download-enable)
  :custom
  (org-download-heading-lvl nil)
  (org-download-image-org-width 900)
  (org-download-image-dir (expand-file-name "images/" org-attach-directory))
  (org-download-screenshot-method (cond
                                   (IS-MAC "screencapture -i %s")
                                   (IS-WIN "powershell.exe -command \"(Get-Clipboard -Format Image).Save('%s')\"")
                                   (IS-WSL "powershell.exe -command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\"")
                                   (t "scrot -s %s")))
  :config
  (setq org-download-annotate-function (lambda (_link) "")))

(use-package org-web-tools)

(use-package denote
  :after org
  :demand t
  :custom
  (denote-directory yx/org-dir)
  (denote-infer-keywords t)
  (denote-known-keywords nil)
  (denote-date-prompt-use-org-read-date t)
  (denote-excluded-directories-regexp "data\\|scaffold")
  (denote-prompts '(subdirectory title keywords))
  (denote-templates nil)
  :config
  (require 'denote-org-extras)
  (denote-rename-buffer-mode 1)
  :preface
  (defun yx/denote-template ()
    "Create note while prompting for a template.
This is equivalent to calling `denote' when `denote-prompts' is set to \\='(template title keywords subdirectory)."
    (declare (interactive-only t))
    (interactive)
    (let ((denote-prompts '(template subdirectory title keywords)))
      (call-interactively #'denote))))

(use-package consult-denote
  :after denote
  :demand t
  :config (consult-denote-mode +1))

(use-package denote-refs)

(use-package denote-menu
  :bind ( :map denote-menu-mode-map
          ("c"   . denote-menu-clear-filters)
          ("e"   . denote-menu-export-to-dired)
          ("/ r" . denote-menu-filter)
          ("/ k" . denote-menu-filter-by-keyword)
          ("/ o" . denote-menu-filter-out-keyword)))

(use-package citar-denote
  :after (citar denote)
  :demand t
  :config
  (setq citar-denote-subdir t)
  (citar-denote-mode))

(use-package edit-indirect)
(use-package org-transclusion)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode))
  :init
  (setq markdown-command "pandoc"
        markdown-enable-math t
        markdown-header-scaling t))

;; %% bibtex
(use-package bibtex
  :ensure nil
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-align-at-equal-sign t))

;; %% latex
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :hook ((LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . TeX-fold-mode)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . (lambda ()
                         (eglot-ensure)
                         (push #'cape-tex completion-at-point-functions))))
  :bind (:map LaTeX-mode-map
              ("s-/" . transform-previous-char))
  :config
  (setq-default Tex-master nil)
  (setq-default TeX-engine 'xetex)
  (setq-default LaTeX-electric-left-right-brace t)
  (setq TeX-auto-save t
        TeX-save-query nil
        TeX-parse-self t
        TeX-source-correlate-start-server t
        TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'TeX-after-comilation-finished-functions #'TeX-revert-document-buffer))

(use-package cdlatex
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (org-mode   . turn-on-org-cdlatex)))

(use-package transform
  :ensure nil)

;; %% citar
(use-package citar
  :after org
  :demand t
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-notes-paths `(,yx/org-dir))
  (citar-library-paths `(,yx/zotero-root))
  (citar-at-point-function 'embark-act)
  (citar-bibliography org-cite-global-bibliography)
  :hook ((org-mode . citar-capf-setup)
         (LaTeX-mode . citar-capf-setup)))

(use-package citar-embark
  :after citar embark
  :demand t
  :no-require t
  :config (citar-embark-mode +1))

;;; Programming
(use-package prog-mode
  :ensure nil
  :custom
  (prettify-symbols-unprettify-at-point 'right-edge)
  :config
  (defun yx/prog-mode-setup ()
    (hs-minor-mode +1)
    (setq line-spacing 0.15
          show-trailing-whitespace t))
  (add-hook 'prog-mode-hook #'yx/prog-mode-setup))

(use-package project
  :ensure nil
  :custom
  (project-mode-line t)
  (project-file-history-behavior 'relativize)
  (project-vc-extra-root-markers '(".envrc" "pyproject.toml")))

(use-package diff-mode
  :ensure nil
  :hook (diff-mode . outline-minor-mode)
  :custom
  (diff-default-read-only t)
  (diff-update-on-the-fly t)
  (diff-advance-after-apply-hunk t))

(use-package ediff
  :ensure nil
  :custom
  (ediff-keep-variants nil)
  (ediff-show-clashes-only t)
  (ediff-floating-control-frame t)
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally)
  :config
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

(use-package gud
  :ensure nil
  :hook (gud-mode . gud-tooltip-mode)
  :custom
  (gud-highlight-current-line t))

(use-package compile
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output t)
  (compilation-scroll-output 'first-error)
  (compilation-auto-jump-to-first-error t))

(use-package xref
  :ensure nil
  :hook ((xref-after-return xref-after-jump) . recenter)
  :custom
  (xref-search-program 'ripgrep)
  (xref-file-name-display 'project-relative)
  (xref-history-storage 'xref-window-local-history)
  (xref-show-xrefs-function 'xref-show-definitions-buffer)
  (xref-show-definitions-function 'xref-show-definitions-completing-read))

(use-package flymake
  :ensure nil
  :custom
  (flymake-no-changes-timeout nil)
  (flymake-show-diagnostics-at-end-of-line t)
  (flymake-fringe-indicator-position 'right-fring))

(use-package apheleia
  :defer 5
  :config (apheleia-global-mode +1))

(use-package reformatter)

(use-package ws-butler
  :hook ((prog-mode conf-mode) . ws-butler-mode))

;; %% snippet
(use-package tempel
  :defer 3
  :bind (("M-+" . tempel-insert)
         ("M-=" . tempel-complete)
         :map tempel-map
         ([tab] . tempel-next)
         ([backtab] . tempel-previous))
  :custom
  (tempel-trigger-prefix "<")
  (tempel-path (nol-expand-etc "templates/tempel.eld"))
  :config
  (defun yx/tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons 'tempel-expand completion-at-point-functions)))
  (add-hook 'conf-mode-hook #'yx/tempel-setup-capf)
  (add-hook 'prog-mode-hook #'yx/tempel-setup-capf)
  (add-hook 'text-mode-hook #'yx/tempel-setup-capf)
  (add-hook 'eglot-managed-mode-hook #'yx/tempel-setup-capf))

;; %% version control
(use-package magit
  :custom
  (magit-clone-default-directory "~/workspace/")
  (magit-diff-refine-hunk 'all)
  (magit-show-long-lines-warning nil)
  (magit-log-arguments '("--color" "--graph" "--decorate"))
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :config
  (defun yx/magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (magit-restore-window-configuration)
    (mapc #'kill-buffer (magit-mode-get-buffers)))
  (keymap-set magit-stash-mode-map "q" #'yx/magit-kill-buffers))

(use-package diff-hl
  :defer 5
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-disable-on-remote t)
  (diff-hl-show-staged-changes nil)
  :config
  (diff-hl-flydiff-mode +1)
  (global-diff-hl-show-hunk-mouse-mode -1))

(use-package git-modes)

;; %% indent
(use-package indent-guide
  :hook (prog-mode . indent-guide-mode)
  :custom
  (indent-guide-recursive nil))

(use-package editorconfig
  :defer 5
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  :config
  (editorconfig-mode 1))

(use-package snap-indent
  :hook (prog-mode . snap-indent-mode)
  :custom
  (snap-indent-format '(delete-trailing-whitespace)))

(use-package isayt
  :vc (:url "https://gitlab.com/andreyorst/isayt.el")
  :hook ((emacs-lisp-mode scheme-mode-hook) . isayt-mode))

;; %% doc
(use-package devdocs)

;; %% symbol highlight
(use-package rainbow-mode
  :hook
  (css-base-mode . rainbow-mode)
  (emacs-lisp-mode . rainbow-mode)
  ((help-mode helpful-mode) . rainbow-mode)
  :custom
  (rainbow-x-colors nil))

(use-package hl-todo
  :hook ((text-mode prog-mode) . hl-todo-mode)
  :init
  (setq hl-todo-keyword-faces
        '(("TODO"  . "#cc9393")
          ("NEXT"  . "#dca3a3")
          ("DONT"  . "#5f7f5f")
          ("FAIL"  . "#8c5353")
          ("HACK"  . "#d0bf8f")
          ("FIXME" . "#cc9393")
          ("ISSUE" . "#e45649")))
  (setq hl-todo-require-punctuation t
        hl-todo-highlight-punctuation ":")
  (defun yx/hl-todo-rg-project ()
    "Use `rg' to find all TODO or similar keywords."
    (interactive)
    (unless (require 'color-rg nil t)
      (error "`color-rg' is not installed"))
    (let* ((regexp (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp))))
      (color-rg-search-input regexp (color-rg-project-root-dir)))))

(use-package symbol-overlay
  :custom (symbol-overlay-priority 0)
  :hook ((prog-mode conf-mode) . symbol-overlay-mode)
  :bind (:map symbol-overlay-map
              ("u" . symbol-overlay-remove-all)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  :config
  (add-to-list 'treesit-extra-load-path
               (nol-expand-var "tree-sitter")))

(use-package treesit-auto
  :defer 2
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs '(c
                        cpp
                        julia
                        latex
                        lua
                        python
                        r
                        yaml
                        toml))
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode +1))

;; %% refoctor
(use-package color-rg
  :vc (:url "https://github.com/manateelazycat/color-rg")
  :defer 2
  :custom
  (color-rg-search-no-ignore-file nil)
  (color-rg-mac-load-path-from-shell nil))

;; %% structured edit
(use-package iedit)

(use-package surround
  :bind-keymap ("C-'" . surround-keymap))

(use-package puni
  :hook ((prog-mode sgml-mode nxml-mode) . puni-mode)
  :custom
  (puni-confirm-when-delete-unbalanced-active-region nil)
  :bind (:map puni-mode-map
              ("DEL"         . nil)     ; confict with hungry-delete
              ("C-M-f"       . puni-forward-sexp-or-up-list)
              ("C-M-b"       . puni-backward-sexp-or-up-list)
              ("C-M-<right>" . puni-slurp-forward)
              ("C-M-<left>"  . puni-slurp-backward)
              ("C-M-<up>"    . puni-barf-forward)
              ("C-M-<down>"  . puni-barf-backward)
              ("C-M-SPC"     . puni-mark-sexp-at-point)
              ("C-M-@"       . puni-mark-sexp-around-point)
              ("C-M-r"       . puni-raise)
              ("C-M-0"       . puni-splice)
              ("C-M-z"       . puni-squeeze)
              ("C-M-t"       . puni-transpose)
              ("C-M-="       . puni-expand-region)
              ("C-M--"       . puni-contract-region)
              :repeat-map puni-e/c-repeat-map
              ("=" . puni-expand-region)
              ("-" . puni-contract-region)))

(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate")
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode))
  :custom
  (combobulate-key-prefix "C-c l o"))

(use-package eglot
  :ensure nil
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-sync-connect nil)
  (eglot-report-progress nil)
  (eglot-events-buffer-size 0)
  (eglot-send-changes-idle-time 0.5)
  (eglot-auto-display-help-buffer nil)
  :config
  (add-to-list 'eglot-stay-out-of 'yasnippet)
  (fset #'jsonrpc--log-event #'ignore)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (defun yx/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'tempel-expand
                       #'cape-file))))
  (add-hook 'eglot-managed-mode-hook #'yx/eglot-capf)
  (use-package consult-eglot :demand t)
  :bind (:map eglot-mode-map
              ("M-c r"   . eglot-rename)
              ("M-c M-f" . eglot-format)
              ("M-c M-a" . eglot-code-actions)
              ("M-c s"   . consult-eglot-symbols)))

(use-package citre
  :custom
  (citre-prompt-language-for-ctags-command t)
  (citre-use-project-root-when-creating-tags t)
  (citre-default-create-tags-file-location 'global-cache)
  (citre-auto-enable-citre-mode-modes '(c-ts-mode
                                        python-ts-mode))
  :config
  (with-eval-after-load 'cc-mode
    (require 'citre-lang-c))
  (with-eval-after-load 'dired
    (require 'citre-lang-fileref)))

(use-package dape
  :init
  (setq dape-adapter-dir (nol-expand-var "dape-debug-adapters")
        dape-buffer-window-arrangment 'right))

(use-package quickrun
  :custom (quickrun-focus-p nil))

(use-package inheritenv
  :demand t
  :config
  (inheritenv-add-advice #'org-babel-eval)
  (inheritenv-add-advice #'with-temp-buffer)
  (inheritenv-add-advice #'async-shell-command)
  (inheritenv-add-advice #'shell-command-to-string))

(use-package buffer-env
  :init
  (add-hook 'comint-mode-hook #'buffer-env-update)
  (add-hook 'hack-local-variables-hook #'buffer-env-update)
  :config
  (add-to-list 'buffer-env-command-alist '("/\\.envrc\\'" . "direnv exec . env -0")))

(use-package symbols-outline
  :unless IS-WIN
  :custom
  (symbols-outline-window-width 35)
  :config
  (unless (executable-find "ctags")
    (setq symbols-outline-fetch-fn #'symbols-outline-lsp-fetch))
  (symbols-outline-follow-mode 1))

;;; Langs
;; %% emacs-lisp
(use-package macrostep
  :custom
  (macrostep-expand-in-separate-buffer t)
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

;; c / c++
(use-package cc-mode
  :ensure nil
  :hook (c-mode . eglot-ensure)
  :config
  (add-to-list 'c-default-style '(c-mode . "linux"))
  (defun yx/cc-mode-common-h ()
    (setq tab-width 8
          indent-tabs-mode t
          comment-start "// "
          comment-end ""
          c-basic-offset 8
          c-electric-pound-behavior 'alignleft)
    (c-toggle-auto-hungry-state 1))
  (add-hook 'c-mode-common-hook #'yx/cc-mode-common-h))

(use-package c-ts-mode
  :ensure nil
  :hook (c-ts-mode . eglot-ensure)
  :init (setq c-ts-mode-indent-offset 8))

;; %% code-cell
(use-package code-cells
  :hook ((julia-mode python-ts-mode) . code-cells-mode-maybe)
  :config
  (setq code-cells-eval-region-commands
        '((python-ts-mode . python-shell-send-region)
          (emacs-lisp-mode . eval-region)))
  (let ((map code-cells-mode-map))
    (keymap-set map "C-c % w" #'code-cells-write-ipynb)
    (keymap-set map "C-c % C-e" #'code-cells-eval-above)
    (keymap-set map "n" (code-cells-speed-key #'code-cells-forward-cell))
    (keymap-set map "p" (code-cells-speed-key #'code-cells-backward-cell))
    (keymap-set map "e" (code-cells-speed-key #'code-cells-eval))
    (keymap-set map "C-e" (code-cells-speed-key #'code-cells-eval-above))
    (keymap-set map "TAB" (code-cells-speed-key #'outline-cycle)))
  (with-eval-after-load 'jupyter
    (defalias 'adopt-jupyter-eval-region (apply-partially 'jupyter-eval-region nil))
    (add-to-list 'code-cells-eval-region-commands
                 '(jupyter-repl-interaction-mode . adopt-jupyter-eval-region)))
  (with-eval-after-load 'julia-snail
    (add-to-list 'code-cells-eval-region-commands
                 '(julia-snail-mode . julia-snail-send-code-cell)))
  )

;; %% python
(use-package python
  :ensure nil
  :config
  (setq python-shell-dedicated t
        python-skeleton-autoinsert t
        python-indent-block-paren-deeper t
        python-indent-guess-indent-offset t
        python-indent-guess-indent-offset-verbose nil
        python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-completion-native-disabled-interpreters '("ipython" "jupyter"))
  (defun yx/python-mode-setup ()
    (setq-local tab-width 4
                python-indent-offset 4
                electric-indent-inhibit t
                devdocs-current-docs '("python~3.12"
                                       "pytorch~2"
                                       "numpy~1.23"))
    (setq-local imenu-create-index-function #'python-imenu-create-flat-index)
    (eglot-ensure)
    (flymake-mode 1))
  (add-hook 'python-base-mode-hook 'yx/python-mode-setup)
  (reformatter-define black-format :program "black" :args '("-q" "-"))
  (reformatter-define ruff-format :program "ruff" :args '("--fix-only" "-")))

(use-package python-mls
  :hook (inferior-python-mode . python-mls-mode))

(use-package jupyter
  :after org
  :demand t
  :config
  (setq jupyter-eval-use-overlays nil)
  ;; @see https://github.com/emacs-jupyter/jupyter/issues/478
  (setf (alist-get "python" org-src-lang-modes nil nil #'equal) 'python-ts))

;; %% R/julia
(use-package ess-site
  :ensure ess
  :hook (R-mode . eglot-ensure)
  :config
  (setq ess-eval-visibly-p 'nowait
        ess-local-process-name "R"
        ess-ask-for-ess-directory nil)
  (keymap-set ess-r-mode-map ";" 'ess-insert-assign)
  (keymap-set inferior-ess-r-mode-map ";" 'ess-insert-assign))

(use-package julia-mode)
(use-package julia-ts-mode
  :config
  (defun yx/julia-ts-setup ()
    (eglot-ensure)
    (setq-local devdocs-current-docs '("julia~1.9")))
  (add-hook 'julia-ts-mode-hook #'yx/julia-ts-setup))

(use-package eglot-jl
  :init
  (with-eval-after-load 'eglot
    (eglot-jl-init)))

(use-package julia-snail
  :custom
  (julia-snail-terminal-type :eat)
  (julia-snail-extensions '(ob-julia formatter))
  :hook
  (julia-mode . julia-snail-mode)
  (julia-ts-mode . julia-snail-mode))

;; scheme && lisp
(setq scheme-program-name "chez"
      inferior-lisp-program "sbcl")

(use-package geiser-chez
  :mode ("\\.sc\\'" . scheme-mode)
  :hook (scheme-mode . turn-on-geiser-mode)
  :custom (geiser-chez-binary "chez"))

(use-package sly
  :hook (lisp-mode . sly-mode))

(use-package haskell-mode
  :custom
  (haskell-stylish-on-save t)
  (haskell-process-log t)
  (haskell-process-auto-import-loaded-modules t)
  :config
  (defun yx/haskell-mode-setup ()
    (haskell-collapse-mode 1)
    (haskell-decl-scan-mode 1)
    (haskell-auto-insert-module-template)
    (speedbar-add-supported-extension ".hs")
    (eval-after-load "which-func"
      '(add-to-list 'which-func-modes 'haskell-mode))
    (eglot-ensure))
  (add-hook 'haskell-mode-hook #'yx/haskell-mode-setup))

;; %% misc lang
(use-package sh-script
  :ensure nil
  :hook (sh-mode . yx/sh-mode-setup)
  :config
  (defun yx/sh-mode-setup ()
    (electric-pair-local-mode -1)
    (compilation-shell-minor-mode 1))
  :custom
  (sh-indentation 2)
  (sh-basic-offset 2))

(use-package sgml-mode
  :ensure nil
  :hook
  (html-mode . sgml-name-8bit-mode)
  (html-mode . sgml-electric-tag-pair-mode)
  :custom
  (sgml-basic-offset 2))

(use-package nxml-mode
  :ensure nil
  :custom
  (nxml-slash-auto-complete-flag t)
  (nxml-auto-insert-xml-declaration-flag t))

(use-package csv-mode)

(use-package vimrc-mode
  :mode "\\.?vim\\(rc\\)?\\'")

(use-package gnuplot-mode
  :mode "\\.gp$")

(use-package graphviz-dot-mode)

;; %% maxima
(unless IS-WIN
  (autoload 'maxima-mode "maxima" "Maxima mode" t)
  (autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
  (autoload 'maxima "maxima" "Maxima interaction" t)
  (autoload 'imath-mode "imath" "Imath mode for math formula input" t)
  (setq imaxima-use-maxima-mode-flag t)
  (add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode)))

(load custom-file t)
;;; init.el ends here
