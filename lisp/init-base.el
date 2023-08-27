;;; init-base.el --- basic  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:02:02
;; Modified: <2023-08-27 15:06:38 yx>
;; Licence: GPLv3

;;; Commentary:

;; better defaults

;;; Code:

;; %% basic
(setq-default
 tab-width 2
 abbrev-mode t
 fill-column 78
 line-spacing 0.1
 truncate-lines t
 indent-tabs-mode nil
 require-final-newline t
 bidi-display-reordering nil
 tab-always-indent 'complete)

(setq
 track-eol t
 visible-bell nil
 view-read-only t
 use-short-answers t
 line-move-visual nil
 system-time-locale "C"
 hl-line-sticky-flag nil
 auto-revert-verbose nil
 require-final-newline t
 confirm-kill-processes nil
 find-file-visit-truename t
 initial-scratch-message ""
 delete-by-moving-to-trash t
 set-mark-command-repeat-pop t
 disabled-command-function nil
 show-paren-context-when-offscreen t
 compilation-scroll-output 'first-error
 backward-delete-char-untabify-method 'hungry
 sentence-end-double-space nil
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

;; %% autosave
(setq
 auto-save-no-message t
 auto-save-visited-interval 30)

;; %% autorevert
(customize-set-variable 'auto-revert-use-notify nil)
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; %% autoinsert
(setq
 auto-insert-query nil
 auto-insert-alist nil)
(add-hook 'after-init-hook 'auto-insert-mode)

(setq
 time-stamp-pattern "10/^[@#;\*].*[Mm]odified: <%%>$")
(add-hook 'before-save-hook 'time-stamp)

;; %% backup
(setq
 version-control t
 backup-by-copying t
 kept-new-versions 6
 kept-old-versions 2
 delete-old-versions t)

;; %% eldoc
(setq
 eldoc-echo-area-use-multiline-p 3
 eldoc-echo-area-display-truncation-message nil)

;; %% uniquify
(setq
 uniquify-strip-common-suffix t
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*"
 uniquify-buffer-name-style 'post-forward-angle-brackets)

;; %% line number
(setq
 display-line-numbers-type 'visual
 display-line-numbers-width-start t)

;; %% recentf
(setq
 recentf-max-saved-items 100
 recentf-auto-cleanup 'never
 recentf-exclude
 '(".*\\.cache.*" "^/.*" "^/ssh:"
   "COMMIT_MSG" "COMMIT_EDITMSG" "/Downloads/" "/elpa/"
   "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
   file-remote-p))
(add-hook 'after-init-hook 'recentf-mode)


;; %% whitespace
(setq
 whitespace-line-column nil
 show-trailing-whitespace nil
 whitespace-action '(auto-clean))

;; %% xref
(setq
 xref-search-program
 (cond
  ((executable-find "rg") 'ripgrep)
  ((executable-find "ugrep") 'ugrep)
  (t 'grep)))
(setq
 xref-show-xrefs-function 'xref-show-definitions-completing-read
 xref-show-definitions-function 'xref-show-definitions-completing-read)

;; %% completion
(setq
 completions-detailed t
 completion-ignore-case t
 completions-format 'horizontal
 completions-header-format nil
 completions-max-height 30
 completion-auto-help 'visible
 completion-cycle-threshold 3
 completion-show-help nil
 completion-show-inline-help nil
 completion-auto-select 'second-tab)

(setq
 save-abbrevs 'silently
 abbrev-suggest-hint-threshold 3)

(setq
 hippie-expand-max-buffers 10
 hippie-expand-try-functions-list
 '(try-complete-file-name
   try-complete-file-name-partially
   try-expand-dabbrev
   try-expand-dabbrev-from-kill
   try-expand-dabbrev-all-buffers)
 )

;; %% isearch
(setq
 isearch-lazy-count t
 isearch-allow-motion t
 apropos-sort-by-scores t
 lazy-highlight-no-delay-length 3)

;; %% epa
(setq
 auth-sources
 (list
  (expand-file-name "etc/authinfo.gpg" user-emacs-directory))
 epa-pinentry-mode 'loopback
 epa-file-select-keys yx/gpg-encrypt-key
 )

;; %% mouse
(setq
 mouse-yank-at-point t
 mouse-wheel-tilt-scroll t
 mouse-wheel-follow-mouse t
 mouse-drag-mode-line-buffer t
 mouse-wheel-progressive-speed nil
 mouse-drag-and-drop-region-cross-program t
 mouse-wheel-scroll-amount-horizontal 2
 mouse-wheel-scroll-amount '(2 ((shift) . hscroll)))

;; %% scroll
(setq
 scroll-step 1
 scroll-margin 1
 scroll-conservatively 101
 fast-but-imprecise-scrolling t
 scroll-preserve-screen-position 'always)

;; %% eww browse-url
(setq
 eww-auto-rename-buffer 'title
 browse-url-browser-function 'eww-browse-url
 browse-url-browser-program yx/default-open-program)
(with-eval-after-load 'eww
  (add-hook 'eww-after-render-hook 'eww-readable))

;; %% flyspell
(setq
 ispell-dictionary "english"
 ispell-program-name "hunspell"
 ispell-local-dictionary "en_US"
 ispell-local-dictionary-alist
 '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))
 ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)
(with-eval-after-load 'flyspell
  (keymap-unset flyspell-mode-map "C-,")
  (keymap-unset flyspell-mode-map "C-.")
  (keymap-unset flyspell-mode-map "C-;")
  )

;; %% savehist
(setq
 history-delete-duplicates t
 savehist-additional-variables
 '(mark-ring
   global-mark-ring
   search-ring
   regexp-search-ring
   extended-command-history))
(add-hook 'after-init-hook 'savehist-mode)

;; %% session
(setq
 desktop-save t
 desktop-restore-eager 5
 desktop-restore-frames nil
 desktop-auto-save-timeout 60
 desktop-load-locked-desktop 'check-pid)
;; (add-hook 'after-init-hook 'desktop-save-mode)

;; %% tramp speed up
(setq
 tramp-verbose 1
 tramp-default-method "ssh"
 remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; %% calendar && diary
(setq
 diary-number-of-entries 7)

(setq
 calendar-date-style 'iso
 calendar-latitude +30.67
 calendar-longitude +104.07
 calendar-week-start-day 1
 calendar-mode-line-format nil
 calendar-mark-holidays-flag t
 calendar-mark-diary-entries-flag t
 calendar-view-diary-initially-flag t)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

;; %% os specific settings stay here
(cond
 (-is-mac
  (setq
   mac-option-modifier 'meta
   mac-command-modifier 'super
   ns-function-modifier 'hyper
   ns-use-thin-smoothing t
   ns-use-native-fullscreen nil
   insert-directory-program "gls"))
 (-is-win
  (setq
   w32-apps-modifier 'hyper
   w32-lwindow-modifier 'super))
 )

;; %% hook
(add-hook
 #'text-mode
 (lambda ()
   (setq-local
    word-wrap t
    word-wrap-by-category t
    truncate-lines nil)
   (visual-line-mode 1)
   (goto-address-mode 1))
 )

(add-hook
 #'after-init-hook
 (lambda ()
   (repeat-mode 1)
   (ffap-bindings)
   (save-place-mode 1)
   (blink-cursor-mode -1)
   (auto-compression-mode 1)
   (delete-selection-mode 1)
   (auto-save-visited-mode 1)
   (windmove-default-keybindings)
   (pixel-scroll-precision-mode 1))
 )

;; never kill scratch
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

;; %% end
(provide 'init-base)
;;; init-base.el ends here
