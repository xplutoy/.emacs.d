;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:02:02
;; Modified: <2023-11-28 09:09:01 yx>
;; Licence: GPLv3

;;; Commentary:

;; better defaults

;;; Code:

;; %%
(setq custom-file
      (expand-file-name "custom.el" yx/etc-dir))
(load custom-file 'noerror)

(setq user-full-name "yangxue")
(setq user-mail-address "yangxue.cs@foxmail.com")

(setq system-time-locale "C")
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

(setq-default
 tab-width 8
 abbrev-mode t
 fill-column 78
 line-spacing 0.1
 truncate-lines t
 indent-tabs-mode nil
 tab-always-indent 'complete)

(setq
 track-eol t
 view-read-only nil
 line-move-visual nil
 align-to-tab-stop nil
 find-file-visit-truename t
 delete-by-moving-to-trash t
 bookmark-save-flag 1
 set-mark-command-repeat-pop t
 cursor-in-non-selected-windows nil
 compilation-scroll-output 'first-error
 backward-delete-char-untabify-method 'hungry
 sentence-end-double-space nil
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

;; %% quiet
(setq
 visible-bell nil
 use-short-answers t
 hl-line-sticky-flag nil
 global-hl-line-sticky-flag nil
 auto-revert-verbose nil
 initial-scratch-message ""
 confirm-kill-processes nil
 disabled-command-function nil)

;; %%
(setq
 show-paren-style 'parenthesis
 show-paren-context-when-offscreen t
 show-paren-when-point-inside-paren t
 )

;; %% font-lock
(setq
 jit-lock-defer-time 0
 jit-lock-chunk-size 6000
 jit-lock-stealth-time 0.5
 jit-lock-context-time 0.2)

(setq redisplay-skip-fontification-on-input t)

;; %% auto
(setq
 auto-save-default t
 auto-save-no-message t
 auto-save-visited-interval 10
 delete-auto-save-files t
 auto-save-visited-predicate 'buffer-modified-p
 kill-buffer-delete-auto-save-files t)

(customize-set-variable 'auto-revert-use-notify nil)
(add-hook 'after-init-hook 'global-auto-revert-mode)

(setq
 auto-insert-query nil
 auto-insert-alist nil
 auto-insert-directory
 (expand-file-name "templates/" no-littering-etc-directory))

(add-hook 'after-init-hook 'auto-insert-mode)

(setq
 time-stamp-pattern "10/^[@#;\*].*[Mm]odified: <%%>$")

(add-hook 'before-save-hook 'time-stamp)

(setq tempo-interactive t)

;; %% backup
(setq
 version-control t
 backup-by-copying t
 kept-new-versions 6
 kept-old-versions 2
 delete-old-versions t)

(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;; %% eldoc
(setq
 eldoc-idle-delay 0.3
 eldoc-echo-area-use-multiline-p nil
 eldoc-echo-area-display-truncation-message nil)

;; %% uniquify
(setq
 uniquify-strip-common-suffix t
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*"
 uniquify-buffer-name-style 'post-forward-angle-brackets)

;; %% line number
(setq
 display-line-numbers-type t
 display-line-numbers-width-start t
 )

;; re-builder
(setq reb-re-syntax 'string)

;; %% recentf
(setq
 recentf-max-menu-items 15
 recentf-max-saved-items 50
 recentf-auto-cleanup "1:00am"
 recentf-exclude
 '("\\.?cache.*" "^/.*" "^/ssh:" "\\.git/.+$"
   "COMMIT_MSG" "COMMIT_EDITMSG" "/Downloads/" "/elpa/"
   "\\.\\(?:gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
   "\\.\\(?:gz\\|zip\\|gpg\\)$"
   file-remote-p)
 )

(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory)))
(add-hook 'after-init-hook 'recentf-mode)

;; %% whitespace
(setq
 whitespace-style
 '(face spaces empty tabs newline trailing space-mark tab-mark newline-mark)
 whitespace-line-column nil
 show-trailing-whitespace nil)

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

;; %% completion minibuffer
(setq
 resize-mini-windows 't
 max-mini-window-height 30)

(setq
 completions-detailed t
 completion-ignore-case t
 completions-format 'one-column
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
 auth-source-debug t
 epa-pinentry-mode 'loopback
 epa-file-select-keys yx/gpg-encrypt-key)
(auth-source-pass-enable)

(setq
 password-cache t
 password-cache-expiry (* 60 60))

;; %% mouse
(setq
 mouse-highlight nil
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

(setq
 auto-window-vscroll nil
 auto-hscroll-mode 'current-line)

(setq
 transient-detect-key-conflicts t
 transient-highlight-mismatched-keys nil)

;; %% media
(setq
 image-use-external-converter t)

;; %% browse url
(setq
 browse-url-generic-program yx/default-open-program
 browse-url-browser-function 'eww-browse-url
 eww-auto-rename-buffer 'title
 eww-search-prefix "https://cn.bing.com/search?q=")

(with-eval-after-load 'eww
  (add-hook 'eww-after-render-hook 'eww-readable))

(setq-default shr-use-fonts nil)
(setq-default shr-inhibit-images t)

(setq
 webjump-sites
 '(("Wikipedia" .
    [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""]))
 webjump-use-internal-browser t)

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
 savehist-save-minibuffer-history t
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
 desktop-restore-eager 2
 desktop-restore-frames nil
 desktop-auto-save-timeout 60
 desktop-load-locked-desktop t
 desktop-files-not-to-save "\\(\\`/[^/:]*:\\|(ftp)\\'\\|(ssh)\\'\\|\\.org$\\|^/tmp/\\)")

(add-hook 'after-init-hook
          (lambda ()
            (desktop-read)
            (desktop-save-mode 1))
          )

(with-eval-after-load 'desktop
  (dolist (mode '(eww-mode
                  Info-mode
                  dired-mode
                  comint-mode
                  doc-view-mode
                  elfeed-search-mode))
    (add-to-list 'desktop-modes-not-to-save mode))
  )

;; %% tramp speed up
(setq
 tramp-verbose 1
 tramp-chunksize 2000
 tramp-default-method "ssh"
 remote-file-name-inhibit-locks t
 remote-file-name-inhibit-cache nil)

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; %% long line
(setq
 bidi-inhibit-bpa t
 long-line-threshold 1000
 syntax-wholeline-max 1000
 large-hscroll-threshold 1000)

(setq-default
 bidi-display-reordering nil
 bidi-paragraph-direction 'left-to-right)

(add-hook 'after-init-hook 'global-so-long-mode)

;; %% calendar
(setq
 calendar-holidays
 '((holiday-chinese 12 30 "春节")
   (holiday-chinese 1 1   "春节")
   (holiday-chinese 1 2   "春节")
   (holiday-chinese 5 5   "端午节")
   (holiday-chinese 1 15  "元宵节")
   (holiday-chinese 7 7   "七夕节")
   (holiday-chinese 9 9   "重阳节")
   (holiday-chinese 8 15  "中秋节")
   (holiday-fixed 1 1     "元旦")
   (holiday-fixed 10 1    "国庆节")
   (holiday-fixed 10 2    "国庆节")
   (holiday-fixed 10 3    "国庆节")
   (holiday-fixed 3 8     "妇女节")
   (holiday-fixed 3 12    "植树节")
   (holiday-fixed 5 1     "劳动节")
   (holiday-fixed 5 4     "青年节")
   (holiday-fixed 6 1     "儿童节")
   (holiday-fixed 9 10    "教师节")
   (holiday-fixed 2 14    "情人节")
   (holiday-fixed 4 1     "愚人节")
   (holiday-float 5 0 2   "母亲节")
   (holiday-float 6 0 3   "父亲节")
   (holiday-float 11 4 4  "感恩节")
   (holiday-fixed 12 25   "圣诞节")
   ))

(setq
 appt-audible t
 appt-display-interval 10
 appt-display-duration 5
 appt-message-warning-time 20
 appt-display-format 'window
 appt-disp-window-function 'yx/appt-display-with-notification)

(add-hook 'after-init-hook 'appt-activate)

(setq
 calendar-latitude +30.67
 calendar-longitude +104.07
 calendar-date-style 'iso
 calendar-week-start-day 1
 calendar-mode-line-format nil
 calendar-mark-holidays-flag t
 calendar-mark-diary-entries-flag nil
 calendar-chinese-celestial-stem
 ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"]
 calendar-chinese-terrestrial-branch
 ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])

(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

;; %% os specific settings stay here
(cond
 (IS-MAC
  (setq
   mac-option-modifier  'meta
   mac-command-modifier 'super
   ns-function-modifier 'hyper
   ns-use-thin-smoothing t
   ns-use-native-fullscreen nil
   insert-directory-program "gls"))
 (IS-WIN
  (setq
   w32-apps-modifier 'hyper
   w32-lwindow-modifier 'super))
 )

(ffap-bindings)

;; %% hook
(defun yx/text-mode-setup ()
  (setq-local
   word-wrap t
   word-wrap-by-category t)
  (visual-line-mode 1)
  (goto-address-mode 1)
  (toggle-truncate-lines -1))

(defun yx/global-mirror-mode-setup ()
  (repeat-mode 1)
  (save-place-mode 1)
  (blink-cursor-mode -1)
  (auto-compression-mode 1)
  (delete-selection-mode 1)
  (auto-save-visited-mode 1)
  (pixel-scroll-precision-mode 1)
  (windmove-default-keybindings 'control))

(add-hook 'text-mode 'yx/text-mode-setup)
(add-hook 'after-init-hook 'yx/global-mirror-mode-setup)

;; never kill scratch
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))


(provide 'init-basic)
;;; init-basic.el ends here