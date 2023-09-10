;;; init-base.el --- basic  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:02:02
;; Modified: <2023-09-10 14:28:39 yx>
;; Licence: GPLv3

;;; Commentary:

;; better defaults

;;; Code:

;; %%
(setq-default
 tab-width 2
 abbrev-mode t
 fill-column 78
 line-spacing 0.1
 truncate-lines t
 indent-tabs-mode nil
 bidi-display-reordering nil
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
 show-paren-context-when-offscreen t
 compilation-scroll-output 'first-error
 backward-delete-char-untabify-method 'hungry
 sentence-end-double-space nil
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

;; %% quiet
(setq
 visible-bell nil
 use-short-answers t
 hl-line-sticky-flag nil
 auto-revert-verbose nil
 initial-scratch-message ""
 confirm-kill-processes nil
 disabled-command-function nil)

;; %% font-lock
(setq
 jit-lock-defer-time 0
 jit-lock-chunk-size 6000
 jit-lock-stealth-time 0.5
 jit-lock-context-time 0.2)

;; %% auto
(setq
 auto-save-no-message t
 auto-save-visited-interval 30)

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
 display-line-numbers-width-start t
 )

;; re-builder
(setq reb-re-syntax 'string)

;; %% recentf
(setq
 recentf-max-saved-items 50
 recentf-auto-cleanup 'never
 recentf-exclude
 '(".*\\.cache.*" "^/.*" "^/ssh:"
   "COMMIT_MSG" "COMMIT_EDITMSG" "/Downloads/" "/elpa/"
   "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
   file-remote-p)
 )

(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory)))
(add-hook 'after-init-hook 'recentf-mode)

;; %% whitespace
(setq-default whitespace-style
              '(face spaces empty tabs newline trailing space-mark tab-mark newline-mark))
(setq whitespace-line-column nil
      show-trailing-whitespace nil
      whitespace-action '(clean auto-clean)
      )

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
 resize-mini-windows nil
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
 desktop-restore-eager 5
 desktop-restore-frames nil
 desktop-auto-save-timeout 60
 desktop-load-locked-desktop t
 desktop-files-not-to-save "\\(\\`/[^/:]*:\\|(ftp)\\'\\|(ssh)\\'\\|\\.org$\\|^/tmp/\\)")

(desktop-save-mode 1)

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
 tramp-default-method "ssh"
 remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; %% calendar
(setq calendar-holidays
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
 appt-display-format 'window
 appt-display-interval 5
 appt-display-duration 120
 appt-message-warning-time 20)

(add-hook 'after-init-hook
          (lambda ()
            (appt-activate 1)))

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
   (pixel-scroll-precision-mode 1)
   (windmove-default-keybindings 'control))
 )

;; never kill scratch
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

;; %% end
(provide 'init-base)
;;; init-base.el ends here
