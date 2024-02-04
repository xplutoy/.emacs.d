;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:13:09
;; Modified: <2024-02-05 07:53:20 yx>
;; Licence: GPLv3

;;; Init
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(defvar yx/etc-dir         "~/.emacs.d/etc/")
(defvar yx/var-dir         "~/.emacs.d/.cache/")

;; env
(setenv "http_proxy"  "http://127.0.0.1:7890")
(setenv "https_proxy" "http://127.0.0.1:7890")

(require 'package)
(setq
 package-archives
 '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
   ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
   ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))
(setq
 package-quickstart nil
 use-package-verbose t
 use-package-always-defer t
 use-package-always-ensure t
 use-package-expand-minimally t
 use-package-enable-imenu-support t
 package-install-upgrade-built-in nil
 package-user-dir (expand-file-name "elpa" yx/var-dir)
 package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))

(when (daemonp) (setq use-package-always-demand t))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(require 'use-package)

;; %% benchmark
(use-package benchmark-init)
(benchmark-init/activate)
(add-hook 'after-init-hook 'benchmark-init/deactivate)

;; %% no-littering
(use-package no-littering
  :demand t
  :init
  (setq no-littering-var-directory yx/var-dir
        no-littering-etc-directory yx/etc-dir)
  :config
  (no-littering-theme-backups)
  )

;;; Utils
(require 'tempo)

(defvar yx/org-dir         "~/yxdocs/org-notes/")
(defvar yx/zotero-dir      "~/Zotero/")
(defvar yx/gpg-sign-key    "67B86CB8A5630C51!")
(defvar yx/gpg-encrypt-key "8B1F9B207AF00BCF!")

(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-WIN   (eq system-type 'windows-nt))
(defconst IS-LINUX (eq system-type 'gnu/linux))

(defvar yx/default-open-program
  (cond (IS-WIN   "start")
        (IS-MAC   "open")
        (IS-LINUX "xdg-open")
        (t "")))

(defconst yx/templates-dir
  (expand-file-name "templates" no-littering-etc-directory))

(defconst yx/cal-china-x-days
  ["日" "一" "二" "三" "四" "五" "六"])

(defconst yx/cal-china-x-month-name
  ["正月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "冬月" "腊月"])

(defconst yx/cal-china-x-day-name
  ["初一" "初二" "初三" "初四" "初五" "初六" "初七" "初八" "初九" "初十"
   "十一" "十二" "十三" "十四" "十五" "十六" "十七" "十八" "十九"  "廿"
   "廿一" "廿二" "廿三" "廿四" "廿五" "廿六" "廿七" "廿八" "廿九" "三十"
   "卅一" "卅二" "卅三" "卅四" "卅五" "卅六" "卅七" "卅八" "卅九" "卅十"])

;; %% functions
(defun yx/pwd-replace-home (pwd)
  "Replace home in PWD with tilde (~) character."
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defmacro yx/prefixs-to-regex (&rest prefixs)
  "Convert a list of buffer-name prefex to regex."
  `(rx bos (or ,@prefixs)))

(defun yx/file-contents-str (file)
  "File contents to string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string))
  )

(defun yx/org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
  This function makes sure that dates are aligned for easy reading."
  (let* ((dayname (aref yx/cal-china-x-days
                        (calendar-day-of-week date)))
         (day (cadr date))
         (month (car date))
         (year (nth 2 date))
         (cn-date (calendar-chinese-from-absolute (calendar-absolute-from-gregorian date)))
         (cn-month (cl-caddr cn-date))
         (cn-day (cl-cadddr cn-date))
         (cn-month-string (concat (aref yx/cal-china-x-month-name
                                        (1- (floor cn-month)))
                                  (if (integerp cn-month)
                                      ""
                                    "[闰]")))
         (cn-day-string (aref yx/cal-china-x-day-name
                              (1- cn-day))))
    (format "%04d-%02d-%02d 周%-8s 农历%s%s" year month
            day dayname cn-month-string cn-day-string)))

(defun yx/is-it-dark-p ()
  "Return t if it's dark outside, otherwise nil."
  (let* ((solar (solar-sunrise-sunset (calendar-current-date)))
         (sunrise (car (nth 0 solar)))
         (sunset (car (nth 1 solar)))
         (time (decode-time (current-time)))
         (hour (nth 2 time))
         (minute (nth 1 time))
         (minute-frac (/ minute 60.0))
         (time-decimal (+ hour minute-frac)))
    (or (> time-decimal sunset) (< time-decimal sunrise)))
  )

(defun yx/diary-sunrise-sunset-split ()
  "Split `solar-sunrise-sunset-string' into sunrise, sunset, and daylight hours."
  (let* ((string (solar-sunrise-sunset-string (calendar-current-date)))
         (regexp (rx (group "Sunrise " (1+ (or digit ":")) (or "am" "pm")) " "
                     (group "(" (1+ alpha) ")") ", "
                     (group "sunset " (1+ (or digit ":")) (or "am" "pm")) " "
                     (group "(" (1+ alpha) ")")
                     (1+ anything)
                     "(" (group (1+ (or digit ":")))
                     ))
         (sunrise (progn
                    (string-match regexp string)
                    (match-string 1 string)) )
         (sunset (capitalize (match-string 3 string)))
         (daylight (format "%s of daylight" (match-string 5 string))))
    (list sunrise sunset daylight))
  )

(defun yx/diary-sunrise ()
  (elt (yx/diary-sunrise-sunset-split) 0))

(defun yx/diary-sunset ()
  (elt (yx/diary-sunrise-sunset-split) 1))

(cond
 (IS-MAC
  (defun yx/notify-send (&rest params)
    "Send notifications via `terminal-notifier'."
    (let ((title (plist-get params :title))
          (body (plist-get params :body)))
      (start-process "terminal-notifier"
                     nil
                     "terminal-notifier"
                     "-group" "Emacs"
                     "-title" title
                     "-message" body
                     "-activte" "org.gnu.Emacs"))))
 (t
  (defalias 'yx/notify-send 'notifications-notify)))

(defun yx/appt-display-with-notification (min-to-app new-time appt-msg)
  (yx/notify-send :title (format "Appointment in %s minutes" min-to-app)
                  :body appt-msg
                  :urgency 'critical)
  (appt-disp-window min-to-app new-time appt-msg))

;; %% tempo skeleton
(tempo-define-template
 "yx/tex-note-tmpl"
 `(,(yx/file-contents-str (expand-file-name "math-note.tmpl.tex" yx/templates-dir)))
 )

(define-skeleton yx/latex-graphics-skl
  "Insert centered picture."
  nil
  > "\\begin{center}" \n
  > "\\includegraphics[width=" @ (skeleton-read "Width: ") "]{" @ _ "}" \n
  > "\\begin{center}" > \n @)

(define-skeleton yx/auto-insert-h-header
  ""
  (replace-regexp-in-string
   "[^A-Z0-9]" "_"
   (string-replace "+" "P"
                   (upcase
                    (file-name-nondirectory buffer-file-name))))
  "/**\n***************************************************"
  "\n* @author: "
  (user-full-name)
  "\n* @date: "
  (format-time-string "%F %T")
  "\n* @brief: "
  (skeleton-read "brief: ")
  "\n* @modified: <>"
  "\n**************************************************\n*/"
  "\n\n#ifndef " str \n "#define " str
  "\n\n" @ _
  "\n\n#endif"
  )

(define-skeleton yx/auto-insert-c-header
  ""
  nil
  "/**\n***************************************************"
  "\n* @author: "
  (user-full-name)
  "\n* @date: "
  (format-time-string "%F %T")
  "\n* @modified: <>"
  "\n**************************************************\n*/"
  "\n\n" @ _ "\n"
  )

(define-skeleton yx/auto-insert-common-header
  ""
  nil
  "# --------------------------------------------------"
  "\n# Author: "
  (user-full-name)
  "\n# Date: "
  (format-time-string "%F %T")
  "\n# Modified: <>\n#"
  "\n# Description: "
  (skeleton-read "Description: ")
  "\n#\n#\n"
  "# --------------------------------------------------"
  "\n\n" @ _ "\n"
  )

(define-skeleton yx/auto-insert-el-header
  ""
  nil
  ";;; -*- lexical-binding: t -*-"
  '(setq lexical-binding t)
  "\n\n;; Author: "
  (user-full-name)
  " <"
  (progn user-mail-address)
  ">"
  "\n;; Copyright (C) "
  (format-time-string "%Y")
  ", "
  (user-full-name)
  ", all right reserved."
  "\n;; Created: "
  (format-time-string "%F %T")
  "\n;; Modified: <>"
  "\n;; Licence: GPLv3"
  "\n\n;;; Commentary:\n\n;; " @ _
  "\n\n;;; Code:\n\n(provide '"
  (file-name-base
   (buffer-file-name))
  ")\n;;; "
  (file-name-nondirectory
   (buffer-file-name))
  " ends here\n"
  )

;;; Defaults
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
 word-wrap-by-category t
 find-file-visit-truename t
 delete-by-moving-to-trash t
 set-mark-command-repeat-pop t
 cursor-in-non-selected-windows nil
 compilation-scroll-output 'first-error
 backward-delete-char-untabify-method 'hungry
 sentence-end-double-space nil
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

(setq
 visible-bell nil
 use-dialog-box nil
 use-file-dialog nil
 use-system-tooltips nil
 use-short-answers t
 y-or-n-p-use-read-key t)

;; %% quiet
(setq
 hl-line-sticky-flag nil
 global-hl-line-sticky-flag nil
 initial-scratch-message ""
 confirm-kill-processes nil
 disabled-command-function nil
 confirm-nonexistent-file-or-buffer nil)

;; %% paren
(setq
 blink-matching-paren nil
 show-paren-style 'parenthesis
 show-paren-context-when-offscreen t
 show-paren-when-point-inside-paren t)

;; %% electric
(setq
 electric-pair-preserve-balance t
 electric-pair-inhibit-predicate
 'electric-pair-conservative-inhibit
 )

;; %% font-lock
(setq
 jit-lock-defer-time 0
 jit-lock-stealth-time 2.0
 jit-lock-stealth-nice 0.2)

;; %% auto save
(setq
 auto-save-default t
 auto-save-no-message t
 delete-auto-save-files t
 kill-buffer-delete-auto-save-files t
 auto-save-visited-interval 10
 auto-save-visited-predicate 'buffer-modified-p)

;; %% auto-revert
(setq
 auto-revert-verbose nil
 auto-revert-avoid-polling t
 auto-revert-check-vc-info t)

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
 display-line-numbers-width 4
 display-line-numbers-width-start t)

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
 '(face
   tabs
   spaces
   tab-mark
   space-mark
   trailing
   missing-newline-at-eof
   space-after-tab::tab
   space-after-tab::space
   space-before-tab::tab
   space-before-tab::space)
 whitespace-line-column nil
 show-trailing-whitespace nil)

;; prettify
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; %% xref
(let ((executable (or (executable-find "rg") "grep"))
      (rgp (string-match-p "rg" grep-program)))
  (setq grep-program executable)
  (setq grep-template
        (if rgp
            "rg -nH --null -e <R> <F>"
          "grep <X> <C> -nH --null -e <R> <F>"))
  (setq xref-search-program (if rgp 'ripgrep 'grep)))

(setq
 xref-file-name-display 'project-relative
 xref-history-storage 'xref-window-local-history
 xref-show-xrefs-function 'xref-show-definitions-buffer
 xref-show-definitions-function 'xref-show-definitions-completing-read)

;; %% completion minibuffer
(setq
 resize-mini-windows t
 max-mini-window-height 0.3
 enable-recursive-minibuffers t)

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

(setq read-extended-command-predicate
      'command-completion-default-include-p)

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq
 ;; abbrev-suggest t
 save-abbrevs 'silently
 abbrev-suggest-hint-threshold 2)

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
 isearch-lazy-highlight t
 isearch-allow-motion t
 isearch-repeat-on-direction-change t
 apropos-sort-by-scores t
 lazy-highlight-no-delay-length 3)

(keymap-set isearch-mode-map "M->" 'isearch-end-of-buffer)
(keymap-set isearch-mode-map "M-<" 'isearch-beginning-of-buffer)
(if IS-MAC (keymap-set isearch-mode-map "s-v" 'isearch-yank-kill))

(add-hook 'occur-mode-hook #'hl-line-mode)

;; %% epa
(setq
 auth-sources
 (list
  (expand-file-name "etc/authinfo.gpg" user-emacs-directory))
 auth-source-debug t
 epa-pinentry-mode 'loopback
 epa-file-select-keys yx/gpg-encrypt-key)

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
 mouse-avoidance-nudge-dist 8
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

;; bookmark
(setq
 bookmark-save-flag 1
 bookmark-fringe-mark nil
 bookmark-use-annotations nil
 bookmark-automatically-show-annotations nil)

;; proced
(setq
 proced-descend t
 proced-filter 'user
 proced-auto-update-flag t
 proced-enable-color-flag t
 proced-auto-update-interval 5)

;; %% media
(setq
 image-use-external-converter t)

;; %% browse url
(setq
 browse-url-browser-function 'eww-browse-url
 browse-url-secondary-browser-function 'browse-url-default-browser
 browse-url-generic-program yx/default-open-program
 eww-auto-rename-buffer 'title
 eww-search-prefix "http://www.google.com/search?q="
 eww-use-external-browser-for-content-type "\\`\\(video/\\|audio\\)"
 eww-browse-url-new-window-is-tab nil)

(with-eval-after-load 'eww
  (add-hook 'eww-after-render-hook 'eww-readable))

(setq
 shr-use-fonts nil
 shr-use-colors nil
 shr-image-animate nil
 shr-inhibit-images t
 shr-max-image-proportion 0.6)

(setq
 webjump-sites
 '(("Wikipedia" .
    [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""]))
 webjump-use-internal-browser t)

;; %% flyspell
(setq
 ispell-dictionary "english"
 ispell-program-name "aspell"
 ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
(with-eval-after-load 'flyspell
  (keymap-unset flyspell-mode-map "C-,")
  (keymap-unset flyspell-mode-map "C-.")
  (keymap-unset flyspell-mode-map "C-;")
  )

(setq
 dictionary-server "dict.org"
 dictionary-default-popup-strategy "lev"
 dictionary-create-buttons nil
 dictionary-use-single-buffer t)

;; %% savehist
(setq
 history-delete-duplicates t
 savehist-save-minibuffer-history t
 savehist-additional-variables
 '(mark-ring
   global-mark-ring
   search-ring
   regexp-search-ring
   command-history
   extended-command-history))
(add-hook 'after-init-hook 'savehist-mode)

;; %% session
(setq
 desktop-save t
 desktop-restore-eager 1
 desktop-restore-frames nil
 desktop-auto-save-timeout 60
 desktop-load-locked-desktop t)

(with-eval-after-load 'desktop
  (mapc
   (lambda (mode)
     (push mode desktop-modes-not-to-save))
   '(eww-mode
     Info-mode
     magit-mode
     magit-log-mode
     dired-mode
     comint-mode
     doc-view-mode
     elfeed-search-mode))
  )

;; %% tramp speed up
(setq
 tramp-verbose 1
 tramp-chunksize 2000
 tramp-default-method "ssh"
 remote-file-name-inhibit-locks t
 remote-file-name-inhibit-cache nil)

(setq
 vc-handled-backends '(Git)
 vc-git-diff-switches '("--histogram")
 vc-ignore-dir-regexp
 (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))

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
   (holiday-chinese 5 5   "端午节")
   (holiday-chinese 1 15  "元宵节")
   (holiday-chinese 7 7   "七夕节")
   (holiday-chinese 9 9   "重阳节")
   (holiday-chinese 8 15  "中秋节")
   (holiday-fixed 1 1     "元旦")
   (holiday-fixed 10 1    "国庆节")
   (holiday-fixed 3 8     "妇女节")
   (holiday-fixed 5 1     "劳动节")
   (holiday-fixed 5 4     "青年节")
   (holiday-fixed 6 1     "儿童节")
   (holiday-fixed 9 10    "教师节")
   (holiday-fixed 2 14    "情人节")
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
  (superword-mode          1)
  (visual-line-mode        1)
  (goto-address-mode       1)
  (variable-pitch-mode     1)
  (toggle-truncate-lines  -1))

(defun yx/global-mirror-mode-toggle ()
  (repeat-mode            1)
  (save-place-mode        1)
  (desktop-save-mode     -1)
  (blink-cursor-mode     -1)
  (auto-compression-mode  1)
  (delete-selection-mode  1)
  (auto-save-visited-mode 1)
  (mouse-avoidance-mode 'jump)
  (unless (display-graphic-p)
    (xterm-mouse-mode 1))
  (pixel-scroll-precision-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (auth-source-pass-enable)
  (windmove-default-keybindings 'control)
  )

(add-hook 'text-mode 'yx/text-mode-setup)
(add-hook 'after-init-hook 'yx/global-mirror-mode-toggle)

(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

;;; Keymaps
;; %% simple
(defalias 'qrr 'query-replace-regexp)

;; %% 全局按键
;; This avoids the ugly accidentally action of scaling text with using the trackpad
(keymap-global-unset "C-<wheel-up>")
(keymap-global-unset "C-<wheel-down>")

(use-package define-repeat-map
  :vc (:url "https://tildegit.org/acdw/define-repeat-map.el" :rev :newest)
  :commands (define-repeat-map))

(define-repeat-map puni-expand-region
  ("+" puni-expand-region
   "-" puni-contract-region)
  (:enter
   mark-word
   mark-sexp
   mark-defun
   puni-mark-sexp-at-point
   puni-mark-sexp-around-point
   puni-mark-list-around-point
   ))

(define-repeat-map org-heading-navigate
  ("u" outline-up-heading
   "p" org-previous-visible-heading
   "n" org-next-visible-heading
   "f" org-forward-heading-same-level
   "b" org-backward-heading-same-level)
  )

(use-package key-chord
  :init
  (key-chord-mode 1)
  (key-chord-define-global "zz"     'zoom)
  (key-chord-define-global "jj"     'avy-goto-char-timer)
  (key-chord-define-global "jk"     'avy-goto-word-1)
  (key-chord-define-global "jl"     'avy-goto-line)
  (with-eval-after-load 'org
    (key-chord-define
     org-mode-map
     "jh" 'avy-org-goto-heading-timer))
  )

(bind-keys
 ([remap move-beginning-of-line]        . crux-move-beginning-of-line) ; C-a
 ([remap goto-line]                     . consult-goto-line) ;M-g g
 ([remap switch-to-buffer]              . consult-buffer) ; C-x b
 ([remap list-buffers]                  . ibuffer) ; C-x C-b
 ([remap repeat-complex-command]        . consult-complex-command) ; C-x M-:
 ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
 ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
 ([remap project-switch-to-buffer]      . consult-project-buffer) ; C-x p b
 ([remap yank-pop]                      . consult-yank-pop) ;M-y
 ([remap bookmark-jump]                 . consult-bookmark) ;C-x r b
 ([remap imenu]                         . consult-imenu) ;M-g i
 ([remap describe-function]             . helpful-callable) ; C-h f
 ([remap describe-key]                  . helpful-key) ; C-h k
 ([remap describe-command]              . helpful-command) ; C-h x
 ([remap describe-variable]             . helpful-variable) ; C-h v
 ([remap list-directory]                . zoxide-travel) ; C-x C-d
 ([remap dired-at-point]                . consult-dir) ; C-x d
 ([remap dabbrev-expand]                . hippie-expand) ; M-/
 ([remap comment-dwim]                  . yx/comment-dwim) ; M-;
 ([remap keyboard-quit]                 . yx/keyboard-quit-dwim) ; C-g
 ([remap kill-buffer]                   . yx/kill-buffer-dwim) ; C-x k
 ([remap save-buffers-kill-emacs]       . delete-frame) ; s-q
 ([remap open-line]                     . crux-smart-open-line) ; C-o
 ([remap fill-paragraph]                . yx/fill-unfill) ; M-q
 )

(bind-keys
 ("C-<f5>"    . dape)
 ("<f5>"      . quickrun)
 ("<f10>"     . yx/transient-global-odd)
 ("s-<return>" . toggle-frame-maximized)
 ("s-/"       . transform-previous-char)
 ("s-r"       . consult-recent-file)
 ("s-t"       . tab-bar-new-tab)
 ("s-j"       . avy-goto-char-timer)
 ("s-d"       . dirvish-side)
 ("s-o"       . ace-window)
 ("s-w"       . tabspaces-close-workspace)
 ("s-<right>" . ns-next-frame)
 ("s-<left>"  . ns-prev-frame)
 ("s-]"       . tab-next)
 ("s-["       . tab-previous)
 ("s-s"       . yx/eshell-here)
 ("C-;"       . iedit-mode)
 ("C-."       . embark-act)
 ("C-,"       . embark-dwim)
 ("C-/"       . undo-only)
 ("C-^"       . crux-top-join-line)
 ("C-M-/"     . vundo)
 ("C-#"       . consult-register-load)
 ("M-#"       . consult-register-store)
 ("C-c #"     . consult-register)
 ("M-o"       . duplicate-line)
 ("M-z"       . yx/quick-zap-up-to-char)
 ("M-0"       . delete-window)
 ;; M-' surround-keymap
 ("M-g ;"     . goto-last-change)
 ("M-g a"     . consult-org-agenda)
 ("M-g M"     . consult-man)
 ("M-g I"     . consult-info)
 ("M-g M-i"   . consult-imenu-multi)
 ("M-g M-f"   . consult-flymake)
 ("M-g M-e"   . consult-compile-error)
 ("M-g o"     . consult-outline)
 ("M-g k"     . consult-kmacro)
 ("M-g m"     . consult-mark)
 ("M-g M-m"   . consult-global-mark)
 ("M-s t"     . yx/hl-todo-rg-project)
 ("M-s M-t"   . hl-todo-occur)
 ("M-s f"     . consult-fd)
 ("M-s M-h"   . symbol-overlay-put)
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
 ("C-c ;"     . flyspell-correct-next)
 ("C-c k"     . kill-buffer-and-window)
 ("C-c K"     . crux-kill-other-buffers)
 ("C-c v"     . magit-file-dispatch)
 ("C-c C-v"   . magit-dispatch)
 ("C-c C-d"   . helpful-at-point)
 ("C-c a"     . org-agenda)
 ("C-c c"     . org-capture)
 ("C-c l"     . org-store-link)
 ("C-c b"     . tabspaces-switch-to-buffer)
 ("C-c d"     . devdocs-lookup)
 ("C-c e"     . embark-export)
 ("C-c r"     . query-replace-regexp)
 ("C-c z"     . hs-toggle-hiding)
 ("C-c Z"     . hs-show-all)
 ("C-c f"     . dirvish-fd)
 ("C-x a a"   . align)
 ("C-x a r"   . align-regexp)
 ("C-x / o"   . browse-url-at-point)
 ("C-x / a"   . ace-link-addr)
 ("C-x / l"   . ace-link)
 ("C-c w o"   . burly-open-bookmark)
 ("C-c w r"   . burly-reset-tab)
 ("C-c w w"   . burly-bookmark-windows)
 ("C-c w f"   . burly-bookmark-frames)
 ("C-c n c"   . denote)
 ("C-c n t"   . denote-template)
 ("C-c n C-c" . citar-create-note)
 ("C-c n C-o" . citar-denote-dwim)
 ("C-c n n"   . denote-open-or-create)
 ("C-c n i"   . denote-link-or-create)
 ("C-c n l"   . denote-backlinks)
 ("C-c n f"   . denote-find-link)
 ("C-c n C-f" . denote-org-dblock-insert-links)
 ("C-c n b"   . denote-find-backlink)
 ("C-c n C-b" . denote-org-dblock-insert-backlinks)
 ("C-c n t"   . org-transclusion-add)
 ("C-c n C-t" . org-transclusion-add-all)
 ("C-h b"     . embark-bindings)
 ("C-h C-m"   . which-key-show-full-major-mode)
 ("C-h B"     . embark-bindings-at-point))

;; %% transient key
(with-eval-after-load 'transient
  (transient-bind-q-to-quit)
  (keymap-set transient-map "<escape>" 'transient-quit-all))

(transient-define-prefix yx/transient-global-odd ()
  "Global transient for frequently used functions."
  [["]]a"
    ("a" "agenda" org-agenda-list)
    ("c" "whitespace-cleanup" whitespace-cleanup)
    ("o" "crux-open-with" crux-open-with)
    ("s" "scratch-buffer" yx/scratch-buffer)
    ("n" "new-empty-buffer" yx/new-empty-buffer)
    ("m" "major-mode-keybings" which-key-show-full-major-mode)
    ("v" "magit-file-dispatch" magit-file-dispatch)
    ("w" "pyim-create-word-from-selection" pyim-create-word-from-selection)
    ("%" "query-replace-regexp" query-replace-regexp)
    ("!" "shell-command" shell-command)]
   ["]]A"
    ("C" "desktop-clear" desktop-clear)
    ("D" "crux-delete-file-and-buffer" crux-delete-file-and-buffer)
    ("G" "magit-status" magit-status)
    ("V" "magit-dispatch" magit-dispatch)
    ("I" "Clock In" yx/org-clock-in)
    ("T" "consult-minor-mode-menu" consult-minor-mode-menu)
    ("R" "rename-visited-file" rename-visited-file)
    ("K" "crux-kill-other-buffers" crux-kill-other-buffers)
    ("E" "crux-sudo-edit" crux-sudo-edit)]
   ["]]T"
    ("t d" "toggle-window-dedicated" yx/toggle-window-dedicated)
    ("t l" "command-log" clm/toggle-command-log-buffer)]
   ]
  )

;;; Ui
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

;;; Layout
(setq
 window-min-height 3
 window-min-width 30
 ns-pop-up-frames nil
 window-sides-vertical nil
 split-height-threshold 80
 split-width-threshold 125
 even-window-sizes 'height-only
 frame-resize-pixelwise t
 window-resize-pixelwise t
 window-combination-resize t
 fit-frame-to-buffer nil
 fit-window-to-buffer-horizontally nil)


(setq
 display-buffer-alist
 '(("\\`\\*[hH]elp"
    (display-buffer-reuse-mode-window
     display-buffer-in-direction)
    (window . root)
    (window-height . 0.4)
    (direction . bottom)
    (mode . (help-mode helpful-mode)))
   ("\\`\\*Async Shell Command\\*\\'"
    (display-buffer-no-window)
    (allow-no-window . t))
   ("\\`\\(\\*Org [^S]\\|CAPTURE\\)"
    (display-buffer-reuse-mode-window
     display-buffer-below-selected))
   ("\\`\\*Embark"
    (display-buffer-reuse-mode-window
     display-buffer-below-selected)
    (window-height . 0.4)
    (window-parameters . ((no-other-window . t)
                          (mode-line-format . none))))
   ("\\`\\(\\*Calendar\\|\\*Bookmark\\)"
    (display-buffer-reuse-mode-window
     display-buffer-below-selected)
    (dedicated . t)
    (window-height . fit-window-to-buffer))
   ("\\`\\*devdocs\\*\\'"
    (display-buffer-reuse-mode-window
     display-buffer-in-side-window)
    (side . right)
    (window-width . 0.4))
   ("\\`\\(\\*Ibuffer\\|\\*Man\\|\\*WoMan\\|\\*info\\|magit\\)"
    (display-buffer-full-frame))
   ))

(add-to-list
 'display-buffer-alist
 `(,(yx/prefixs-to-regex
     "*R"
     "*julia"
     "*lua"
     "*Lua"
     "*Python"
     "*eshell"
     "*Eshell"
     "*term"
     "*Occur"
     "*Backtrac"
     "*Flymake"
     "*vc-git"
     "*Error"
     "*Warnings"
     "*Messages"
     "*quickrun"
     "*Dictionary"
     "*fanyi"
     "*stardict*"
     "*color-rg")
   (display-buffer-reuse-mode-window
    display-buffer-in-side-window)
   (side . bottom)
   (window-height . 0.4)))

(defun yx/toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer))
  (force-window-update))

;; %% @see http://yummymelon.com/devnull/using-bookmarks-in-emacs-like-you-do-in-web-browsers.html
(easy-menu-define yx/bookmarks-menu nil
  "Keymap for CC Bookmarks Menu"
  '("Bookmarks"
    ["Edit Bookmarks" list-bookmarks
     :help "Display a list of existing bookmarks."]
    ["--" nil]
    ["Add Bookmark…" bookmark-set
     :help "Set a bookmark named NAME at the current location."]
    ["---" nil]
    ["Jump to Bookmark…" bookmark-jump
     :help "Jump to bookmark"]))

(easy-menu-add-item global-map '(menu-bar)
                    yx/bookmarks-menu
                    "Tools")

(define-key global-map [menu-bar edit bookmark] nil)

;; %% windows manager
(setq
 winner-dont-bind-my-keys t
 winner-boring-buffers-regexp "^\\*")
(add-hook 'after-init-hook 'winner-mode)
(add-hook 'after-init-hook 'temp-buffer-resize-mode)

(setq
 help-window-select t
 help-window-keep-selected t)

(setq
 switch-to-buffer-in-dedicated-window nil
 switch-to-buffer-obey-display-actions t
 switch-to-buffer-preserve-window-point t
 switch-to-prev-buffer-skip 'visible
 switch-to-prev-buffer-skip-regexp "^\\*\\|^magit.*")

(use-package burly
  :hook (after-init . burly-tabs-mode))

;; %% tabbar
(setq
 tab-bar-show t
 tab-bar-format
 '(tab-bar-format-menu-bar
   tab-bar-format-tabs
   tab-bar-separator
   tab-bar-format-add-tab)
 tab-bar-tab-hints t
 tab-bar-new-tab-to 'right
 tab-bar-new-button-show t
 tab-bar-close-button-show nil
 tab-bar-new-tab-choice "*scratch*"
 tab-bar-tab-name-truncated-max 20
 tab-bar-select-tab-modifiers '(super))

(defun yx/tab-bar-setup()
  (tab-bar-mode 1)
  (tab-bar-history-mode 1)
  (let ((map tab-bar-map))
    (keymap-unset map "<wheel-up>")
    (keymap-unset map "<wheel-down>")
    (keymap-unset map "<wheel-left>")
    (keymap-unset map "<wheel-right>")))
(add-hook 'after-init-hook #'yx/tab-bar-setup)

(use-package sr-speedbar
  :ensure nil
  :defer 2
  :init
  (setq
   speedbar-use-images nil
   sr-speedbar-width 30
   sr-speedbar-skip-other-window-p t))

(use-package ace-window
  :init
  (setq
   aw-scope 'frame
   aw-background nil
   aw-dispatch-always t
   aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; %% tabspaces
(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :custom
  (tabspaces-use-filtered-buffers-as-default nil)
  (tabspaces-default-tab "Main")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-session t)
  (tabspaces-session-file (no-littering-expand-var-file-name "tabsession.el"))
  (tabspaces-session-auto-restore t)
  )

;; %% buffer manager
(setq buffer-quit-function 'winner-undo)

(use-package zoom
  :custom
  (zoom-size '(0.618 . 0.618)))

(setq
 ibuffer-expert t
 ibuffer-display-summary nil
 ;; ibuffer-never-show-predicates '("^\\*")
 ibuffer-show-empty-filter-groups nil)

(defun yx/ibuffer-setup ()
  (hl-line-mode 1)
  (ibuffer-auto-mode 1)
  (ibuffer-do-sort-by-recency))
(add-hook 'ibuffer-mode-hook 'yx/ibuffer-setup)

(use-package ibuffer-vc
  :init
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))

(use-package popper
  :defer 2
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :config
  (setq
   popper-display-control 'user
   popper-group-function #'popper-group-by-directory)
  (setq popper-reference-buffers
        '("\\*julia\\*$"
          "\\*color-rg\\*$"
          "\\*Python\\*$"
          (compilation-mode . hide)
          help-mode
          helpful-mode
          occur-mode
          color-rg-mode
          ))
  (popper-mode 1)
  (popper-echo-mode 1)
  )

;;; Completion
(use-package vertico
  :init
  (setq vertico-resize nil
        vertico-preselect 'directory)
  (vertico-mode 1)
  (vertico-mouse-mode 1)
  (vertico-indexed-mode 1)
  :bind (:map vertico-map
              ("M-q" . vertico-quick-insert)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  )

(use-package orderless
  :custom
  (completion-styles '(basic substring initials flex orderless))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles . (basic partial-completion orderless)))
     (bookmark (styles . (basic substring)))
     (library (styles . (basic substring)))
     (embark-keybinding (styles . (basic substring)))
     (imenu (styles . (basic substring orderless)))
     (consult-location (styles . (basic substring orderless)))
     (kill-ring (styles . (emacs22 orderless)))
     (eglot (styles . (emacs22 substring orderless)))))
  )

;; %% embark
(use-package embark
  :init
  (setq prefix-help-command 'embark-prefix-help-command)
  :custom
  (embark-confirm-act-all nil)
  (embark-selection-indicator nil)
  (embark-prompter 'embark-completing-read-prompter)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :bind
  ( :map embark-general-map
    (", b" . engine/search-bing)
    (", z" . engine/search-zhihu)
    :map
    embark-file-map
    (", s" . crux-sudo-edit)
    :map
    embark-identifier-map
    (", h" . symbol-overlay-put)
    :map
    minibuffer-local-map
    ("C-SPC" . (lambda () (interactive) (embark-select) (vertico-next)))
    ))

(use-package embark-consult
  :after embark
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; %% consult
(use-package consult
  :config
  (setq
   consult-narrow-key "?"
   xref-show-xrefs-function #'consult-xref
   xref-show-definitions-function #'consult-xref
   consult-ripgrep-args (concat consult-ripgrep-args " --hidden"))
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-bookmark consult-recent-file :preview-key "M-.")
  :bind (:map minibuffer-local-map
              ("M-s" . consult-history)
              ("M-r" . consult-history))
  )

(use-package consult-dir
  :after consult
  :bind (:map vertico-map
              ("C-x C-d" . consult-dir)
              ("C-x C-j" . consult-dir-jump-file))
  )

;; %% corfu
(use-package corfu
  :hook ((text-mode prog-mode) . corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match t)
  (corfu-echo-documentation nil)
  :config
  (corfu-echo-mode 1)
  (corfu-history-mode 1)
  (corfu-indexed-mode 1)
  (corfu-popupinfo-mode 1)
  :bind
  (:map corfu-map
        ("M-q"     . corfu-quick-insert)
        ("M-SPC"   . corfu-insert-separator))
  )

(use-package cape
  :init
  (setq cape-dabbrev-min-length 3)
  (dolist (ele '(cape-abbrev cape-file))
    (add-to-list 'completion-at-point-functions ele))
  )

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  )

;;; Misc
(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB

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

(use-package engine-mode
  :hook (after-init . engine-mode)
  :custom
  (engine/browser-function 'browse-url-generic)
  :config
  (yx/define-enines
   '(("c" "https://github.com/search?q=%s")
     ("g" "https://www.google.com/search?q=%s")
     ("b" "https://cn.bing.com/search?q=%s&ensearch=1")
     ("w" "https://zh.wikipedia.org/w/index.php?search=%s")
     ("a" "https://www.wolframalpha.com/input/?i=%s")
     ("z" "https://www.zhihu.com/search?q=%s")
     ("d" "https://search.douban.com/book/subject_search?search_text=%s")))
  :preface
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
  )

(use-package posframe)

;; %% auxiliary tool
(use-package crux
  :ensure nil
  :defer 2
  :autoload (yx/def-org-maybe-surround)
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-reopen-as-root-mode 1)
  )

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.2)
  :config
  (which-key-setup-minibuffer)
  )

(use-package goggles
  :hook ((text-mode prog-mode) . goggles-mode)
  :custom
  (goggles-pulse t)
  )

(use-package ace-link
  :hook (after-init . ace-link-setup-default))

(use-package helpful
  :bind
  (:map helpful-mode-map
        ("q" . delete-window)
        ("b" . yx/helpful-next-buffer)
        ("f" . yx/helpful-prev-buffer))
  :preface
  (defun yx/helpful-next-buffer ()
    (interactive)
    (cl-letf ((bufname (buffer-name))
              (switch-to-prev-buffer-skip-regexp nil))
      (next-buffer)
      (unless (string= (buffer-name) bufname)
        (while (not (eq major-mode 'helpful-mode))
          (next-buffer)))))
  (defun yx/helpful-prev-buffer ()
    (interactive)
    (cl-letf ((bufname (buffer-name))
              (switch-to-prev-buffer-skip-regexp nil))
      (previous-buffer)
      (unless (string= (buffer-name) bufname)
        (while (not (eq major-mode 'helpful-mode))
          (previous-buffer))))
    ))

(use-package command-log-mode
  :custom
  (command-log-mode-key-binding-open-log nil))

;; %% edit enhencement
(use-package vundo)
(use-package goto-chg)

(use-package avy
  :init
  (setq avy-style 'at
        avy-timeout-seconds 0.8)
  :bind (:map isearch-mode-map
              ("C-'" . avy-isearch))
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
  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)
  )

(use-package hungry-delete
  :hook (after-init . global-hungry-delete-mode)
  :custom
  (hungry-delete-join-reluctantly t)
  )

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark))
  )

(use-package drag-stuff
  :hook ((text-mode
          prog-mode) . drag-stuff-mode)
  :bind (:map drag-stuff-mode-map
              ("M-J" . drag-stuff-down)
              ("M-K" . drag-stuff-up)
              ("M-H" . drag-stuff-left)
              ("M-L" . drag-stuff-right))
  )

;; %% spell
(use-package jinx
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (setq jinx-languages "en_US")
  )

(use-package flyspell-correct
  :after flyspell
  :bind
  (:map flyspell-mode-map
        ("M-$" . flyspell-correct-wrapper))
  )

;; %% chinese
(use-package pyim
  :bind
  (("s-," . pyim-convert-string-at-point)
   :map pyim-mode-map
   ("," . pyim-next-page)
   ("." . pyim-previous-page))
  :commands
  (pyim-create-word-from-selection)
  :init
  (setq
   default-input-method "pyim"
   pyim-outcome-trigger  nil
   pyim-enable-shortcode nil
   pyim-punctuation-dict nil
   pyim-page-tooltip 'posframe
   pyim-dcache-backend 'pyim-dregcach
   pyim-indicator-list '(pyim-indicator-with-modeline)
   pyim-dicts '((:name "big-dict" :file "~/.emacs.d/.cache/pyim-bigdict.pyim"))
   pyim-english-input-switch-functions
   '(pyim-probe-auto-english
     pyim-probe-program-mode
     pyim-probe-isearch-mode
     pyim-probe-org-latex-mode
     pyim-probe-org-structure-template))
  :config
  (require 'pyim-dregcache)
  (pyim-default-scheme 'xiaohe-shuangpin)
  (use-package pyim-tsinghua-dict
    :vc (:url "https://github.com/redguardtoo/pyim-tsinghua-dict" :rev :newest)
    :demand t)
  (pyim-tsinghua-dict-enable)
  )

(use-package stardict
  :ensure nil
  :bind (("M-s d" . stardict-define-at-point)
         ("M-s M-d" . stardict-define))
  :init
  (setq stardict-name "langdao-ec-gb"
        stardict-dir "~/.config/stardict/dic/stardict-langdao-ec-gb-2.4.2")
  )

(use-package bing-dict
  :init
  (setq bing-dict-add-to-kill-ring t
        bing-dict-show-thesaurus 'antonym
        bing-dict-cache-auto-save nil
        bing-dict-vocabulary-save t
        bing-dict-vocabulary-file (no-littering-expand-var-file-name "bing-dict-vocabulary.org"))
  (add-hook 'eww-mode-hook 'bing-dict-eldoc-mode)
  (add-hook 'Info-mode-hook 'bing-dict-eldoc-mode)
  (add-hook 'elfeed-show-hook 'bing-dict-eldoc-mode)
  :preface
  (defun bing-dict-eldoc-documentation-function ()
    (let ((word (word-at-point)))
      (when (and word (> (length word) 5))
        (bing-dict-brief word))
      nil))
  (define-minor-mode bing-dict-eldoc-mode
    "Use bing-dict as backend of eldoc."
    :lighter " Bing Dict"
    (if bing-dict-eldoc-mode
        (progn (setq-local eldoc-documentation-function
                           #'bing-dict-eldoc-documentation-function)
               (eldoc-mode +1))
      (setq-local eldoc-documentation-function #'ignore)
      (eldoc-mode -1)))
  )

;; %% Tools
(use-package tldr)

(use-package dwim-shell-command
  :bind
  (([remap shell-command] . dwim-shell-command)
   :map dired-mode-map
   ([remap dired-do-async-shell-command] . dwim-shell-command)
   ([remap dired-do-shell-command] . dwim-shell-command)
   ([remap dired-smart-shell-command] . dwim-shell-command)))

(use-package outli
  :vc (:url "https://github.com/jdtsmith/outli" :rev :newest)
  :hook ((prog-mode text-mode) . outli-mode)
  :bind (:map outli-mode-map
              ("C-c C-u" . (lambda () (interactive) (outline-back-to-heading)))))

;;; Dired
(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-mouse-drag-files t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask)
  (dired-auto-revert-buffer 'dired-buffer-stale-p)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-alGgh")
  (wdired-create-parent-directories t)
  (wdired-allow-to-change-permissions t)
  (dired-guess-shell-alist-user
   `(("\\.\\(?:docx\\|pdf\\|djvu\\gif)\\'" ,yx/default-open-program)))
  :config
  (add-hook 'dired-mode-hook 'yx/dired-setup)
  (add-hook 'wdired-mode-hook 'highlight-changes-mode)
  (put 'dired-find-alternate-file 'disabled nil)
  :preface
  (defun yx/dired-setup ()
    (setq
     dired-omit-files
     (concat dired-omit-files "\\|^\\..*$"))
    (hl-line-mode 1)
    (dired-omit-mode 1)
    (dired-hide-details-mode 1))
  )

;; %% dired+
(use-package diredfl
  :hook
  (dired-mode . diredfl-mode)
  (dirvish-directory-view-mode . diredfl-mode)
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t)
  )

(use-package zoxide)

(use-package dirvish
  :hook (after-init . dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"           "Home")
     ("d" "~/yxdocs/"    "yxdocs")
     ("D" "~/Downloads/" "Downloads")
     ("w" "~/workspace/" "workspace>")))
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
  :bind
  (:map dirvish-mode-map
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
        ("M-l" . dirvish-ls-switches-menu)
        ("M-m" . dirvish-mark-menu)
        ("M-t" . dirvish-layout-toggle)
        ("M-s" . dirvish-setup-menu)
        ("M-e" . dirvish-emerge-menu)
        ("M-j" . dirvish-fd-jump))
  )

;;; Gnus
(setq
 gnus-home-directory "~/.gnus.d/"
 gnus-default-directory gnus-home-directory
 mm-default-directory gnus-home-directory
 message-directory (expand-file-name "Mail/" gnus-home-directory))

(setq
 read-mail-command 'gnus
 message-confirm-send t
 message-from-style 'angles
 message-kill-buffer-on-exit t
 mail-user-agent 'gnus-user-agent
 mail-envelope-from 'header
 mail-specify-envelope-from t
 sendmail-program "/usr/local/bin/msmtp"
 send-mail-function 'message-send-mail-with-sendmail
 message-send-mail-function 'message-send-mail-with-sendmail
 )

(setq mm-inline-large-images t)

(setq
 mml-default-sign-method "pgpmime"
 mml-secure-openpgp-sign-with-sender t)

(use-package gnus
  :commands (gnus)
  :ensure nil
  :config
  (setq
   mail-sources
   '((imap
      :server "imap.qq.com"
      :port 993
      :user "yangxue.cs@foxmail.com"
      :mailbox "INBOX"
      :fetchflag "\\Seen"
      :stream tls
      :dontexpunge t)
     ))
  (setq
   gnus-select-method '(nnnil "")
   gnus-secondary-select-methods
   '((nnml "foxmail.cs")
     (nnimap
      "foxmail.wk"
      (nnimap-address "imap.qq.com")
      (nnimap-server-port 993)
      (nnimap-user "yangxue.wk@foxmail.com")
      (nnimap-stream ssl)
      (nnimap-expunge 'nerver)
      (nnimap-search-engine imap)
      (nnimap-inbox "INBOX")
      (nnimap-split-methods default))
     (nnimap
      "outlook.cs"
      (nnimap-user "yangxue.cs@outlook.com")
      (nnimap-stream ssl)
      (nnimap-server-port 993)
      (nnimap-address "outlook.office365.com")
      (nnimap-expunge 'nerver)
      (nnimap-search-engine imap)
      (nnimap-inbox "INBOX")
      (nnimap-split-methods default))
     ))
  (setq
   nnmail-expiry-wait '30
   nnmail-resplit-incoming t
   nnmail-split-fancy-match-partial-words t
   nnmail-split-methods 'nnmail-split-fancy
   nnmail-split-fancy
   '(|
     (: nnmail-split-fancy-with-parent)
     (to  "yangxue.cs@foxmail.com" "INBOX.foxmail.cs")
     (to  "yangxue.wk@foxmail.com" "INBOX.foxmail.wk")
     (to  "yangxue.cs@outlook.com" "INBOX.outlook.cs")
     (any "emacs-devel@gnu.org"    "INBOX.emacs-devel")
     (any "emacs-orgmode@gnu.org"  "INBOX.emacs-orgmode")
     (any "help-gnu-emacs@gnu.org" "INBOX.help-gnu-emacs")
     "INBOX.Misc")
   )

  (setq
   gnus-asynchronous t
   gnus-use-header-prefetch t
   gnus-use-cache t
   gnus-use-scoring t
   gnus-suppress-duplicates t
   gnus-novice-user nil
   gnus-expert-user t
   gnus-interactive-exit 'quiet
   gnus-inhibit-startup-message t)
  (setq
   gnus-save-newsrc-file nil
   gnus-read-newsrc-file nil
   gnus-save-killed-list nil
   gnus-read-active-file nil
   gnus-use-dribble-file t
   gnus-always-read-dribble-file t
   gnus-message-archive-group nil
   gnus-inhibit-user-auto-expire t
   gnus-search-use-parsed-queries t
   gnus-article-browse-delete-temp t
   gnus-check-new-newsgroups 'ask-server
   gnus-mime-display-multipart-related-as-mixed t
   gnus-subscribe-newsgroup-method 'gnus-subscribe-zombies)
  (setq
   gnus-cache-remove-articles '(read)
   gnus-cacheable-groups "^\\(nntp\\|nnimap\\)"
   gnus-cache-enter-articles '(ticked dormant unread))

  (setq
   nnrss-ignore-article-fields '(description guid pubData dc:creator link))

  (gnus-demon-add-handler 'gnus-demon-scan-mail nil 10)
  )

(use-package gnus-group
  :ensure nil
  :hook (gnus-select-group . gnus-group-set-timestamp)
  :config
  (setq
   gnus-sum-thread-tree-root            "◉ "
   gnus-sum-thread-tree-false-root      "◎ "
   gnus-sum-thread-tree-single-indent   "◌ "
   gnus-sum-thread-tree-vertical        "| "
   gnus-sum-thread-tree-indent          "  "
   gnus-sum-thread-tree-leaf-with-other "+-> "
   gnus-sum-thread-tree-single-leaf     "`-> "
   gnus-summary-line-format "%U%R%z%B%[%4L: %-10,10f%] %s\n")
  (setq
   gnus-summary-make-false-root 'adopt
   gnus-summary-ignore-duplicates t
   gnus-summary-gather-subject-limit 'fuzzy
   gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
   gnus-simplify-subject-functions '(gnus-simplify-subject-re gnus-simplify-whitespace))
  (setq
   gnus-use-trees t
   gnus-show-threads t
   gnus-fetch-old-headers t
   gnus-tree-minimize-window t
   gnus-generate-tree-function
   'gnus-generate-horizontal-tree
   gnus-build-sparse-threads 'some
   gnus-fetch-old-ephemeral-headers 2
   gnus-thread-indent-level 2
   gnus-thread-hide-subtree nil
   gnus-thread-sort-functions
   '(gnus-thread-sort-by-subject
     gnus-thread-sort-by-most-recent-number))
  (setq
   gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
   gnus-subthread-sort-functions '(gnus-thread-sort-by-date))
  (setq
   gnus-view-pseudos 'automatic
   gnus-view-pseudos-separately t
   gnus-view-pseudo-asynchronously t)
  (setq
   gnus-auto-select-first t
   gnus-auto-select-next nil
   gnus-paging-select-next nil)

  (setq
   gnus-group-sort-function '(gnus-group-sort-by-method)
   gnus-group-line-format "%M%S%p%P %0{%5y%} %B%{%G%}\n")

  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  )

;;; Shell
(setq
 comint-input-ignoredups t
 comint-prompt-read-only t
 comint-completion-autolist t
 comint-completion-addsuffix t
 comint-buffer-maximum-size 9999
 comint-scroll-to-bottom-on-input t
 comint-scroll-show-maximum-output t
 comint-scroll-to-bottom-on-output nil)

(setq
 shell-kill-buffer-on-exit t
 shell-highlight-undef-enable t
 shell-command-prompt-show-cwd t)

(use-package eshell
  :init
  (setq
   eshell-kill-on-exit t
   eshell-save-history-on-exit t
   eshell-kill-processes-on-exit 'ask
   eshell-destroy-buffer-when-process-dies nil
   eshell-hist-ignoredups t
   eshell-error-if-no-glob t
   eshell-prefer-lisp-functions t
   eshell-rm-removes-directories t
   eshell-scroll-to-bottom-on-input 'all
   eshell-scroll-to-bottom-on-output 'all
   eshell-prompt-function 'yx/eshell-prompt)
  :config
  (dolist (m '(eshell-tramp
               eshell-rebind
               eshell-elecslash))
    (add-to-list 'eshell-modules-list m))
  (add-hook 'eshell-mode-hook 'yx/eshell-setup)

  :preface
  (defun yx/eshell-setup ()
    (keymap-set eshell-mode-map "C-l" 'yx/eshell-clear)
    (keymap-set eshell-mode-map "C-r" 'consult-history)
    (setq
     eshell-visual-commands
     '("vim" "ssh" "tail" "top" "htop" "tmux" "less" "more")
     eshell-visual-subcommands '(("git" "log" "diff" "show")))
    (eshell/alias "q"    "exit")
    (eshell/alias "r"    "consult-recent-file")
    (eshell/alias "d"    "dired $1")
    (eshell/alias "f"    "find-file $1")
    (eshell/alias "gs"   "magit-status")
    (eshell/alias "gd"   "magit-diff-unstaged")
    (eshell/alias "gds"  "magit-diff-staged")
    (eshell/alias "gv"   "magit-dispatch")
    (eshell/alias "ll"   "ls -AlohG --color=always")
    (setq-local completion-at-point-functions
                '(cape-file
                  pcomplete-completions-at-point
                  cape-elisp-symbol
                  t))
    )

  (defun yx/eshell-prompt()
    (setq eshell-prompt-regexp "^[^#$\n]*[#$] ")
    (concat (yx/pwd-replace-home (eshell/pwd))
            (if (fboundp 'magit-get-current-branch)
                (if-let ((branch (magit-get-current-branch)))
                    (format " [git:%s]" branch)
                  "")
              "")
            (if (= (user-uid) 0) "\n# " "\n$ ")))

  (defun yx/eshell-clear ()
    "Clear the current Eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (defun yx/eshell-here ()
    (interactive)
    (let* ((project (project-current))
           (name (if project (project-name project) "#"))
           (dir (if (buffer-file-name)
                    (file-name-directory (buffer-file-name))
                  default-directory))
           (old-name eshell-buffer-name)
           (height (/ (frame-height) 3)))
      (setq eshell-buffer-name (concat "*Eshell-" name "*"))
      (eshell)
      (setq eshell-buffer-name old-name)
      (eshell/clear)
      (eshell/cd dir)))

  (defun eshell/z ()
    (let ((dir (completing-read "Directory: " (ring-elements eshell-last-dir-ring) nil t)))
      (eshell/cd dir)))

  (defun eshell/F (filename)
    "Open a file as root from Eshell"
    (let ((qual-filename (if (string-match "^/" filename)
                             filename
                           (concat (expand-file-name (eshell/pwd)) "/" filename))))
      (switch-to-buffer
       (find-file-noselect
        (concat "/sudo::" qual-filename))))))

;; %% eat
(use-package eat
  :vc (:url "https://codeberg.org/akib/emacs-eat" :rev :newest)
  :init
  (setq
   eat-kill-buffer-on-exit t
   eat-enable-yank-to-terminal t)
  :hook
  ((eshell-load . eat-eshell-mode)
   (eshell-load . eat-eshell-visual-command-mode))
  )

(use-package pcmpl-args
  :after eshell
  :demand t)

(use-package eshell-syntax-highlighting
  :after eshell-mode
  :demand t
  :config
  (eshell-syntax-highlighting-global-mode +1))

;;; Org
(use-package org
  :ensure nil
  :defer 2
  :bind
  (:map org-mode-map
        ("M-<f10>" . yx/transient-org)
        ("RET"     . yx/org-return-dwim)
        ("M-g h"   . consult-org-heading)
        ("C-c M-y" . yx/org-link-copy)
        ("C-c M-i" . org-web-tools-insert-link-for-url)
        ("C-c t h" . org-toggle-heading)
        ("C-c t l" . org-toggle-link-display)
        ("C-c t v" . yx/org-display-subtree-inline-images)
        ("C-x n h" . yx/org-show-current-heading-tidily))
  :autoload (org-calendar-holiday)
  :hook
  (org-mode . yx/org-mode-setup)
  (org-agenda-finalize . yx/org-agenda-finalize-setup)
  (org-babel-after-execute . yx/org-babel-display-image)
  :custom
  (org-directory yx/org-dir)
  (org-ellipsis nil)
  (org-num-max-level 2)
  (org-reverse-note-order t)
  (org-list-allow-alphabetical t)
  (org-return-follows-link nil)
  (org-mouse-1-follows-link nil)
  (org-crypt-key yx/gpg-encrypt-key)
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
  (org-image-actual-width '(300))
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (org-element-use-cache nil)
  (org-element-cache-persistent nil)

  (org-blank-before-new-entry '((heading . nil)
                                (plain-list-item . nil)))

  (org-startup-indented nil)
  (org-adapt-indentation nil)

  (org-startup-folded t)
  (org-hide-block-startup t)
  (org-hide-drawer-startup t)
  (org-startup-align-all-tables nil)
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

  (org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)

  (org-goto-max-level 3)
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)

  (org-latex-compiler "xelatex")
  (org-latex-packages-alist '(("" "ctex" t)
                              ("" "bm")
                              ("" "amsfonts")
                              ("" "xcolor" t)
                              ("" "minted" t)))
  (org-latex-minted-options '(("breaklines")
                              ("bgcolor" "bg")))
  (org-latex-src-block-backend 'minted)
  ;; (org-latex-preview-numbered t)
  (org-startup-with-latex-preview nil)
  (org-highlight-latex-and-related '(latex))
  (org-preview-latex-default-process 'dvisvgm)
  (org-latex-preview-ltxpng-directory
   (no-littering-expand-var-file-name "ltximg/"))

  (org-footnote-auto-adjust nil)

  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-confirm-babel-evaluate nil)
  (org-src-preserve-indentation nil)
  (org-src-window-setup 'split-window-right)
  (org-src-ask-before-returning-to-edit-buffer nil)

  (org-babel-default-header-args
   '(
     (:evel    . "never-export")
     (:session . "none")
     (:results . "replace")
     (:exports . "both")
     (:cache   . "no")
     (:noweb   . "no")
     (:hlines  . "no")
     (:wrap    . "results")
     (:tangle  . "no")
     ))

  (org-modules '(ol-info ol-eww ol-docview org-habit org-tempo))

  ;; tag
  (org-tags-column 0)
  (org-auto-align-tags t)
  (org-tags-exclude-from-inheritance '(project crypt))
  (org-tag-persistent-alist '((:startgroup . nil)
                              ("#math" . ?m) ("#ai" . ?a) ("#tech" . ?t) ("#life")
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

  (org-default-notes-file
   (expand-file-name "inbox.org" org-directory))

  (org-capture-bookmark nil)
  (org-capture-templates
   '(("t" "Task"  entry (file+headline org-default-notes-file "Task")
      "* TODO [#B] %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+7d\"))\n" :prepend t)
     ("s" "Someday"  entry (file+headline org-default-notes-file "Someday/Maybe")
      "* SOMEDAY [#C] %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"12/30\"))" :prepend t)
     ("h" "Habit" entry (file+headline org-default-notes-file "Habit")
      "* NEXT [#B] %?\nSCHEDULED: \<%(format-time-string (string ?% ?F ?  ?% ?a) (current-time)) .+1d/7d\>\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n" :prepend t))
   )

  (org-stuck-projects '("+project/-DONE-CANCELED"
                        ("NEXT")
                        nil ""))

  (org-scheduled-past-days 36500)
  (org-deadline-warning-days 365)

  (org-habit-show-all-today nil)

  (org-agenda-span 'day)
  (org-agenda-files `(,org-default-notes-file))
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

  (org-global-properties
   '(("STYLE_ALL"  . "habit")
     ("Score_ALL"  . "1 2 3 5 8")
     ("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00")))
  (org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")

  (org-cite-csl-styles-dir
   (expand-file-name "styles/" yx/zotero-dir))
  (org-cite-export-processors
   '((latex biblatex)
     (t . (csl "ieee.csl"))))
  (org-cite-global-bibliography
   (list (expand-file-name "bibliography.bib" yx/org-dir)))

  (org-attach-id-dir
   (expand-file-name "data/" yx/org-dir))
  (org-attach-store-link-p 'attach)
  (org-attach-sync-delete-empty-dir t)

  (org-export-with-toc t)
  (org-export-with-drawers nil)
  (oeg-export-with-footnotes t)
  (org-export-with-broken-links t)
  (org-export-with-section-numbers nil)
  (org-export-with-sub-superscripts '{})

  :config
  (plist-put org-format-latex-options :scale 1.8)
  ;; (plist-put org-latex-preview-options :zoom 1.15)
  ;; (plist-put org-latex-preview-options :scale 2.20)
  (with-eval-after-load 'ox-latex
    (setq org-latex-logfiles-extensions
          (append org-latex-logfiles-extensions '("toc" "dvi" "tex" "bbl"))))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
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
  (yx/def-org-maybe-surround "~" "=" "*" "/" "+")
  (add-hook 'org-ctrl-c-ctrl-c-hook 'yx/check-latex-fragment)

  :custom-face
  (org-level-1 ((t (:height 1.3))))
  (org-level-2 ((t (:height 1.2))))
  (org-level-3 ((t (:height 1.1))))
  (org-document-title ((t (:height 1.5))))

  :preface
  (defun yx/org-mode-setup ()
    (auto-fill-mode -1)
    (variable-pitch-mode 1)
    (yas-minor-mode -1) ; confict with C-c &
    (push 'cape-tex completion-at-point-functions)
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table)
    )

  (defun yx/check-latex-fragment ()
    (let ((datum (org-element-context)))
      (when (memq (org-element-type datum) '(latex-environment latex-fragment))
        (org-latex-preview)
        t))
    )

  (defun yx/org-agenda-finalize-setup ()
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt)
    )

  (defun yx/org-babel-display-image ()
    (when org-inline-image-overlays
      (org-redisplay-inline-images))
    )

  (defun yx/org-clock-in ()
    (interactive)
    (org-clock-in '(4))
    )

  (transient-define-prefix yx/transient-org ()
    "Org commands."
    [["Misc"
      ("@" "org-cite-insert" org-cite-insert)
      ("a" "org-archive-subtree" org-archive-subtree)
      ("g" "org-goto" org-goto)
      ("i" "org-clock-in" org-clock-in)
      ("o" "org-clock-out" org-clock-out)
      ("n" "org-narrow-to-subtree" org-narrow-to-subtree)]
     ["Toggle"
      ("L" "org-toggle-link-display" org-toggle-link-display)
      ("I" "org-toggle-inline-images" org-toggle-inline-images)
      ("F" "org-preview-latex-fragment" org-preview-latex-fragment)]
     ]
    ))

;; %% org+
(use-package org-ql
  :after org
  )

(use-package org-super-agenda
  :init
  (setq
   org-super-agenda-groups
   '((:name "Today"
            :time-grid t
            :deadline today
            :scheduled today)
     (:name "Important"
            :tag "urgent"
            :priority>= "A")
     (:name "Overdue"
            :deadline past)
     (:name "Next"
            :todo "NEXT")
     (:name "Someday/Hold"
            :todo "HOLD"
            :todo "WAITING"
            :todo "SOMEDAY")
     (:name "Other"
            :anything))
   )
  :hook (org-agenda-mode . org-super-agenda-mode)
  )

(use-package org-modern
  :after org
  :demand t
  :config
  (global-org-modern-mode)
  )

(use-package valign
  :hook (org-mode . valign-mode)
  :custom
  (valign-fancy-bar 1)
  )

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-delay 1)
  (org-appear-autolinks nil)
  (org-appear-inside-latex t)
  (org-appear-autoentities t)
  (org-appear-autoemphasis t)
  (org-appear-autosubmarkers t)
  )

(use-package org-download
  :after org
  :hook (dired-mode . org-download-enable)
  :commands (org-download-screenshot org-download-clipboard)
  :custom
  (org-download-heading-lvl nil)
  (org-download-screenshot-method "screencapture -i %s")
  (org-download-image-dir
   (expand-file-name (concat org-attach-directory "images/") yx/org-dir))
  :bind
  (:map org-mode-map
        ("C-c y" . org-download-screenshot)
        ("C-c C-y" . org-download-clipboard))
  )

(use-package org-web-tools)

;;; Reading
(setq
 doc-view-continuous t
 doc-view-resolution 300)

(use-package pdf-tools
  :if (display-graphic-p)
  :hook
  (pdf-tools-enabled . pdf-isearch-minor-mode)
  :init
  (setq
   pdf-view-use-scaling t
   pdf-view-use-imagemagick nil)
  (setq-default pdf-view-display-size 'fit-width)
  (pdf-loader-install)
  )

;; %% olivetti
(use-package olivetti
  :hook ((org-mode
          org-agenda-mode
          ) . olivetti-mode)
  :init
  (setq
   olivetti-style nil
   olivetti-mode-map nil
   olivetti-body-width 0.66
   olivetti-minimum-body-width (+ fill-column 2)
   )
  )

;; %% elfeed
(use-package elfeed
  :init
  (setq elfeed-search-print-entry-function
        'yx/elfeed-search-print-entry--better-default)
  :custom
  (elfeed-feeds
   '(("https://www.inference.vc/rss" ai)
     ("https://spaces.ac.cn/feed" ai webkit)
     ("https://ruder.io/rss/index.rss" ai)
     ("https://lilianweng.github.io/index.xml" ai webkit)
     ("https://www.juliabloggers.com/feed/" julia)
     ("https://planet.lisp.org/rss20.xml" lisp)
     ("https://planet.scheme.org/atom.xml" scheme)
     ("https://planet.haskell.org/rss20.xml" haskell)
     ("https://planet.emacslife.com/atom.xml" emacs)
     ("http://lambda-the-ultimate.org/rss.xml" lang)
     ("https://matt.might.net/articles/feed.rss" lang)
     ("http://www.ruanyifeng.com/blog/atom.xml" tech webkit)
     ("https://vimtricks.com/feed/" vim)
     ("https://egh0bww1.com/rss.xml" emacs)
     ("https://karthinks.com/index.xml" emacs)
     ("https://manateelazycat.github.io/feed.xml" emacs)
     ("https://matt.might.net/articles/feed.rss" emacs)
     ("https://andreyor.st/categories/emacs/feed.xml" emacs)
     ("https://sachachua.com/blog/category/emacs/feed/" emacs)))
  (elfeed-search-filter "@6-months-ago +unread")
  :hook (elfeed-show . olivetti-mode)
  :config
  (run-at-time nil (* 4 60 60) 'elfeed-update)
  (keymap-set elfeed-search-mode-map "m" (yx/elfeed-tag-selection-as 'star))
  (keymap-set elfeed-search-mode-map "l" (yx/elfeed-tag-selection-as 'readlater))
  :preface
  (defun yx/elfeed-kill-entry ()
    "Like `elfeed-kill-entry' but pop elfeed search"
    (interactive)
    (elfeed-kill-buffer)
    (switch-to-buffer "*elfeed-search*"))
  (defun yx/elfeed-tag-selection-as (mytag)
    (lambda ()
      "Toggle a tag on an Elfeed search selection"
      (interactive)
      (elfeed-search-toggle-all mytag)))
  :bind
  (:map elfeed-show-mode-map
        ("w" . elfeed-show-yank)
        ("%" . elfeed-webkit-toggle)
        ("q" . yx/elfeed-kill-entry)
        )
  )

(use-package elfeed-webkit
  :after elfeed
  :config
  (elfeed-webkit-auto-toggle-by-tag)
  :bind
  (:map elfeed-webkit-map
        ("q" . yx/elfeed-kill-entry))
  )

;;; Writing
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
  (require 'denote-org-dblock)
  (denote-rename-buffer-mode 1)
  :preface
  (defun yx/denote-template ()
    "Create note while prompting for a template.
This is equivalent to calling `denote' when `denote-prompts' is
set to \\='(template title keywords subdirectory)."
    (declare (interactive-only t))
    (interactive)
    (let ((denote-prompts '(template subdirectory title keywords)))
      (call-interactively #'denote)))
  )

(use-package citar-denote
  :after (citar denote)
  :demand t
  :config
  (setq citar-denote-subdir t)
  (citar-denote-mode)
  )

(use-package edit-indirect)
(use-package org-transclusion)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode))
  :init
  (setq
   markdown-command "pandoc"
   markdown-enable-math t
   markdown-header-scaling t)
  )

;; %% latex
(use-package tex
  :ensure auctex
  :mode (("\\.tex\\'" . LaTeX-mode))
  :config
  (setq-default
   Tex-master nil
   TeX-engine 'xetex)
  (setq
   TeX-auto-save t
   TeX-parse-self t
   reftex-plug-into-AUCTeX t
   TeX-source-correlate-start-server t)

  (setq TeX-view-program-list
        '(("pdf-tools" TeX-pdf-tools-sync-view)
          ("Skim" "displayline -b -g %n %o %b"))
        TeX-view-program-selection
        `((output-pdf ,(cond
                        (IS-MAC "Skim")
                        (t "pdf-tools")))
          (output-dvi  ,yx/default-open-program)
          (output-html ,yx/default-open-program)))

  (defun yx/latex-mode-setup ()
    (TeX-PDF-mode 1)
    (TeX-fold-mode 1)
    (TeX-source-correlate-mode 1)
    (LaTeX-math-mode 1)
    (prettify-symbols-mode 1)
    (turn-on-reftex)
    (push 'cape-tex completion-at-point-functions))
  (add-hook 'tex-mode-hook 'yx/latex-mode-setup)
  (add-hook 'TeX-after-comilation-finished-functions 'TeX-revert-document-buffer)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(latex-mode "texlab")))
  )

(use-package cdlatex
  :hook
  ((LaTeX-mode . turn-on-cdlatex)
   (org-mode   . turn-on-org-cdlatex))
  )

(use-package transform
  :ensure nil
  :commands (transform-greek-help
             transform-previous-char)
  )

;; %% citar
(use-package citar
  :after org
  :demand t
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-notes-paths `(,yx/org-dir))
  (citar-library-paths `(,yx/zotero-dir))
  (citar-at-point-function 'embark-act)
  (citar-bibliography org-cite-global-bibliography)
  :hook
  (org-mode . citar-capf-setup)
  (LaTeX-mode . citar-capf-setup)
  )

(use-package citar-embark
  :after citar embark
  :demand t
  :no-require t
  :config (citar-embark-mode +1))

;;; Programming
(defun yx/prog-common-setup ()
  (setq-local
   whitespace-style
   '(face trailing lines-char space-before-tab space-after-tab))
  (whitespace-mode            1)
  (reveal-mode                1)
  (hl-line-mode               1)
  (hs-minor-mode              1)
  (superword-mode             1)
  (show-paren-mode            1)
  (electric-pair-mode         1)
  (display-line-numbers-mode  1)
  (electric-indent-local-mode 1)
  (keymap-local-set "RET" 'newline-and-indent)
  (push 'cape-keyword completion-at-point-functions))

(add-hook 'prog-mode-hook 'yx/prog-common-setup)

;; ediff
(setq
 diff-default-read-only t
 diff-update-on-the-fly t)

(setq
 ediff-keep-variants nil
 ediff-show-clashes-only t
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-split-window-function 'split-window-horizontally)

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; %% formatter & linter & profiler
(setq
 flymake-no-changes-timeout nil
 flymake-start-on-save-buffer t
 flymake-start-on-flymake-mode t
 flymake-fringe-indicator-position 'right-fringe)
(with-eval-after-load 'flymake
  (bind-keys :map flymake-mode-map
             ("s-; s"   . flymake-start)
             ("s-; d"   . flymake-show-buffer-diagnostics)
             ("s-; M-d" . flymake-show-project-diagnostics)
             ("s-; M-n" . flymake-goto-next-error)
             ("s-; M-p" . flymake-goto-prev-error))
  )

(use-package apheleia
  :init
  (apheleia-global-mode +1))

(use-package ws-butler
  :hook ((prog-mode conf-mode) . ws-butler-mode))

;; %% snippet
(use-package tempel
  :defer 2
  :bind
  (("M-+" . tempel-insert)
   ("M-=" . tempel-complete)
   :map tempel-map
   ([tab] . tempel-next)
   ([backtab] . tempel-previous))
  :hook
  ((prog-mode text-mode) . tempel-setup-capf)
  :init
  (setq tempel-path
        (expand-file-name "templates/tempel.eld" no-littering-etc-directory))
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons 'tempel-expand
                      completion-at-point-functions)))
  )

(use-package yasnippet
  :ensure t
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode) . yas-minor-mode-on)
  )

(use-package yasnippet-snippets)

;; %% version control
(use-package magit
  :init
  (setq
   magit-diff-refine-hunk t
   magit-show-long-lines-warning nil))

(use-package diff-hl
  :hook
  (after-init . global-diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (setq
   diff-hl-disable-on-remote t
   diff-hl-show-staged-changes t)
  (diff-hl-flydiff-mode 1)
  (global-diff-hl-show-hunk-mouse-mode 1))

(use-package git-modes)

;; %% indent
(use-package indent-guide
  :hook (prog-mode . indent-guide-mode)
  :custom
  (indent-guide-recursive nil))

(use-package editorconfig
  :defer 2
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  :config
  (editorconfig-mode 1))

(use-package snap-indent
  :hook (prog-mode . snap-indent-mode)
  :custom
  (snap-indent-format '(delete-trailing-whitespace)))

(use-package aggressive-indent
  :hook ((emacs-lisp-mode scheme-mode-hook) . aggressive-indent-mode))

;; %% doc
(use-package devdocs
  :init
  (add-hook 'julia-ts-mode-hook
            (lambda () (setq-local devdocs-current-docs '("julia~1.9"))))
  (add-hook 'python-base-mode-hook
            (lambda () (setq-local devdocs-current-docs '("python~3.12" "pytorch" "numpy~1.23"))))
  )

;; %% symbol highlight
(use-package rainbow-mode
  :custom
  (rainbow-x-colors nil)
  :hook (emacs-lisp . rainbow-mode))

(use-package hl-todo
  :hook ((text-mode prog-mode) . hl-todo-mode)
  :init
  (setq hl-todo-keyword-faces
        '(("TODO"  . "#cc9393")
          ("NEXT"  . "#dca3a3")
          ("OKAY"  . "#7cb8bb")
          ("DONT"  . "#5f7f5f")
          ("FAIL"  . "#8c5353")
          ("HACK"  . "#d0bf8f")
          ("FIXME" . "#cc9393")
          ("ISSUE" . "#e45649")
          ("TRICK" . "#d0bf8f")))
  (setq hl-todo-require-punctuation t
        hl-todo-highlight-punctuation ":")

  (defun yx/hl-todo-rg-project ()
    "Use `rg' to find all TODO or similar keywords."
    (interactive)
    (unless (require 'color-rg nil t)
      (error "`color-rg' is not installed"))
    (let* ((regexp (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp))))
      (color-rg-search-input regexp (color-rg-project-root-dir))
      ))
  )

(use-package symbol-overlay
  :custom
  (symbol-overlay-priority 0)
  :hook ((prog-mode conf-mode) . symbol-overlay-mode)
  :bind (:map symbol-overlay-map
              ("u" . symbol-overlay-remove-all))
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package treesit
  :unless IS-WIN
  :ensure nil
  :init
  (setq
   major-mode-remap-alist
   '((c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (python-mode . python-ts-mode))
   treesit-language-source-alist
   '((c      "https://github.com/tree-sitter/tree-sitter-c")
     (cpp    "https://github.com/tree-sitter/tree-sitter-cpp")
     (lua    "https://github.com/MunifTanjim/tree-sitter-lua")
     (org    "https://github.com/milisims/tree-sitter-org")
     (bash "https://github.com/tree-sitter/tree-sitter-bash")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (julia  "https://github.com/tree-sitter/tree-sitter-julia")
     (python "https://github.com/tree-sitter/tree-sitter-python"))
   treesit-load-name-override-list '((c++ "libtree-sitter-cpp"))
   treesit-extra-load-path (list (no-littering-expand-var-file-name "tree-sitter")))
  (defun yx/treesit--install-language-grammar (lang-pair)
    (let ((lang (car lang-pair)))
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang (car treesit-extra-load-path)))))
  (mapc 'yx/treesit--install-language-grammar treesit-language-source-alist))

;; %% refoctor
(use-package color-rg
  :vc (:url https://github.com/manateelazycat/color-rg :rev :newest)
  :defer 2
  :custom
  (color-rg-search-no-ignore-file nil)
  (color-rg-mac-load-path-from-shell nil)
  )

;; %% structured edit
(use-package iedit)

(use-package surround
  :defer 2
  :bind-keymap ("M-'" . surround-keymap))

(use-package puni
  :hook ((tex-mode
          prog-mode
          sgml-mode
          nxml-mode) . puni-mode)
  :bind
  (:map puni-mode-map
        ("DEL"     . nil)
        ("C-d"     . nil)
        ("C-w"     . nil)
        ("s-' r"   . puni-raise)
        ("s-' u"   . puni-splice)
        ("s-' M-s" . puni-squeeze)
        ("s-' l"   . puni-slurp-forward)
        ("s-' h"   . puni-slurp-backward)
        ("s-' M-l" . puni-barf-forward)
        ("s-' M-h" . puni-barf-backward)
        ("s-' m"   . puni-mark-sexp-at-point)
        ("s-' M-m" . puni-mark-sexp-around-point)
        ("s-' ="   . puni-expand-region)
        ("s-' -"   . puni-contract-region)
        )
  )

(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate" :rev :newest)
  :custom
  (combobulate-key-prefix "M-l l")
  :hook ((python-ts-mode
          ) . combobulate-mode)
  )

;; %% lsp
(use-package eglot
  :ensure nil
  :hook
  ((c-mode
    c-ts-mode
    R-mode
    python-mode
    python-ts-mode
    julia-mode
    julia-ts-mode
    LaTeX-mode
    haskell-mode) . eglot-ensure)
  :init
  (setq
   eglot-autoshutdown t
   eglot-extend-to-xref t
   eglot-sync-connect nil
   eglot-report-progress nil
   eglot-events-buffer-size 0
   eglot-send-changes-idle-time 0.3)
  :bind
  (:map
   eglot-mode-map
   ("s-; r" . eglot-rename)
   ("s-; f" . eglot-format)
   ("s-; a" . eglot-code-actions)
   ("s-; g" . consult-eglot-symbols))
  :config
  (fset #'jsonrpc--log-event #'ignore) ; massive perf boost---don't log every event
  )

(use-package consult-eglot
  :after consult
  )

(use-package sideline
  :hook
  (flymake-mode . yx/sideline-flymake-mode-setup)
  :preface
  (defun yx/sideline-flymake-mode-setup ()
    (setq-local
     sideline-backends-right '(sideline-flymake))
    (sideline-mode 1)))
(use-package sideline-flymake)

(use-package dape
  :init
  (setq
   dape-adapter-dir
   (no-littering-expand-var-file-name "dape-debug-adapters")
   dape-buffer-window-arrangment 'right)
  )

(use-package quickrun
  :custom
  (quickrun-focus-p nil)
  )

;;; Langs
(defvar yx/default-python-env "~/workspace/.venv/")

;; %% emacs-lisp
(define-auto-insert "\\.el$" 'yx/auto-insert-el-header)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (prettify-symbols-mode)
            (treesit-parser-create 'elisp)))

;; %% c/c++
(setq
 c-basic-offset 8
 c-default-style
 '((java-mode . "java")
   (awk-mode  . "awk")
   (other     . "linux"))
 )

(setq
 c-ts-mode-indent-offset 8
 c-ts-mode-indent-style 'linux)

(add-hook 'c-mode-common-hook
          (lambda () (c-toggle-auto-hungry-state 1)))

(define-auto-insert
  "\\.\\([Hh]\\|hh\\|hpp\\|hxx\\|h\\+\\+\\)\\'"
  'yx/auto-insert-h-header)
(define-auto-insert
  "\\.\\([Cc]\\|cc\\|cpp\\|cxx\\|c\\+\\+\\)\\'"
  'yx/auto-insert-c-header)

;; %% code-cell
(use-package code-cells
  :hook ((julia-mode
          python-ts-mode) . code-cells-mode-maybe)
  :bind (:map code-cells-mode-map
              ("M-p"     . code-cells-backward-cell)
              ("M-n"     . code-cells-forward-cell))
  :config
  (setq code-cells-eval-region-commands
        '((python-ts-mode . python-shell-send-region)
          (emacs-lisp-mode . eval-region)))
  (with-eval-after-load 'jupyter
    (defalias 'adopt-jupyter-eval-region (apply-partially 'jupyter-eval-region nil))
    (add-to-list 'code-cells-eval-region-commands
                 '(jupyter-repl-interaction-mode . adopt-jupyter-eval-region)))
  (with-eval-after-load 'julia-snail
    (add-to-list 'code-cells-eval-region-commands
                 '(julia-snail-mode . julia-snail-send-code-cell)))
  )

;; %% python
(setq
 python-shell-dedicated t
 python-skeleton-autoinsert t
 python-indent-guess-indent-offset t
 python-indent-guess-indent-offset-verbose nil
 python-shell-virtualenv-root yx/default-python-env
 python-shell-interpreter "jupyter"
 python-shell-interpreter-args "console --simple-prompt"
 python-shell-completion-native-disabled-interpreters '("ipython" "jupyter"))

(defun yx/python-mode-setup ()
  (setq-local
   tab-width 4
   python-indent-offset 4
   electric-indent-inhibit t
   imenu-create-index-function 'python-imenu-create-flat-index
   ))
(add-hook 'python-ts-mode-hook 'yx/python-mode-setup)

(define-auto-insert "\\.py$" 'yx/auto-insert-common-header)

(use-package pyvenv
  :hook (after-init . yx/active-default-pyvenv)
  :preface
  (defun yx/active-default-pyvenv ()
    (interactive)
    (pyvenv-activate yx/default-python-env)
    )
  )

(use-package pyvenv-auto
  :hook (python-ts-mode . pyvenv-auto-run))

(use-package poetry
  :hook (python-ts-mode . poetry-tracking-mode))

(use-package jupyter
  :after org
  :demand t
  :config
  (setq jupyter-eval-use-overlays nil)
  ;; @see https://github.com/emacs-jupyter/jupyter/issues/478
  (setf (alist-get "python" org-src-lang-modes nil nil #'equal) 'python-ts)
  )

;; %% R/julia
(use-package ess-site
  :ensure ess
  :init
  (setq
   ess-eval-visibly-p 'nowait
   ess-local-process-name "R"
   ess-ask-for-ess-directory nil)
  :config
  (keymap-set ess-r-mode-map ";" 'ess-insert-assign)
  (keymap-set inferior-ess-r-mode-map ";" 'ess-insert-assign)
  )

(use-package julia-mode)
(use-package julia-ts-mode
  :mode "\\.jl$")

(use-package eglot-jl
  :init
  (with-eval-after-load 'eglot
    (eglot-jl-init))
  )

(use-package julia-snail
  :custom
  (julia-snail-terminal-type :eat)
  (julia-snail-extensions '(ob-julia formatter))
  :hook
  (julia-mode . julia-snail-mode)
  (julia-ts-mode . julia-snail-mode)
  )

(define-auto-insert "\\.R$" 'yx/auto-insert-common-header)
(define-auto-insert "\\.jl$" 'yx/auto-insert-common-header)

;; %% toy langs
(use-package lua-ts-mode
  :vc (:url "https://git.sr.ht/~johnmuhl/lua-ts-mode" :rev :newest)
  :mode ("\\.lua\\'" . lua-ts-mode)
  :init (setq lua-ts-indent-offset 2))

(setq scheme-program-name "chez")
(use-package geiser-chez
  :mode ("\\.sc\\'" . scheme-mode)
  :custom (geiser-chez-binary "chez"))

(use-package haskell-mode
  :hook (haskell-mode . yx/haskell-mode-setup)
  :custom
  (haskell-stylish-on-save t)
  (haskell-process-log t)
  (haskell-process-auto-import-loaded-modules t)
  :preface
  (defun yx/haskell-mode-setup ()
    (haskell-collapse-mode 1)
    (haskell-decl-scan-mode 1)
    (haskell-auto-insert-module-template)
    (speedbar-add-supported-extension ".hs")
    (eval-after-load "which-func"
      '(add-to-list 'which-func-modes 'haskell-mode)))
  )

;; %% misc lang
(add-hook
 'sh-mode-hook
 (lambda()
   (setq sh-indentation 2
         sh-basic-offset 2)
   (electric-pair-mode -1)
   (compilation-shell-minor-mode 1)
   ))

(use-package vimrc-mode
  :mode "\\.?vim\\(rc\\)?\\'")

(use-package gnuplot-mode
  :mode "\\.gp$")

(use-package graphviz-dot-mode)

;; %% maxima
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)
(add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode))
;;; init.el ends here
