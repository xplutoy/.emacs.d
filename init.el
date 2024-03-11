;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:13:09
;; Modified: <2024-03-11 08:37:37 yx>
;; Licence: GPLv3

;;; Init
(defvar yx/etc-dir "~/.emacs.d/etc/")
(defvar yx/var-dir "~/.emacs.d/.cache/")

;; env
(setenv "http_proxy"  "http://127.0.0.1:7890")
(setenv "https_proxy" "http://127.0.0.1:7890")

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(setq custom-file
      (expand-file-name "custom.el" yx/etc-dir))

(require 'package)
(require 'use-package-ensure)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(setq package-archives
      '(("gnu"          . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu"       . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"        . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")))

(setq package-quickstart nil
      package-install-upgrade-built-in t
      package-user-dir (expand-file-name "elpa" yx/var-dir)
      package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))

(setq use-package-verbose t
      use-package-always-defer t
      use-package-always-ensure t
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

(when (daemonp)
  (setq use-package-always-demand t))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; %% benchmark
(use-package benchmark-init)
;; (benchmark-init/activate)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

;; %% no-littering
(use-package no-littering
  :demand t
  :init
  (setq no-littering-var-directory yx/var-dir
        no-littering-etc-directory yx/etc-dir)
  :config
  (no-littering-theme-backups))

;;; Utils
(defvar yx/org-root         "~/yxdocs/org-notes/")
(defvar yx/zotero-root      "~/Zotero/")
(defvar yx/gpg-sign-key    "67B86CB8A5630C51!")
(defvar yx/gpg-encrypt-key "8B1F9B207AF00BCF!")

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-WIN     (eq system-type 'windows-nt))
(defconst IS-LINUX   (eq system-type 'gnu/linux))

(defvar yx/default-open (cond
                         (IS-WIN   "start")
                         (IS-MAC   "open")
                         (IS-LINUX "xdg-open")
                         (t "")))

(defconst yx/templates-dir
  (no-littering-expand-etc-file-name "templates"))

(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list (delq ,(if fetcher
                          `(funcall ,fetcher ,elt ,list)
                        elt)
                     ,list)))

(defun yx/pwd-replace-home (pwd)
  "Replace home in PWD with tilde (~) character."
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun yx/file-contents-2-str (file)
  "File contents to string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;; Templates
(require 'tempo)
(tempo-define-template
 "yx/tex-note-tmpl"
 `(,(yx/file-contents-2-str (expand-file-name "math-note.tmpl.tex" yx/templates-dir))))

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
(setq user-full-name "yangxue")
(setq user-mail-address "yangxue.cs@foxmail.com")

(setq system-time-locale "C")
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

(setq-default abbrev-mode t)

(setq-default tab-width 8
              indent-tabs-mode nil
              tab-always-indent 'complete)

(setq visible-bell nil)

(setq use-short-answers t
      y-or-n-p-use-read-key t)

(setq hl-line-sticky-flag nil
      global-hl-line-sticky-flag nil)

(setq-default word-wrap t
              fill-column 89
              truncate-lines t)

(setq track-eol t
      line-move-visual nil
      word-wrap-by-category t
      truncate-partial-width-windows nil
      set-mark-command-repeat-pop t
      backward-delete-char-untabify-method 'hungry)

(setq find-file-visit-truename t
      delete-by-moving-to-trash t)

(setq sentence-end-double-space nil
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

(setq confirm-kill-processes nil
      disabled-command-function nil
      confirm-nonexistent-file-or-buffer nil)

(use-package startup
  :ensure nil
  :custom
  (inhibit-default-init t)
  (inhibit-splash-screen t)
  (inhibit-startup-message t)
  (initial-scratch-message nil))

;; %% paren
(setq blink-matching-paren nil
      show-paren-style 'parenthesis
      show-paren-context-when-offscreen t
      show-paren-when-point-inside-paren t)

;; %% electric
(setq electric-pair-preserve-balance nil
      electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

;; %% font-lock
(setq jit-lock-defer-time 0
      jit-lock-chunk-size 4096
      jit-lock-stealth-time 2.0
      jit-lock-stealth-nice 0.2)

;; %% auto save
(setq save-silently t
      auto-save-default t
      auto-save-timeout 10
      auto-save-no-message t
      kill-buffer-delete-auto-save-files t
      auto-save-visited-interval 15
      auto-save-visited-predicate
      (lambda () (and (buffer-modified-p)
                      (not (string-suffix-p "gpg" (file-name-extension (buffer-name)) t)))))

(setq version-control t
      backup-by-copying t
      kept-new-versions 5
      delete-old-versions t)

(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

(setq auto-insert-query nil
      auto-insert-alist nil
      auto-insert-directory (no-littering-expand-etc-file-name "templates/"))

(setq time-stamp-pattern "10/^[@#;\*].*[Mm]odified: <%%>$")

(add-hook 'before-save-hook 'time-stamp)

(setq tempo-interactive t)

(setq view-read-only t)
(add-hook 'view-mode-hook
          (lambda () (setq cursor-type (if view-mode 'hollow 'box))))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-idle-delay 0.3)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-display-truncation-message nil))

(use-package help
  :ensure nil
  :custom
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
  (display-line-numbers-type t)
  (display-line-numbers-width 4)
  (display-line-numbers-major-tick 20)
  (display-line-numbers-width-start t))

(use-package re-builder
  :ensure nil
  :custom (reb-re-syntax 'string))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 100
        recentf-auto-cleanup "1:00am"
        recentf-exclude
        '("\\.?cache.*" "^/.*" "^/ssh:" "\\.git/.+$"
          "COMMIT_MSG" "COMMIT_EDITMSG" "/Downloads/" "/elpa/"
          "\\.\\(?:gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
          "\\.\\(?:gz\\|zip\\|gpg\\)$"
          file-remote-p))

  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))

  ;; Text properties inflate the size of recentf's files
  (add-to-list 'recentf-filename-handlers #'substring-no-properties))

(use-package whitespace
  :ensure nil
  :hook ((prog-mode conf-mode) . whitespace-mode)
  :custom
  (whitespace-style '(face trailing space-before-tab space-after-tab)))

;; prettify
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; %% xref
(when-let ((rg (executable-find "rg")))
  (setq grep-program rg))

;; %% completion minibuffer
(setq resize-mini-windows t
      max-mini-window-height 0.3
      enable-recursive-minibuffers t)

(setq completions-detailed t
      completion-ignore-case t
      completions-format 'one-column
      completions-header-format nil
      completions-max-height 30
      completion-auto-help 'visible
      completion-cycle-threshold 3
      completion-show-help nil
      completion-show-inline-help nil
      completion-auto-select 'second-tab
      minibuffer-visible-completions t)

(setq read-extended-command-predicate 'command-completion-default-include-p)

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq abbrev-suggest t
      save-abbrevs 'silently
      abbrev-suggest-hint-threshold 2)

(setq hippie-expand-max-buffers 5
      hippie-expand-try-functions-list '(try-complete-file-name
                                         try-complete-file-name-partially
                                         try-expand-dabbrev
                                         try-expand-dabbrev-from-kill
                                         try-expand-dabbrev-all-buffers))


(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
              ([remap isearch-delete-char] . isearch-del-char))
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

(add-hook 'occur-mode-hook #'hl-line-mode)

;; %% epa
(setq auth-sources `(,(no-littering-expand-etc-file-name "authinfo.gpg"))
      auth-source-debug t
      epa-pinentry-mode 'loopback
      epa-file-select-keys yx/gpg-encrypt-key)

(setq password-cache t
      password-cache-expiry (* 60 60))

;; %% mouse
(setq mouse-yank-at-point t
      mouse-wheel-tilt-scroll t
      mouse-wheel-follow-mouse t
      mouse-drag-mode-line-buffer t
      mouse-avoidance-nudge-dist 8
      mouse-drag-and-drop-region-cross-program t
      mouse-wheel-scroll-amount-horizontal 2
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll)))

;; %% scroll
(setq scroll-step 1
      scroll-margin 1
      scroll-conservatively 101
      fast-but-imprecise-scrolling t
      scroll-preserve-screen-position 'always)

(use-package pixel-scroll
  :ensure nil
  :hook (after-init . pixel-scroll-precision-mode))

(setq auto-window-vscroll nil
      auto-hscroll-mode 'current-line)

(setq transient-detect-key-conflicts t
      transient-highlight-mismatched-keys nil)

;; bookmark
(setq bookmark-save-flag 1
      bookmark-fringe-mark nil
      bookmark-use-annotations nil
      bookmark-automatically-show-annotations nil)

;; proced
(setq proced-descend t
      proced-filter 'user
      proced-enable-color-flag t
      proced-auto-update-flag nil)

;; %% browser-url
(setq shr-use-fonts nil
      shr-use-colors nil
      shr-image-animate nil
      shr-inhibit-images t
      shr-max-image-proportion 0.6)

(setq image-use-external-converter t)

(setq browse-url-browser-function 'eww-browse-url
      browse-url-secondary-browser-function 'browse-url-default-browser
      browse-url-generic-program yx/default-open)

(setq eww-auto-rename-buffer 'title
      eww-search-prefix "http://www.google.com/search?q="
      eww-use-external-browser-for-content-type "\\`\\(video/\\|audio\\)"
      eww-browse-url-new-window-is-tab nil)

(with-eval-after-load 'eww
  (add-hook 'eww-after-render-hook 'eww-readable))

(use-package webjump
  :ensure nil
  :custom
  (webjump-sites '(("Org"        . "https://orgmode.org")
                   ("Google"     . [simple-query "www.google.com" "www.google.com/search?q=" ""])
                   ("DuckDuckGo" . [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
                   ("Wikipedia"  . [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""]))))

;; %% flyspell
(setq ispell-dictionary "english"
      ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))

(with-eval-after-load 'flyspell
  (keymap-unset flyspell-mode-map "C-,")
  (keymap-unset flyspell-mode-map "C-.")
  (keymap-unset flyspell-mode-map "C-;"))

(use-package autorevert
  :ensure nil
  :config
  (setq auto-revert-verbose nil
        auto-revert-use-notify nil
        revert-without-query '(".")
        auto-revert-check-vc-info t)
  (global-auto-revert-mode 1))

;; %% https://emacs-china.org/t/emacs-1w/25802/20
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      syntax-wholeline-max 1000
      large-hscroll-threshold 1000)

(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode)
  :config
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  (appendq! so-long-minor-modes '(eldoc-mode
                                  highlight-numbers-mode
                                  ws-butler-mode
                                  indent-guide-mode )))

(setq dictionary-server "dict.org"
      dictionary-default-popup-strategy "lev"
      dictionary-create-buttons nil
      dictionary-display-definition-function 'dictionary-display-definition-in-help-buffer)

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package savehist
  :ensure nil
  :config
  (setq history-delete-duplicates t
        savehist-save-minibuffer-history t
        savehist-additional-variables '(kill-ring
                                        mark-ring global-mark-ring
                                        search-ring regexp-search-ring
                                        command-history)))

(use-package server
  :ensure nil
  :defer 1
  :custom (server-client-instructions nil)
  :config
  (unless (server-running-p)
    (server-mode)))

(use-package midnight-mode
  :ensure nil
  :defer 2
  :config
  (setq midnight-period 7200)
  (midnight-mode +1))

;; %% session
(setq desktop-save t
      desktop-restore-eager 5
      desktop-auto-save-timeout 60
      desktop-modes-not-to-sav '(tags-table-mode
                                 dired-mode
                                 eww-mode
                                 comint-mode
                                 elfeed-search-mode
                                 doc-view-mode
                                 Info-mode info-lookup-mode
                                 magit-mode magit-log-mode))

(setq tramp-verbose 1
      tramp-chunksize 2000
      tramp-default-method "ssh"
      remote-file-name-inhibit-locks t
      remote-file-name-inhibit-cache nil
      remote-file-name-access-timeout 2
      remote-file-name-inhibit-auto-save t)

(setq vc-handled-backends '(Git)
      vc-git-diff-switches '("--histogram")
      vc-ignore-dir-regexp (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))

(setq-default bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right)

(setq appt-audible t
      appt-display-interval 10
      appt-display-duration 5
      appt-display-format 'window
      appt-message-warning-time 20)

(add-hook 'emacs-startup-hook #'appt-activate)

(setq calendar-latitude +30.67
      calendar-longitude +104.07
      calendar-date-style 'iso
      calendar-week-start-day 1
      calendar-mark-holidays-flag t
      calendar-mark-diary-entries-flag t)

(add-hook 'calendar-today-visible-hook #'calendar-mark-today)

;; %% os specific settings stay here
(cond
 (IS-MAC
  (setq ns-command-modifier   'super
        ns-alternate-modifier 'meta
        ns-function-modifier  'hyper
        ns-pop-up-frames nil
        ns-use-thin-smoothing t
        ns-use-native-fullscreen nil))
 (IS-WIN
  (setq w32-apps-modifier    'hyper
        w32-lwindow-modifier 'super
        w32-pass-lwindow-to-system nil
        w32-get-true-file-attributes nil
        w32-pipe-read-delay 0
        w32-pipe-buffer-size  (* 64 1024))))

;; %% hook
(defun yx/text-mode-setup ()
  (setq-local word-wrap t
              line-spacing 0.15)
  (superword-mode          1)
  (visual-line-mode        1)
  (goto-address-mode       1)
  (variable-pitch-mode     1)
  (toggle-truncate-lines  -1))

(defun yx/global-mirror-mode-toggle ()
  (repeat-mode            1)
  (desktop-save-mode     -1)
  (blink-cursor-mode     -1)
  (global-reveal-mode     1)
  (auto-compression-mode  1)
  (delete-selection-mode  1)
  (auto-save-visited-mode 1)
  (mouse-avoidance-mode 'cat-and-mouse)
  (unless (display-graphic-p)
    (xterm-mouse-mode 1))
  (minibuffer-depth-indicate-mode 1)
  (auth-source-pass-enable)
  (windmove-default-keybindings 'control))

(add-hook 'text-mode 'yx/text-mode-setup)
(add-hook 'after-init-hook 'yx/global-mirror-mode-toggle)

(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))



;;; Keymaps
;; %% 全局按键
;; This avoids the ugly accidentally action of scaling text with using the trackpad
(keymap-global-unset "C-<wheel-up>")
(keymap-global-unset "C-<wheel-down>")

(use-package key-chord
  :init
  (key-chord-mode 1)
  (key-chord-define-global "zz"     'zoom)
  (key-chord-define-global "jj"     'avy-goto-char-timer)
  (key-chord-define-global "jk"     'avy-goto-word-1)
  (key-chord-define-global "jl"     'avy-goto-line)
  (with-eval-after-load 'org
    (key-chord-define org-mode-map
                      "jh" 'avy-org-goto-heading-timer)))

(defvar-keymap yx/app-prefix-map
  :doc "Keymap for app"
  "g" #'gnus
  "c" #'calendar
  "C" #'calc
  "r" #'elfeed
  "E" #'erc-tls
  "v" #'vterm
  "e" #'emms-browser
  "P" #'proced
  "a" #'org-agenda-list
  "p" #'package-list-packages)
(keymap-global-set "s-a" yx/app-prefix-map)

(bind-keys ([remap move-beginning-of-line]        . crux-move-beginning-of-line) ; C-a
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
           ([remap upcase-word]                   . upcase-dwim) ; M-u
           ([remap downcase-word]                 . downcase-dwim) ; M-l
           ([remap capitalize-word]               . capitalize-dwim) ; M-c
           ([remap goto-char]                     . avy-goto-char-timer) ; M-g c
           ([remap suspend-frame]                 . repeat) ; C-z
           )

(bind-keys ("C-<f5>"    . dape)
           ("<f5>"      . quickrun)
           ("s-e"       . yx/eshell-here)
           ("s-s"       . yx/transient-global-simple)
           ("s-/"       . transform-previous-char)
           ("s-r"       . consult-recent-file)
           ("s-t"       . tab-bar-new-tab)
           ("s-j"       . avy-goto-char-timer)
           ("s-d"       . dirvish-side)
           ("s-i"       . symbols-outline-show)
           ("s-o"       . ace-window)
           ("s-w"       . tabspaces-close-workspace)
           ("s-<right>" . ns-next-frame)
           ("s-<left>"  . ns-prev-frame)
           ("s-]"       . tab-next)
           ("s-["       . tab-previous)
           ("C-;"       . iedit-mode)
           ("C-."       . embark-act)
           ("C-,"       . embark-dwim)
           ("C-/"       . undo-only)
           ("C-^"       . crux-top-join-line)
           ("C-M-/"     . vundo)
           ("C-#"       . consult-register-load)
           ("C-O"       . crux-smart-open-line-above)
           ("M-#"       . consult-register-store)
           ("C-c #"     . consult-register)
           ("M-o"       . duplicate-dwim)
           ("M-z"       . avy-zap-to-char-dwim)
           ("M-Z"       . avy-zap-up-to-char-dwim)
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
           ("M-g l"     . avy-goto-line)
           ("M-g w"     . avy-goto-word-0)
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
           ("C-c k"     . kill-buffer-and-window)
           ("C-c v"     . magit-file-dispatch)
           ("C-c C-v"   . magit-dispatch)
           ("C-c C-d"   . helpful-at-point)
           ("C-c a"     . org-agenda)
           ("C-c c"     . org-capture)
           ("C-c l"     . org-store-link)
           ("C-c b"     . tabspaces-switch-to-buffer)
           ("C-c d"     . bing-dict-brief)
           ("C-c r"     . query-replace-regexp)
           ("C-c z"     . hs-toggle-hiding)
           ("C-c Z"     . hs-show-all)
           ("C-c f"     . dirvish-fd)
           ("C-c M-f"   . ffap)
           ("C-x a a"   . align)
           ("C-x a r"   . align-regexp)
           ("C-x / /"   . webjump)
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
           ("C-c q a"   . org-ql-find-in-agenda)
           ("C-c q d"   . org-ql-find-in-org-directory)
           ("C-c q s"   . org-ql-search)
           ("C-c q v"   . org-ql-view)
           ("C-h b"     . embark-bindings)
           ("C-h C-m"   . which-key-show-full-major-mode)
           ("C-h B"     . embark-bindings-at-point))

;; %% transient key
(require 'transient)
(transient-bind-q-to-quit)

(transient-define-prefix yx/transient-global-simple ()
  "Global transient for frequently used functions."
  [["]]1"
    ("c" "whitespace-cleanup" whitespace-cleanup)
    ("o" "crux-open-with" crux-open-with)
    ("s" "scratch-buffer" yx/scratch-buffer)
    ("n" "new-empty-buffer" yx/new-empty-buffer)
    ("m" "major-mode-keybings" which-key-show-full-major-mode)
    ("v" "magit-file-dispatch" magit-file-dispatch)
    ("w" "pyim-create-word-from-selection" pyim-create-word-from-selection)
    ("%" "query-replace-regexp" query-replace-regexp)
    ("!" "shell-command" shell-command)]
   ["]]2"
    ("C" "desktop-clear" desktop-clear)
    ("D" "crux-delete-file-and-buffer" crux-delete-file-and-buffer)
    ("V" "magit-dispatch" magit-dispatch)
    ("T" "consult-minor-mode-menu" consult-minor-mode-menu)
    ("R" "rename-visited-file" rename-visited-file)
    ("K" "crux-kill-other-buffers" crux-kill-other-buffers)
    ("E" "crux-sudo-edit" crux-sudo-edit)]
   ["]]3"
    ("t l" "command-log" clm/toggle-command-log-buffer)]])



;;; Ui
(setq use-dialog-box nil
      use-file-dialog nil)

(setq x-stretch-cursor nil
      x-underline-at-descent-line t)

(setq cursor-in-non-selected-windows nil)

(when IS-MAC (menu-bar-mode 1))

(setq project-mode-line t
      which-func-display 'header)

;; %% font
(defvar yx/font-height 140)
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
  (cl-loop for font in '( "LXGW WenKai Mono" "Microsoft Yahei" "PingFang SC")
           when (x-list-fonts font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font))
  (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
           when (x-list-fonts font)
           return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
  (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
           when (x-list-fonts font)
           return (set-fontset-font t 'emoji  (font-spec :family font) nil 'prepend)))

(add-hook 'window-setup-hook 'yx/setup-fonts)
(add-hook 'server-after-make-frame-hook 'yx/setup-fonts)

(setq modus-themes-mixed-fonts t
      modus-themes-variable-pitch-ui t
      modus-themes-italic-constructs t
      modus-themes-common-palette-overrides '((fringe unspecifield)
                                              (fg-line-number-active fg-main)
                                              (bg-line-number-active unspecifield)
                                              (fg-line-number-inactive "gray50")
                                              (bg-line-number-inactive unspecifield)))

(use-package ef-themes
  :init
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t))

(if (display-graphic-p)
    (load-theme 'modus-operandi-tinted t)
  (load-theme 'modus-vivendi-tinted t))

(use-package lin
  :hook (after-init . lin-global-mode)
  :custom
  (lin-face 'lin-magenta))

(use-package minions
  :demand t
  :hook (emacs-startup . minions-mode))

(use-package spacious-padding
  :custom
  (spacious-padding-subtle-mode-line nil)
  (spacious-padding-widths '( :internal-border-width 4
                              :header-line-width 2
                              :mode-line-width 2
                              :tab-width 4
                              :right-divider-width 12
                              :fringe-width 8))
  :init (spacious-padding-mode 1))

(use-package breadcrumb
  :demand t
  :hook (emacs-startup . breadcrumb-mode))

;;; Layout
(use-package popper
  :hook (after-init . popper-mode)
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-echo-lines 1)
  (popper-display-control t)
  (popper-group-function #'popper-group-by-project)
  (popper-reference-buffers '("\\*Messages\\*$"
                              "Output\\*$" "\\*Pp Eval Output\\*$"
                              "^\\*eldoc.*\\*$"
                              "\\*Compile-Log\\*$"
                              "\\*Completions\\*$"
                              "\\*Warnings\\*$"
                              "\\*Async Shell Command\\*$"
                              "\\*Apropos\\*$"
                              "\\*Backtrace\\*$"
                              "^\\*.*eshell.*\\*.*$"
                              "^\\*.*shell.*\\*.*$"
                              "^\\*.*terminal.*\\*.*$"
                              "\\*gud-debug\\*$"
                              "\\*quickrun\\*$"
                              "\\*tldr\\*$" "\\*vc-.*\\**"
                              "\\*diff-hl\\**"
                              "\\*Agenda Commands\\*"
                              "\\*Org Select\\*"
                              "\\*Capture\\*" "^CAPTURE-.*\\.org*"
                              bookmark-bmenu-mode
                              help-mode helpful-mode
                              tabulated-list-mode
                              Buffer-menu-mode
                              list-environment-mode
                              inferior-python-mode
                              comint-mode compilation-mode
                              flymake-diagnostics-buffer-mode
                              devdocs-mode grep-mode occur-mode
                              "^\\*Process List\\*$" process-menu-mode))
  :config
  (defun yx/popper-fit-window-height (win)
    "Determine the height of popup window WIN by fitting it to the buffer's content."
    (fit-window-to-buffer
     win
     (floor (frame-height) 2)
     (floor (frame-height) 4)))
  (setq popper-window-height #'yx/popper-fit-window-height)
  (add-to-list 'display-buffer-alist
               '(("\\`\\(\\*Proced\\*\\|\\*Ibuffer\\|\\*Man\\|\\*WoMan\\|\\*info\\|\\*Org Agenda\\)"
                  (display-buffer-full-frame))))
  (popper-echo-mode +1))

(setq window-sides-vertical nil
      split-width-threshold 60
      even-window-sizes 'height-only
      frame-resize-pixelwise t
      window-resize-pixelwise t
      fit-frame-to-buffer nil
      fit-window-to-buffer-horizontally nil)

(setq winner-dont-bind-my-keys t
      winner-boring-buffers-regexp "^\\*")
(add-hook 'after-init-hook 'winner-mode)
(add-hook 'after-init-hook 'temp-buffer-resize-mode)

(setq buffer-quit-function 'winner-undo)

(setq switch-to-buffer-in-dedicated-window nil
      switch-to-buffer-obey-display-actions t
      switch-to-buffer-preserve-window-point t
      switch-to-prev-buffer-skip 'visible
      switch-to-prev-buffer-skip-regexp "^\\*\\|^magit.*")

(use-package zoom
  :custom
  (zoom-size '(0.618 . 0.618)))

(use-package burly
  :hook (after-init . burly-tabs-mode))

(setq ibuffer-expert t
      ibuffer-display-summary nil
      ibuffer-show-empty-filter-groups nil
      ibuffer-default-sorting-mode 'major-mode)

(defun yx/ibuffer-setup ()
  (hl-line-mode 1)
  (ibuffer-auto-mode 1)
  (ibuffer-do-sort-by-recency))
(add-hook 'ibuffer-mode-hook 'yx/ibuffer-setup)

(use-package ibuffer-vc
  :init
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))

;; %% tabbar
(setq tab-bar-show t
      tab-bar-format '(tab-bar-format-menu-bar
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
    (keymap-unset map "<wheel-down>")))
(add-hook 'after-init-hook #'yx/tab-bar-setup)

(use-package sr-speedbar
  :ensure nil
  :defer 2
  :init
  (setq speedbar-use-images nil
        sr-speedbar-width 30
        sr-speedbar-skip-other-window-p t))

(use-package ace-window
  :init
  (setq aw-scope 'frame
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
  (tabspaces-session-auto-restore nil)
  (tabspaces-session-file (no-littering-expand-var-file-name "tabsession.el")))

;;; Completion
(use-package vertico
  :init
  (setq vertico-resize nil
        vertico-preselect 'directory)
  (vertico-mode 1)
  :bind (:map vertico-map
              ("M-q" . vertico-quick-insert)
              ("M-r" . vertico-repeat-select)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char))
  :hook ((minibuffer-setup-hook vertico-repeat-save)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :config
  (vertico-mouse-mode 1)
  (vertico-indexed-mode 1)
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'vertico-repeat-history)))

(use-package orderless
  :demand t
  :custom
  (orderless-component-separator  #'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
  :config
  (orderless-define-completion-style yx/orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))
                                        (buffer (styles basic partial-completion))
                                        (command (styles yx/orderless-with-initialism))
                                        (symbol (styles yx/orderless-with-initialism))
                                        (variable (styles yx/orderless-with-initialism))
                                        (eglot (styles yx/orderless-with-initialism))
                                        (eglot-capf (styles yx/orderless-with-initialism)))))

;; %% embark
(use-package embark
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
  (defun yx/dired-open-externally (&optional arg)
    "Open marked or current file in operating system's default application."
    (interactive "P")
    (dired-map-over-marks (embark-open-externally (dired-get-filename)) arg))
  (defun yx/consult-outline-insert-heading (target)
    (let* ((marker (plist-get
                    (text-properties-at 0 target)
                    'consult--candidate))
           (headline-name (org-entry-get nil "ITEM")))
      (org-insert-link nil headline-name)))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind (("C-." . embark-act)
         :map dired-mode-map
         ("e" . yx/dired-open-externally)
         :map minibuffer-local-map
         ("C-c C-e" . embark-export)
         ("C-c C-c" . embark-collect)
         ("C-SPC" . (lambda () (interactive) (embark-select) (vertico-next)))
         :map  embark-general-map
         ("h" . yx/consult-outline-insert-heading)))

(use-package embark-consult
  :after embark
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; %% consult
(use-package consult
  :config
  (setq consult-narrow-key "<"
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        consult-ripgrep-args (concat consult-ripgrep-args " --hidden"))
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-bookmark
   consult-recent-file
   consult--source-buffer
   consult--source-recent-file :preview-key "M-.")
  :bind (:map minibuffer-local-map
              ("M-h" . consult-history)))

(use-package consult-dir
  :after consult
  :bind (:map vertico-map
              ("C-x C-d" . consult-dir)
              ("C-x C-j" . consult-dir-jump-file)))

;; %% corfu
(use-package corfu
  :hook ((text-mode prog-mode) . corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-echo-documentation nil)
  :config
  (corfu-echo-mode 1)
  (corfu-history-mode 1)
  (corfu-indexed-mode 1)
  (corfu-popupinfo-mode 1)
  :bind (:map corfu-map
              ("M-q" . corfu-quick-insert)))

(use-package cape
  :init
  (setq cape-dabbrev-min-length 3)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  :bind (("C-c p p" . completion-at-point)
         ("C-c p t" . complete-tag)
         ("C-c p a" . cape-abbrev)
         ("C-c p d" . cape-dabbrev)
         ("C-c p k" . cape-keyword)
         ("C-c p h" . cape-history)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p f" . cape-file)
         ("C-c p /" . cape-tex)
         ("C-c p :" . cape-emoji)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345)))

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;;; Misc
(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB

(when (featurep 'xwidget-internal)
  (use-package xwidget
    :ensure nil
    :bind (:map xwidget-webkit-mode-map
                ("W" . xwidget-webkit-fit-width))))

(use-package engine-mode
  :hook (after-init . engine-mode)
  :custom
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
     ("d" "https://search.douban.com/book/subject_search?search_text=%s"))))

(use-package posframe)

;; %% auxiliary tool
(use-package crux
  :ensure nil
  :defer 2
  :autoload (yx/def-org-maybe-surround)
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-reopen-as-root-mode 1))

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.8)
  (which-key-show-early-on-C-h t)
  :config
  (which-key-setup-side-window-bottom))

(use-package ace-link
  :hook (after-init . ace-link-setup-default))

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
  :init
  (setq avy-style 'at
        avy-timeout-seconds 0.8)
  :bind (:map isearch-mode-map
              ("M-j" . avy-isearch))
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
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(use-package avy-zap)

(use-package hungry-delete
  :hook (after-init . global-hungry-delete-mode)
  :custom
  (hungry-delete-join-reluctantly t))

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(use-package drag-stuff
  :hook ((text-mode prog-mode) . drag-stuff-mode)
  :bind (:map drag-stuff-mode-map
              ("M-J" . drag-stuff-down)
              ("M-K" . drag-stuff-up)
              ("M-H" . drag-stuff-left)
              ("M-L" . drag-stuff-right)))

(use-package pulsar
  :defer 2
  :custom
  (pulsar-delay 0.05)
  (pulsar-iterations 8)
  :config
  (add-hook 'next-error-hook         #'pulsar-pulse-line)
  (add-hook 'minibuffer-setup-hook   #'pulsar-pulse-line)
  (add-hook 'imenu-after-jump-hook   #'pulsar-recenter-center)
  (add-hook 'imenu-after-jump-hook   #'pulsar-reveal-entry)
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-center)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)
  (pulsar-global-mode 1))

(use-package goggles
  :hook ((text-mode prog-mode) . goggles-mode)
  :custom
  (goggles-pulse t))

;; %% erc
(use-package erc
  :ensure nil
  :custom
  (erc-nick "xplutoyz")
  (erc-join-buffer 'bury)
  (erc-interpret-mirc-color t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 15)
  (erc-prompt-for-password nil)
  (erc-prompt-for-nickserv-password nil)
  (erc-use-auth-source-for-nickserv-password t)
  (erc-hide-list '("JOIN" "NICK" "PART" "QUIT"))
  :config
  (add-to-list 'erc-modules 'services)
  (erc-update-modules)
  (erc-services-mode 1))

;; %% emms
(use-package emms
  :hook ((emms-playlist-mode . hl-line-mode))
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
  (emms-history-load))

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
  (setq default-input-method "pyim"
        pyim-outcome-trigger  nil
        pyim-enable-shortcode nil
        pyim-punctuation-dict nil
        pyim-page-tooltip 'posframe
        pyim-dcache-backend 'pyim-dregcach
        pyim-indicator-list '(pyim-indicator-with-modeline)
        pyim-dicts '((:name "big-dict" :file "~/.emacs.d/.cache/pyim-bigdict.pyim"))
        pyim-english-input-switch-functions '(pyim-probe-auto-english
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
  (pyim-tsinghua-dict-enable))

(use-package stardict
  :ensure nil
  :bind (("M-s d" . stardict-define-at-point)
         ("M-s M-d" . stardict-define))
  :init
  (setq stardict-name "langdao-ec-gb"
        stardict-dir "~/.config/stardict/dic/stardict-langdao-ec-gb-2.4.2"))

(use-package bing-dict
  :init
  (setq bing-dict-vocabulary-save t
        bing-dict-cache-auto-save nil
        bing-dict-show-thesaurus 'synonym
        bing-dict-vocabulary-file (no-littering-expand-var-file-name "bing-dict-vocabulary.org"))
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
  (add-hook 'eww-mode-hook 'bing-dict-eldoc-mode)
  (add-hook 'Info-mode-hook 'bing-dict-eldoc-mode)
  (add-hook 'elfeed-show-hook 'bing-dict-eldoc-mode))

(use-package cal-china-x
  :defer 2
  :config
  (setq mark-holidays-in-calendar t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
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
  (dired-guess-shell-alist-user
   `(("\\.\\(?:docx\\|pdf\\|djvu\\gif)\\'" ,yx/default-open)))
  :config
  (defun yx/dired-setup ()
    (setq
     dired-omit-files
     (concat dired-omit-files "\\|^\\..*$"))
    (hl-line-mode 1)
    (dired-omit-mode 1)
    (dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook 'yx/dired-setup)
  (add-hook 'wdired-mode-hook 'highlight-changes-mode)
  (put 'dired-find-alternate-file 'disabled nil))

;; %% dired+
(use-package diredfl
  :hook ((dired-mode . diredfl-mode)
         (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package zoxide)

(use-package dirvish
  :hook (after-init . dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"           "Home")
     ("d" "~/yxdocs/"    "yxdocs")
     ("m" "/mnt/"        "Drives")
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
        ("M-j" . dirvish-fd-jump)))

;;; Gnus
(setq gnus-home-directory "~/.gnus.d/"
      gnus-default-directory gnus-home-directory
      mm-default-directory gnus-home-directory
      message-directory (expand-file-name "Mail/" gnus-home-directory))

(setq read-mail-command 'gnus
      message-confirm-send t
      message-from-style 'angles
      message-kill-buffer-on-exit t
      mail-user-agent 'gnus-user-agent
      mail-envelope-from 'header
      mail-specify-envelope-from t
      sendmail-program "/usr/local/bin/msmtp"
      send-mail-function 'message-send-mail-with-sendmail
      message-send-mail-function 'message-send-mail-with-sendmail)

(setq mm-inline-large-images t)

(setq mml-default-sign-method "pgpmime"
      mml-secure-openpgp-sign-with-sender t)

(use-package gnus
  :commands (gnus)
  :ensure nil
  :config
  (setq mail-sources
        '((imap
           :server "imap.qq.com"
           :port 993
           :user "yangxue.cs@foxmail.com"
           :mailbox "INBOX"
           :fetchflag "\\Seen"
           :stream tls
           :dontexpunge t)))
  (setq gnus-select-method '(nnnil "")
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
           (nnimap-split-methods default))))
  (setq nnmail-expiry-wait '30
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
          "INBOX.Misc"))

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
        gnus-use-dribble-file t
        gnus-always-read-dribble-file t
        gnus-message-archive-group nil
        gnus-inhibit-user-auto-expire t
        gnus-search-use-parsed-queries t
        gnus-article-browse-delete-temp t
        gnus-check-new-newsgroups 'ask-server
        gnus-mime-display-multipart-related-as-mixed t
        gnus-subscribe-newsgroup-method 'gnus-subscribe-zombies)

  (setq gnus-cache-remove-articles '(read)
        gnus-cacheable-groups "^\\(nntp\\|nnimap\\)"
        gnus-cache-enter-articles '(ticked dormant unread))

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
        gnus-summary-gather-subject-limit 'fuzzy
        gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
        gnus-simplify-subject-functions '(gnus-simplify-subject-re gnus-simplify-whitespace))

  (setq gnus-use-trees t
        gnus-show-threads t
        gnus-fetch-old-headers t
        gnus-tree-minimize-window t
        gnus-build-sparse-threads 'some
        gnus-fetch-old-ephemeral-headers 2
        gnus-thread-indent-level 2
        gnus-thread-hide-subtree nil
        gnus-generate-tree-function #'gnus-generate-horizontal-tree
        gnus-thread-sort-functions '(gnus-thread-sort-by-subject
                                     gnus-thread-sort-by-most-recent-number))

  (setq gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
        gnus-subthread-sort-functions '(gnus-thread-sort-by-date))

  (setq gnus-view-pseudos 'automatic
        gnus-view-pseudos-separately t
        gnus-view-pseudo-asynchronously t)

  (setq gnus-auto-select-first t
        gnus-auto-select-next nil
        gnus-paging-select-next nil)

  (setq gnus-group-sort-function '(gnus-group-sort-by-method)
        gnus-group-line-format "%M%S%p%P %0{%5y%} %B%{%G%}\n")

  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode))

;;; Shell
(setq comint-input-ignoredups t
      comint-prompt-read-only t
      comint-completion-autolist t
      comint-completion-addsuffix t
      comint-buffer-maximum-size 9999
      comint-scroll-to-bottom-on-input t
      comint-scroll-show-maximum-output t
      comint-scroll-to-bottom-on-output nil)

(setq shell-kill-buffer-on-exit t
      shell-highlight-undef-enable t
      shell-command-prompt-show-cwd t)

(use-package eshell
  :bind (:map eshell-mode-map
              ("C-l" . yx/eshell-clear)
              ("M-h" . consult-history))
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

  :init
  (defun yx/eshell-here ()
    (interactive)
    (let* ((project (project-current))
           (name (if project (concat "-" (project-name project)) ""))
           (dir (if (buffer-file-name)
                    (file-name-directory (buffer-file-name))
                  default-directory))
           (height (/ (frame-height) 3)))
      (setq eshell-buffer-name (concat "*eshell" name "*"))
      (eshell)
      (yx/eshell-clear)
      (eshell/cd dir)))

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
    (set-window-fringes nil 0 0)
    (set-window-margins nil 1 nil)
    (visual-line-mode +1)
    (add-to-list 'completion-at-point-functions 'cape-elisp-symbol)
    (add-to-list 'completion-at-point-functions 'cape-file)
    (setq-local corfu-auto nil)
    (corfu-mode 1))

  (add-hook 'eshell-mode-hook #'yx/eshell-setup)

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
  :vc (:url "https://codeberg.org/akib/emacs-eat" :rev :newest)
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

;;; Org
(use-package org
  :ensure nil
  :defer 2
  :bind
  (:map org-mode-map
        ("RET"     . yx/org-return-dwim)
        ("M-g h"   . consult-org-heading)
        ("C-c M-y" . yx/org-link-copy)
        ("C-c M-i" . org-web-tools-insert-link-for-url)
        ("C-c t h" . org-toggle-heading)
        ("C-c t l" . org-toggle-link-display)
        ("C-c t v" . yx/org-display-subtree-inline-images)
        ("C-x n h" . yx/org-show-current-heading-tidily)
        :repeat-map org-heading-navigate-repeat-map
        ("u" . outline-up-heading)
        ("p" . org-previous-visible-heading)
        ("n" . org-next-visible-heading)
        ("f" . org-forward-heading-same-level)
        ("b" . org-backward-heading-same-level))
  :autoload (org-calendar-holiday)
  :hook
  (org-mode . yx/org-mode-setup)
  (org-agenda-finalize . yx/org-agenda-finalize-setup)
  (org-babel-after-execute . yx/org-babel-display-image)
  :custom
  (org-directory yx/org-root)
  (org-ellipsis nil)
  (org-num-max-level 2)
  (org-reverse-note-order t)
  (org-list-allow-alphabetical t)
  (org-return-follows-link t)
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

  (org-lowest-priority ?D)
  (org-priority-default ?C)

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

  (org-footnote-auto-adjust nil)

  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-confirm-babel-evaluate nil)
  (org-src-preserve-indentation nil)
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

  (org-default-notes-file (expand-file-name "inbox.org" org-directory))

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
   (expand-file-name "diary.org" yx/org-root))
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
  (org-cite-global-bibliography (list (expand-file-name "bibliography.bib" yx/org-root)))
  (org-cite-export-processors '((latex biblatex)
                                (t . (csl "ieee.csl"))))

  (org-attach-id-dir (expand-file-name "data/" yx/org-root))
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

  :config
  (add-hook 'org-trigger-hook #'save-buffer)
  (plist-put org-format-latex-options :scale 1.5)
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
  (yx/def-org-maybe-surround "~" "=" "*" "/" "+")

  (font-lock-add-keywords
   'org-mode
   '(("\\(\\(?:\\\\\\(?:label\\|ref\\|eqref\\)\\)\\){\\(.+?\\)}"
      (1 font-lock-keyword-face)
      (2 font-lock-constant-face))))

  (add-hook 'org-ctrl-c-ctrl-c-hook 'yx/check-latex-fragment)

  (defun yx/org-reformat-buffer ()
    (interactive)
    (when (y-or-n-p "Really format current buffer? ")
      (let ((document (org-element-interpret-data (org-element-parse-buffer))))
        (erase-buffer)
        (insert document)
        (goto-char (point-min)))))

  :preface
  (defun yx/org-mode-setup ()
    (auto-fill-mode -1)
    (variable-pitch-mode 1)
    (push 'cape-tex completion-at-point-functions)
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table))

  (defun yx/check-latex-fragment ()
    (let ((datum (org-element-context)))
      (when (memq (org-element-type datum) '(latex-environment latex-fragment))
        (org-latex-preview)
        t)))

  (defun yx/org-agenda-finalize-setup ()
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt))

  (defun yx/org-babel-display-image ()
    (when org-inline-image-overlays
      (org-redisplay-inline-images)))

  (defun yx/org-agenda-format-date-aligned (date)
    "Format a DATE string for display in the daily/weekly agenda, or timeline.
  This function makes sure that dates are aligned for easy reading."
    (require 'cal-china-x)
    (let* ((dayname (aref cal-china-x-days
                          (calendar-day-of-week date)))
           (day (cadr date))
           (month (car date))
           (year (nth 2 date))
           (cn-date (calendar-chinese-from-absolute (calendar-absolute-from-gregorian date)))
           (cn-month (cl-caddr cn-date))
           (cn-day (cl-cadddr cn-date))
           (cn-month-string (concat (aref cal-china-x-month-name
                                          (1- (floor cn-month)))
                                    (if (integerp cn-month)
                                        ""
                                      "[闰]")))
           (cn-day-string (aref cal-china-x-day-name
                                (1- cn-day))))
      (format "%04d-%02d-%02d 周%-8s 农历%s%s" year month
              day dayname cn-month-string cn-day-string))))

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
  (org-preview-latex-image-directory (no-littering-expand-var-file-name "ltximg/"))
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
                 :anything))))

(use-package org-modern
  :after org
  :demand t
  :custom-face
  (org-modern-block-name ((t (:height 0.9))))
  :config
  (setq org-modern-block-fringe 0)
  (global-org-modern-mode 1))

(use-package valign
  :hook (org-mode . valign-mode)
  :custom
  (valign-fancy-bar 1))

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode))

(use-package org-download
  :after org
  :hook (dired-mode . org-download-enable)
  :commands (org-download-screenshot org-download-clipboard)
  :custom
  (org-download-heading-lvl nil)
  (org-download-screenshot-method "screencapture -i %s")
  (org-download-image-dir
   (expand-file-name (concat org-attach-directory "images/") yx/org-root))
  :bind (:map org-mode-map
              ("C-c y" . org-download-screenshot)
              ("C-c C-y" . org-download-clipboard)))

(use-package org-web-tools)

;;; Reading
(setq
 doc-view-continuous t
 doc-view-resolution 300)

(use-package pdf-tools
  :hook (pdf-tools-enabled . pdf-isearch-minor-mode)
  :init
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (setq-default pdf-view-display-size 'fit-width)
  (pdf-loader-install))

;; %% olivetti
(use-package olivetti
  :hook ((org-mode org-agenda-mode) . olivetti-mode)
  :init
  (setq olivetti-style nil
        olivetti-mode-map nil
        olivetti-body-width 0.66
        olivetti-minimum-body-width (+ fill-column 2)))

;; %% elfeed
(use-package elfeed
  :bind (:map elfeed-show-mode-map
              ("w" . elfeed-show-yank)
              ("%" . elfeed-webkit-toggle)
              ("q" . yx/elfeed-kill-entry)
              :map elfeed-search-mode-map
              ("R" . yx/elfeed-mark-all-as-read))
  :init
  (setq elfeed-feeds
        '(("http://www.zhihu.com/rss" new)
          ("https://www.inference.vc/rss" ai)
          ("https://spaces.ac.cn/feed" ai webkit)
          ("https://ruder.io/rss/index.rss" ai)
          ("https://lilianweng.github.io/index.xml" ai webkit)
          ("https://www.juliabloggers.com/feed/" julia)
          ("https://planet.lisp.org/rss20.xml" lisp)
          ("https://planet.scheme.org/atom.xml" scheme)
          ("https://planet.haskell.org/rss20.xml" haskell)
          ("https://planet.emacslife.com/atom.xml" emacs)
          ("http://wingolog.org/feed/atom" lang)
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
  (setq elfeed-search-filter "@6-months-ago +unread"
        elfeed-search-print-entry-function 'yx/elfeed-search-print-entry--better-default)
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
  (defun yx/elfeed-mark-all-as-read ()
    "Mark all feeds in buffer as read."
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))
  (defun yx/elfeed-search-print-entry--better-default (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (date-width (car (cdr elfeed-search-date-format)))
           (title (concat (or (elfeed-meta entry :title)
                              (elfeed-entry-title entry) "")
                          " "))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title (when feed (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (mapconcat (lambda (s) (propertize s 'face 'elfeed-search-tag-face)) tags ","))
           (title-width (- (frame-width)
                           ;; (window-width (get-buffer-window (elfeed-search-buffer) t))
                           date-width elfeed-search-trailing-width))
           (title-column (elfeed-format-column
                          title (elfeed-clamp
                                 elfeed-search-title-min-width
                                 title-width
                                 elfeed-search-title-max-width) :left))
           (align-to-feed-pixel (+ date-width
                                   (max elfeed-search-title-min-width
                                        (min title-width elfeed-search-title-max-width)))))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")
      (insert (propertize title-column 'face title-faces 'kbd-help title))
      (put-text-property (1- (point)) (point) 'display `(space :align-to ,align-to-feed-pixel))
      (when feed-title (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
      (when tags (insert "(" tags-str ")")))))

(use-package elfeed-webkit
  :after elfeed
  :bind (:map elfeed-webkit-map
              ("q" . yx/elfeed-kill-entry))
  :config
  (elfeed-webkit-auto-toggle-by-tag))


;;; Writing
(use-package denote
  :after org
  :demand t
  :custom
  (denote-directory yx/org-root)
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
(setq bibtex-dialect 'biblatex
      bibtex-align-at-equal-sign t)

;; %% latex
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :hook ((LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . electric-pair-local-mode)
         (LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . TeX-fold-mode)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTex-mode . (lambda ()
                         (push #'cape-tex completion-at-point-functions))))
  :config
  (setq-default Tex-master nil)
  (setq-default TeX-engine 'xetex)

  (setq TeX-auto-save t
        TeX-save-query nil
        TeX-parse-self t
        TeX-source-correlate-start-server t
        TeX-view-program-selection '((output-pdf "PDF Tools")))

  (setq reftex-plug-into-AUCTeX t)

  (add-hook 'TeX-after-comilation-finished-functions 'TeX-revert-document-buffer))

(use-package cdlatex
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (org-mode   . turn-on-org-cdlatex)))

(use-package transform
  :ensure nil
  :commands (transform-greek-help
             transform-previous-char))

;; %% citar
(use-package citar
  :after org
  :demand t
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-notes-paths `(,yx/org-root))
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
(defun yx/prog-common-setup ()
  (setq-local line-spacing 0.15)
  (hl-line-mode 1)
  (hs-minor-mode 1)
  (superword-mode 1)
  (show-paren-mode 1)
  (electric-pair-local-mode 1)
  (electric-indent-local-mode 1)
  (electric-layout-local-mode 1)
  (keymap-local-set "RET" 'newline-and-indent))

(add-hook 'prog-mode-hook 'yx/prog-common-setup)

;; project
(setq project-file-history-behavior 'relativize
      project-vc-extra-root-markers '(".envrc" "pyproject.toml"))

;; diff
(setq diff-default-read-only t
      diff-update-on-the-fly t
      ediff-show-clashes-only t
      ediff-floating-control-frame t
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

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
  :bind (("C-x c l" . flymake-start)
         :map flymake-mode-map
         ("M-g d"   . flymake-show-buffer-diagnostics)
         ("M-g M-d" . flymake-show-project-diagnostics)
         ("M-g M-n" . flymake-goto-next-error)
         ("M-g M-p" . flymake-goto-prev-error)
         :repeat-map flymake-repeatmap
         ("p" . flymake-goto-prev-error)
         ("n" . flymake-goto-next-error))
  :custom
  (flymake-no-changes-timeout nil)
  (flymake-show-diagnostics-at-end-of-line t)
  (flymake-fringe-indicator-position 'right-fring)
  :config
  (advice-add #'elisp-flymake-byte-compile :around
              (defun yx/elisp-flymake-byte-compile-around (oldfun &rest args)
                (let ((elisp-flymake-byte-compile-load-path
                       (cons "./" load-path)))
                  (apply oldfun args)))))

(use-package apheleia
  :defer 2
  :config (apheleia-global-mode +1))

(use-package ws-butler
  :hook ((prog-mode conf-mode) . ws-butler-mode))

;; %% snippet
(use-package tempel
  :defer 2
  :hook ((prog-mode text-mode) . yx/tempel-setup-capf)
  :custom
  (tempel-trigger-prefix "<")
  (tempel-path (no-littering-expand-etc-file-name "templates/tempel.eld"))
  :bind (("M-+" . tempel-insert)
         ("M-=" . tempel-complete)
         :map tempel-map
         ([tab] . tempel-next)
         ([backtab] . tempel-previous))
  :preface
  (defun yx/tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons 'tempel-expand
                      completion-at-point-functions))))

;; %% version control
(use-package magit
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-show-long-lines-warning nil)
  (magit-log-arguments '("--color" "--graph" "--decorate"))
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :bind (:map magit-status-mode-map
              ("q" . #'yx/magit-kill-buffers))
  :preface
  (defun yx/magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (magit-restore-window-configuration)
    (mapc #'kill-buffer (magit-mode-get-buffers))))

(use-package diff-hl
  :hook
  (after-init . global-diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :custom
  (diff-hl-disable-on-remote t)
  (diff-hl-show-staged-changes t)
  :config
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

(use-package isayt
  :vc (:url https://gitlab.com/andreyorst/isayt.el :rev :newest)
  :hook ((emacs-lisp-mode scheme-mode-hook) . isayt-mode))

;; %% doc
(use-package devdocs
  :bind (:map prog-mode-map
              ("C-x c d" . devdocs-lookup)))

(add-hook 'julia-ts-mode-hook
          (lambda () (setq-local devdocs-current-docs '("julia~1.9"))))
(add-hook 'python-base-mode-hook
          (lambda () (setq-local devdocs-current-docs '("python~3.12" "pytorch" "numpy~1.23"))))

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
  :init
  (setq treesit-font-lock-level 4
        treesit-language-source-alist
        '((c      "https://github.com/tree-sitter/tree-sitter-c")
          (cpp    "https://github.com/tree-sitter/tree-sitter-cpp")
          (lua    "https://github.com/MunifTanjim/tree-sitter-lua")
          (org    "https://github.com/milisims/tree-sitter-org")
          (bash   "https://github.com/tree-sitter/tree-sitter-bash")
          (julia  "https://github.com/tree-sitter/tree-sitter-julia")
          (python "https://github.com/tree-sitter/tree-sitter-python"))
        treesit-load-name-override-list '((c++ "libtree-sitter-cpp"))
        treesit-extra-load-path (list (no-littering-expand-var-file-name "tree-sitter")))
  (defun yx/treesit--install-language-grammar (lang-pair)
    (let ((lang (car lang-pair)))
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang (car treesit-extra-load-path)))))
  (mapc 'yx/treesit--install-language-grammar treesit-language-source-alist)
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

;; %% refoctor
(use-package color-rg
  :vc (:url https://github.com/manateelazycat/color-rg :rev :newest)
  :defer 2
  :custom
  (color-rg-search-no-ignore-file nil)
  (color-rg-mac-load-path-from-shell nil))

;; %% structured edit
(use-package iedit)

(use-package surround
  :defer 2
  :bind-keymap ("M-0" . surround-keymap))

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

;; %% lsp
(use-package eglot
  :ensure nil
  :hook ((R-mode
          LaTeX-mode
          haskell-mode
          c-mode c-ts-mode
          python-mode python-ts-mode
          julia-mode julia-ts-mode) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-sync-connect nil)
  (eglot-report-progress nil)
  (eglot-events-buffer-size 0)
  (eglot-send-changes-idle-time 0.3)
  :bind (:map eglot-mode-map
              ("C-x c r" . eglot-rename)
              ("C-x c f" . eglot-format)
              ("C-x c a" . eglot-code-actions)
              ("C-x c g" . consult-eglot-symbols))
  :config
  (fset #'jsonrpc--log-event #'ignore) ; massive perf boost---don't log every event
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (defun yx/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'tempel-expand
                       #'cape-file))))
  (add-hook 'eglot-managed-mode-hook #'yx/eglot-capf))

(use-package consult-eglot
  :after consult)

(use-package citre
  :bind (:map prog-mode-map
              ("M-]"       . citre-jump)
              ("M-]"       . citre-jump-back)
              ("C-x c ."   . citre-jump)
              ("C-x c ,"   . citre-jump-back)
              ("C-x c p"   . citre-peek)
              ("C-x c u"   . citre-update-this-tags-file))
  :custom
  (citre-prompt-language-for-ctags-command t)
  (citre-use-project-root-when-creating-tags t)
  (citre-default-create-tags-file-location 'global-cache)
  (citre-auto-enable-citre-mode-modes '(c-ts-mode python-ts-mode))
  :config
  (with-eval-after-load 'cc-mode (require 'citre-lang-c))
  (with-eval-after-load 'dired (require 'citre-lang-fileref)))

(use-package dape
  :init
  (setq dape-adapter-dir (no-littering-expand-var-file-name "dape-debug-adapters")
        dape-buffer-window-arrangment 'right))

(use-package quickrun
  :custom
  (quickrun-focus-p nil))

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

(define-auto-insert "\\.el$" 'yx/auto-insert-el-header)

;; %% c/c++
(setq c-basic-offset 8
      c-default-style 'linux
      c-ts-mode-indent-offset 8
      c-ts-mode-indent-style 'linux)

(add-hook 'c-mode-common-hook
          (setq-local indent-tabs-mode nil)
          (lambda () (c-toggle-auto-hungry-state 1)))

(define-auto-insert
  "\\.\\([Hh]\\|hh\\|hpp\\|hxx\\|h\\+\\+\\)\\'"
  'yx/auto-insert-h-header)
(define-auto-insert
  "\\.\\([Cc]\\|cc\\|cpp\\|cxx\\|c\\+\\+\\)\\'"
  'yx/auto-insert-c-header)

;; %% code-cell
(use-package code-cells
  :hook ((julia-mode python-ts-mode) . code-cells-mode-maybe)
  :config
  (setq code-cells-eval-region-commands
        '((python-ts-mode . python-shell-send-region)
          (emacs-lisp-mode . eval-region)))
  (let ((map code-cells-mode-map))
    (keymap-set map "C-c C-n" 'code-cells-forward-cell)
    (keymap-set map "C-c C-p" 'code-cells-backward-cell)
    (keymap-set map "n" (code-cells-speed-key 'code-cells-forward-cell))
    (keymap-set map "p" (code-cells-speed-key 'code-cells-backward-cell))
    (keymap-set map "e" (code-cells-speed-key 'code-cells-eval))
    (keymap-set map "TAB" (code-cells-speed-key 'outline-cycle)))
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
                imenu-create-index-function 'python-imenu-create-flat-index)
    (flymake-mode 1))
  (add-hook 'python-base-mode-hook 'yx/python-mode-setup)

  (define-auto-insert "\\.py$" 'yx/auto-insert-common-header))

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
  :init
  (setq ess-eval-visibly-p 'nowait
        ess-local-process-name "R"
        ess-ask-for-ess-directory nil)
  :config
  (keymap-set ess-r-mode-map ";" 'ess-insert-assign)
  (keymap-set inferior-ess-r-mode-map ";" 'ess-insert-assign))

(use-package julia-mode)
(use-package julia-ts-mode
  :mode "\\.jl$")

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

(define-auto-insert "\\.R$" 'yx/auto-insert-common-header)
(define-auto-insert "\\.jl$" 'yx/auto-insert-common-header)

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

(load custom-file t)
;;; init.el ends here
