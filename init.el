;;; init.el --- init  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:13:09
;; Modified: <2023-08-29 12:21:09 yx>
;; Licence: GPLv3

;;; Commentary:

;; init

;;; Code:

;; %%
(setq user-full-name "yangxue")
(setq user-mail-address "yangxue.cs@foxmail.com")

(defvar yx/org-dir "~/yxdocs/yx-slip-notes/")
(defvar yx/etc-dir "~/.emacs.d/etc/")
(defvar yx/var-dir "~/.emacs.d/.cache/")
(defvar yx/zotero-dir "~/Zotero/")
(defvar yx/gpg-sign-key "67B86CB8A5630C51!")
(defvar yx/gpg-encrypt-key "8B1F9B207AF00BCF!")

(defconst -is-mac   (eq system-type 'darwin))
(defconst -is-win   (eq system-type 'windows-nt))
(defconst -is-linux (eq system-type 'gnu/linux))

(defvar yx/default-open-program
  (cond (-is-win "start")
        (-is-mac "open")
        (-is-linux "xdg-open")
        (t ""))
  )


(menu-bar-mode -1)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(setq custom-file (expand-file-name "custom.el" yx/etc-dir))
(load custom-file 'noerror)
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-help)

;; %% package init
(require 'package)
(setq
 package-archives
 '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
   ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
   ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
 package-quickstart nil
 use-package-always-defer t
 use-package-always-ensure t
 use-package-expand-minimally t
 package-install-upgrade-built-in nil
 package-user-dir (expand-file-name "elpa" yx/var-dir)
 package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))
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
        no-littering-etc-directory yx/etc-dir
        )
  :config
  (no-littering-theme-backups)
  )

;; %% submodule
(require 'init-ui)
(require 'init-base)
(require 'init-comp)
(require 'init-evil)
(require 'init-misc)
(require 'init-wind)
(require 'init-term)
(require 'init-note)
(require 'init-lang)
(require 'init-mail)
(require 'init-read)
(require 'init-temp)

;;; init.el ends here
