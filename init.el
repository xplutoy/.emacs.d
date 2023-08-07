;;; init.el --- emacs init.el. -*- coding: utf-8; lexical-binding: t; -*-
(setq user-full-name "yangxue")
(setq user-mail-address "yangxue.cs@foxmail.com")

(defvar yx/org-dir "~/dotdocs/yx-slip-notes/")
(defvar yx/etc-dir "~/.emacs.d/etc/")
(defvar yx/var-dir "~/.emacs.d/.cache/")
(defvar yx/gpg-sign-key "67B86CB8A5630C51!")
(defvar yx/gpg-encrypt-key "8B1F9B207AF00BCF!")

(defconst -is-mac (eq system-type 'darwin))
(defconst -is-win (eq system-type 'windows-nt))
(defconst -is-linux (eq system-type 'gnu/linux))

(defconst -os-default-opener
  (cond
   (-is-mac "open")
   (-is-win "start")
   (t "xdg-open"))
  )

(menu-bar-mode -1)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(setq custom-file (expand-file-name "custom.el" yx/etc-dir))
(load custom-file 'noerror)
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(require 'use-package)
(setq
 package-archives
 '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
   ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
   ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
 package-quickstart nil
 use-package-always-defer t
 use-package-always-ensure t
 use-package-expand-minimally t
 package-user-dir (expand-file-name "elpa" yx/var-dir)
 package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))
(package-initialize)

(use-package benchmark-init)
(benchmark-init/activate)
(add-hook 'after-init-hook 'benchmark-init/deactivate)

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

;;; init.el ends here
