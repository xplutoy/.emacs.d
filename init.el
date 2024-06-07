;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:13:09
;; Modified: <2024-06-07 15:17:48 yangx>
;; Licence: GPLv3

;;; Init
(defvar yx/etc-dir "~/.emacs.d/etc/")
(defvar yx/var-dir "~/.emacs.d/var/")
(defvar yx/lib-dir "~/.emacs.d/lisp/")
(defvar yx/org-dir "~/yxdocs/org-notes/")
(defvar yx/zotero-root "~/Zotero/")

(add-to-list 'load-path yx/lib-dir)

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'load-path
             (expand-file-name "modules" user-emacs-directory))

(add-to-list 'load-path
             (expand-file-name "site-lisp" user-emacs-directory))


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

(require 'common-x)

(defun yx/collect-lib-autoloads ()
  (unless (string= (file-name-nondirectory (yx/latest-file yx/lib-dir))
                   "lisp-autoloads.el")
    (require 'loaddefs-gen nil t)
    (loaddefs-generate yx/lib-dir
                       (expand-file-name "lisp-autoloads.el" yx/lib-dir)
                       nil nil nil t))
  (load (expand-file-name "lisp-autoloads.el" yx/lib-dir) nil t))

(yx/collect-lib-autoloads)

(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

(require 'y-ui)
(require 'y-os)
(require 'y-keymaps)
(require 'y-essentials)
(require 'y-completion)
(require 'y-edit)
(require 'y-misc)
(require 'y-layout)
(require 'y-dired)
(require 'y-llm)
(require 'y-org)
(require 'y-reading)
(require 'y-writing)
(require 'y-develop)
(require 'y-shell)
(require 'y-web)
(require 'y-mail)

(load custom-file t)
;;; init.el ends here
