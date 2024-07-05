;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-09 03:37:21
;; Modified: <2024-07-05 10:25:42 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(require 'package)
(require 'use-package)

(setq package-quickstart nil
      package-native-compile t
      package-install-upgrade-built-in t)

(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-expand-minimally t
      use-package-vc-prefer-newest t
      use-package-enable-imenu-support t)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(use-package benchmark-init
  :init
  (benchmark-init/activate)
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package no-littering
  :demand t
  :config
  (defalias 'nol-expand-etc #'no-littering-expand-etc-file-name)
  (defalias 'nol-expand-var #'no-littering-expand-var-file-name)
  (no-littering-theme-backups))

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/modules")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(require 'x-common)

(defvar yx/org-dir "~/org-notes/")
(defvar yx/zotero-dir "~/Zotero/")
(defvar yx/code-dir (cond (IS-WIN "D:/Codes/") (t "~/Codes")))

(x-common-collect-autoloads "~/.emacs.d/lisp"
                            (nol-expand-var "lisp-autoload.el"))


(require 'y-ui)
(require 'y-os)
(require 'y-keymap)
(require 'y-default)
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
