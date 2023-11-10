;;; core-package.el --- package init  -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-09-15 19:52:43
;; Modified: <2023-10-28 13:08:52 yx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(defvar yx/etc-dir         "~/.emacs.d/etc/")
(defvar yx/var-dir         "~/.emacs.d/.cache/")

(require 'package)
(setq
 package-archives
 '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
   ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
   ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
(setq
 package-quickstart nil
 use-package-always-defer t
 use-package-always-ensure t
 use-package-expand-minimally t
 use-package-enable-imenu-support t
 package-install-upgrade-built-in nil
 package-user-dir (expand-file-name "elpa" yx/var-dir)
 package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))
(if (daemonp)
    (setq use-package-always-demand t))

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


(provide 'core-package)
;;; core-package.el ends here
