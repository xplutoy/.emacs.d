;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:13:09
;; Modified: <2024-01-06 06:46:30 yx>
;; Licence: GPLv3

;;; Commentary:

;; init

;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; core
(require 'init-package)
(require 'init-helper)
(require 'init-basic)
(require 'init-keymaps)

;; editor
(require 'init-ui)
(require 'init-completion)
;; (require 'init-evil)
(require 'init-misc)
(require 'init-dired)
(require 'init-layout)
(require 'init-mail)
(require 'init-terminal)
(require 'init-org)
(require 'init-reading)
(require 'init-writing)

;; lang
(require 'init-ide)
(require 'init-lang)


;;; init.el ends here
