;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:13:09
;; Modified: <2023-11-28 07:46:59 yx>
;; Licence: GPLv3

;;; Commentary:

;; init

;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; core
(require 'init-package)
(require 'init-helper)
(require 'init-basic)
(require 'init-keymaps)

;; editor
(require 'init-ui)
(require 'init-completion)
(require 'init-misc)
(require 'init-dired)
(require 'init-window)
(require 'init-mail)
(require 'init-terminal)
(require 'init-org)
(require 'init-reader)
(require 'init-writer)

;; lang
(require 'init-ide)
(require 'init-lang)
(require 'init-python)
(require 'init-julia)


;;; init.el ends here
