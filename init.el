;;; init.el --- init  -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:13:09
;; Modified: <2023-10-28 13:10:48 yx>
;; Licence: GPLv3

;;; Commentary:

;; init

;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'core-package)
(require 'core-helper)
(require 'core-config)
(require 'core-keymaps)

(require 'editor-ui)
(require 'editor-completion)
(require 'editor-misc)
(require 'editor-dired)
(require 'editor-window)
(require 'editor-mail)
(require 'editor-terminal)
(require 'editor-reader)
(require 'editor-writer)
(require 'editor-ide)

(require 'lang-org)
(require 'lang-misc)
(require 'lang-python)
(require 'lang-julia)

;;; init.el ends here
