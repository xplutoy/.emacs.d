;;; lang-julia.el --- julia  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-09-15 22:08:56
;; Modified: <2023-09-15 22:09:08 yx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(use-package julia-mode)
(use-package julia-ts-mode
  :mode "\\.jl$")

(use-package eglot-jl
  :init
  (with-eval-after-load 'eglot
    (eglot-jl-init))
  )

(define-auto-insert "\\.jl$" 'yx/auto-insert-common-header)

(use-package julia-snail
  :custom
  (julia-snail-terminal-type :eat)
  (julia-snail-extensions '(ob-julia formatter))
  :hook
  (julia-mode . julia-snail-mode)
  (julia-ts-mode . julia-snail-mode)
  )

(provide 'lang-julia)
;;; lang-julia.el ends here
