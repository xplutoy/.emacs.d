;;; lang-misc.el --- ide  -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 22:57:16
;; Modified: <2023-11-27 10:05:10 yx>
;; Licence: GPLv3

;;; Commentary:

;; ide

;;; Code:
(defun yx/prog-common-setup ()
  (subword-mode 1)
  (hl-line-mode 1)
  (hs-minor-mode 1)
  (semantic-mode 1)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (display-line-numbers-mode 1)
  (electric-indent-local-mode 1)
  (setq-local
   whitespace-style
   '(face trailing lines-char space-before-tab space-after-tab)
   require-final-newline t
   show-trailing-whitespace t)
  (whitespace-mode 1)
  (local-set-key (kbd "RET") 'newline-and-indent)
  )

(add-hook 'prog-mode-hook 'yx/prog-common-setup)

;; %% emacs-lisp
(define-auto-insert "\\.el$" 'yx/auto-insert-el-header)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (prettify-symbols-mode)
            (treesit-parser-create 'elisp)))

;; %% c/c++
(setq
 c-basic-offset 8
 c-default-style
 '((java-mode . "java")
   (awk-mode  . "awk")
   (other     . "linux"))
 )

(setq
 c-ts-mode-indent-offset 8
 c-ts-mode-indent-style 'linux)

(add-hook 'c-mode-common-hook
          (lambda () (c-toggle-auto-hungry-state 1)))

(define-auto-insert
  "\\.\\([Hh]\\|hh\\|hpp\\|hxx\\|h\\+\\+\\)\\'"
  'yx/auto-insert-h-header)
(define-auto-insert
  "\\.\\([Cc]\\|cc\\|cpp\\|cxx\\|c\\+\\+\\)\\'"
  'yx/auto-insert-c-header)

;; %% code-cell
(use-package code-cells
  :hook ((julia-mode
          python-ts-mode
          emacs-lisp-mode) . code-cells-mode-maybe)
  :bind (:map code-cells-mode-map
              ("M-p"     . code-cells-backward-cell)
              ("M-n"     . code-cells-forward-cell))
  :config
  (setq code-cells-eval-region-commands
        '((python-ts-mode . python-shell-send-region)
          (emacs-lisp-mode . eval-region)))
  (with-eval-after-load 'jupyter
    (defalias 'adopt-jupyter-eval-region (apply-partially 'jupyter-eval-region nil))
    (add-to-list 'code-cells-eval-region-commands
                 '(jupyter-repl-interaction-mode . adopt-jupyter-eval-region)))
  (with-eval-after-load 'julia-snail
    (add-to-list 'code-cells-eval-region-commands
                 '(julia-snail-mode . julia-snail-send-code-cell)))
  )

;; %% R/julia
(use-package ess-site
  :ensure ess
  )
(define-auto-insert "\\.R$" 'yx/auto-insert-common-header)

;; %% toy langs
(use-package geiser-chez
  :init
  (setq geiser-chez-binary "chez")
  )

;; %% misc lang
(add-hook
 'sh-mode-hook
 (lambda()
   (setq sh-indentation 2
         sh-basic-offset 2)
   (electric-pair-mode -1)
   (ansi-color-for-comint-mode-on)
   (compilation-shell-minor-mode 1)
   )
 )

(use-package yaml-mode)

(use-package json-mode)

(use-package vimrc-mode
  :mode "\\.?vim\\(rc\\)?\\'"
  )

(use-package gnuplot-mode
  :mode "\\.gp$"
  )

;; %% maxima
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)
(add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode))

;; %% end
(provide 'lang-misc)
