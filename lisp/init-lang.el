;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 22:57:16
;; Modified: <2024-01-10 10:40:33 yx>
;; Licence: GPLv3

;;; Commentary:

;; ide

;;; Code:
(defvar yx/default-python-env "~/workspace/.venv/")

(defun yx/prog-common-setup ()
  (setq-local
   whitespace-style
   '(face trailing lines-char space-before-tab space-after-tab))
  (whitespace-mode 1)
  (hl-line-mode 1)
  (subword-mode -1)
  (hs-minor-mode 1)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (display-line-numbers-mode 1)
  (electric-indent-local-mode 1)
  (keymap-local-set "RET" 'newline-and-indent)
  (push 'cape-keyword completion-at-point-functions))

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
          python-ts-mode) . code-cells-mode-maybe)
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

;; %% python
(setq
 python-shell-dedicated t
 python-skeleton-autoinsert t
 python-indent-guess-indent-offset t
 python-indent-guess-indent-offset-verbose nil
 python-shell-virtualenv-root yx/default-python-env
 python-shell-interpreter "jupyter"
 python-shell-interpreter-args "console --simple-prompt"
 python-shell-completion-native-disabled-interpreters '("ipython" "jupyter"))

(defun yx/python-mode-setup ()
  (setq-local
   tab-width 4
   python-indent-offset 4
   electric-indent-inhibit t
   imenu-create-index-function 'python-imenu-create-flat-index
   ))
(add-hook 'python-ts-mode-hook 'yx/python-mode-setup)

(define-auto-insert "\\.py$" 'yx/auto-insert-common-header)

(use-package pyvenv
  :hook (after-init . yx/active-default-pyvenv)
  :preface
  (defun yx/active-default-pyvenv ()
    (interactive)
    (pyvenv-activate yx/default-python-env)
    )
  )

(use-package pyvenv-auto
  :hook (python-ts-mode . pyvenv-auto-run))

(use-package poetry
  :hook (python-ts-mode . poetry-tracking-mode))

(use-package jupyter
  :after org
  :demand t
  :config
  (setq jupyter-eval-use-overlays nil)
  ;; @see https://github.com/emacs-jupyter/jupyter/issues/478
  (setf (alist-get "python" org-src-lang-modes nil nil #'equal) 'python-ts)
  )

;; %% R/julia
(use-package ess-site
  :ensure ess
  :init
  (setq
   ess-eval-visibly-p 'nowait
   ess-local-process-name "R"
   ess-ask-for-ess-directory nil)
  :config
  (keymap-set ess-r-mode-map ";" 'ess-insert-assign)
  (keymap-set inferior-ess-r-mode-map ";" 'ess-insert-assign)
  )

(use-package julia-mode)
(use-package julia-ts-mode
  :mode "\\.jl$")

(use-package eglot-jl
  :init
  (with-eval-after-load 'eglot
    (eglot-jl-init))
  )

(use-package julia-snail
  :custom
  (julia-snail-terminal-type :eat)
  (julia-snail-extensions '(ob-julia formatter))
  :hook
  (julia-mode . julia-snail-mode)
  (julia-ts-mode . julia-snail-mode)
  )

(define-auto-insert "\\.R$" 'yx/auto-insert-common-header)
(define-auto-insert "\\.jl$" 'yx/auto-insert-common-header)

;; %% toy langs
(use-package lua-ts-mode
  :mode ("\\.lua\\'" . lua-ts-mode)
  :init
  (setq lua-ts-indent-offset 2)
  )

(use-package geiser-chez
  :init
  (setq geiser-chez-binary "chez")
  )

(use-package haskell-mode
  :hook (haskell-mode . yx/haskell-mode-setup)
  :custom
  (haskell-stylish-on-save t)
  (haskell-process-log t)
  (haskell-process-auto-import-loaded-modules t)
  :preface
  (defun yx/haskell-mode-setup ()
    (haskell-collapse-mode 1)
    (haskell-decl-scan-mode 1)
    (haskell-auto-insert-module-template)
    (speedbar-add-supported-extension ".hs")
    (eval-after-load "which-func"
      '(add-to-list 'which-func-modes 'haskell-mode)))
  )

;; %% misc lang
(add-hook
 'sh-mode-hook
 (lambda()
   (setq sh-indentation 2
         sh-basic-offset 2)
   (electric-pair-mode -1)
   (compilation-shell-minor-mode 1)
   ))

(use-package vimrc-mode
  :mode "\\.?vim\\(rc\\)?\\'")

(use-package gnuplot-mode
  :mode "\\.gp$")

(use-package graphviz-dot-mode)

;; %% maxima
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)
(add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode))


(provide 'init-lang)
;;; init-lang.el ends here
