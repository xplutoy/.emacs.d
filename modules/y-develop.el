;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 11:59:41
;; Modified: <2024-06-26 23:49:31 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(use-package gdb-mi
  :ensure nil
  :custom
  (gdb-show-main t)
  (gdb-many-windows t)
  (gdb-delete-out-of-scope nil)
  (gdb-use-colon-colon-notation t)
  (gdb-restore-window-configuration-after-quit t))

(use-package gud
  :ensure nil
  :hook (gud-mode . gud-tooltip-mode)
  :custom
  (gud-highlight-current-line t))

(use-package compile
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output t)
  (compilation-scroll-output 'first-error)
  (compilation-auto-jump-to-first-error t))

(use-package dape
  :init
  (setq dape-adapter-dir (nol-expand-var "dape-debug-adapters")
        dape-buffer-window-arrangment 'right))

(use-package quickrun
  :custom (quickrun-focus-p nil))

(use-package xref
  :ensure nil
  :hook ((xref-after-return xref-after-jump) . recenter)
  :custom
  (xref-search-program 'ripgrep)
  (xref-file-name-display 'project-relative)
  (xref-history-storage 'xref-window-local-history)
  (xref-show-xrefs-function 'xref-show-definitions-buffer)
  (xref-show-definitions-function 'xref-show-definitions-completing-read))

(use-package symbols-outline
  :unless IS-WIN
  :custom
  (symbols-outline-window-width 35)
  :config
  (unless (executable-find "ctags")
    (setq symbols-outline-fetch-fn #'symbols-outline-lsp-fetch))
  (symbols-outline-follow-mode 1))

(use-package diff-mode
  :ensure nil
  :hook (diff-mode . outline-minor-mode)
  :custom
  (diff-default-read-only t)
  (diff-update-on-the-fly t)
  (diff-advance-after-apply-hunk t))

(use-package ediff
  :ensure nil
  :custom
  (ediff-keep-variants nil)
  (ediff-show-clashes-only t)
  (ediff-floating-control-frame t)
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally)
  :config
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

(use-package flymake
  :ensure nil
  :custom
  (flymake-no-changes-timeout nil)
  (flymake-show-diagnostics-at-end-of-line t)
  (flymake-fringe-indicator-position 'right-fring))

(use-package project
  :ensure nil
  :custom
  (project-mode-line t)
  (project-file-history-behavior 'relativize)
  (project-vc-extra-root-markers '(".envrc" "pyproject.toml")))

(use-package apheleia
  :defer 5
  :config (apheleia-global-mode +1))

(use-package reformatter)

(use-package ws-butler
  :hook ((prog-mode conf-mode) . ws-butler-mode))

(use-package indent-guide
  :hook (prog-mode . indent-guide-mode)
  :custom
  (indent-guide-recursive nil))

(use-package snap-indent
  :hook (prog-mode . snap-indent-mode)
  :custom
  (snap-indent-format '(delete-trailing-whitespace)))

(use-package isayt
  ;; :vc (:url "https://gitlab.com/andreyorst/isayt.el")
  :ensure nil
  :hook ((emacs-lisp-mode scheme-mode-hook) . isayt-mode)
  :init
  (unless (package-installed-p 'isayt)
    (package-vc-install "https://gitlab.com/andreyorst/isayt.el")))

(use-package editorconfig
  :defer 5
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  :config
  (editorconfig-mode 1))

(use-package devdocs)

(use-package tempel
  :defer 3
  :bind (("M-+" . tempel-insert)
         ("M-=" . tempel-complete)
         :map tempel-map
         ([tab] . tempel-next)
         ([backtab] . tempel-previous))
  :custom
  (tempel-trigger-prefix "<")
  (tempel-path (nol-expand-etc "templates/tempel.eld"))
  :config
  (defun yx/tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons 'tempel-expand completion-at-point-functions)))
  (add-hook 'conf-mode-hook #'yx/tempel-setup-capf)
  (add-hook 'prog-mode-hook #'yx/tempel-setup-capf)
  (add-hook 'text-mode-hook #'yx/tempel-setup-capf)
  (add-hook 'eglot-managed-mode-hook #'yx/tempel-setup-capf))

(use-package tramp
  :ensure nil
  :custom
  (tramp-verbose 1)
  (tramp-chunksize 2000)
  (tramp-default-method "ssh")
  (tramp-default-remote-shell "/bin/bash")
  :config
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil)))

(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git))
  (vc-git-diff-switches '("--histogram"))
  (vc-ignore-dir-regexp (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp)))

(use-package magit
  :custom
  (magit-clone-default-directory "~/workspace/")
  (magit-diff-refine-hunk 'all)
  (magit-show-long-lines-warning nil)
  (magit-log-arguments '("--color" "--graph" "--decorate"))
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :config
  (defun yx/magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (magit-restore-window-configuration)
    (mapc #'kill-buffer (magit-mode-get-buffers)))
  (keymap-set magit-stash-mode-map "q" #'yx/magit-kill-buffers))

(use-package git-modes)

(use-package diff-hl
  :defer 5
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-disable-on-remote t)
  (diff-hl-show-staged-changes nil)
  :config
  (diff-hl-flydiff-mode +1)
  (global-diff-hl-show-hunk-mouse-mode -1))

(use-package colorful-mode
  :hook ((help-mode helpful-mode prog-mode) . colorful-mode)
  :init
  (setq colorful-use-prefix nil)
  (setq-default colorful-mode-map nil))

(use-package hl-todo
  :hook ((text-mode prog-mode) . hl-todo-mode)
  :init
  (setq hl-todo-keyword-faces
        '(("TODO"  . "#cc9393")
          ("NEXT"  . "#dca3a3")
          ("DONT"  . "#5f7f5f")
          ("FAIL"  . "#8c5353")
          ("HACK"  . "#d0bf8f")
          ("FIXME" . "#cc9393")
          ("ISSUE" . "#e45649")))
  (setq hl-todo-require-punctuation t
        hl-todo-highlight-punctuation ":")
  (defun yx/hl-todo-rg-project ()
    "Use `rg' to find all TODO or similar keywords."
    (interactive)
    (unless (require 'color-rg nil t)
      (error "`color-rg' is not installed"))
    (let* ((regexp (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp))))
      (color-rg-search-input regexp (color-rg-project-root-dir)))))

(use-package symbol-overlay
  :custom (symbol-overlay-priority 0)
  :hook ((prog-mode conf-mode) . symbol-overlay-mode)
  :bind (:map symbol-overlay-map
              ("u" . symbol-overlay-remove-all)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  :config
  (add-to-list 'treesit-extra-load-path
               (nol-expand-var "tree-sitter")))

(use-package treesit-auto
  :defer 2
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs '(c
                        cpp
                        julia
                        latex
                        lua
                        python
                        r
                        yaml
                        toml))
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode +1))

(use-package iedit
  :config
  (advice-add 'iedit-mode :around #'x-iedit-scoped))
(use-package surround
  :bind-keymap ("C-'" . surround-keymap))

(use-package puni
  :hook ((prog-mode sgml-mode nxml-mode) . puni-mode)
  :custom
  (puni-confirm-when-delete-unbalanced-active-region nil)
  :bind (:map puni-mode-map
              ("DEL"         . nil)     ; confict with hungry-delete
              ("C-M-f"       . puni-forward-sexp-or-up-list)
              ("C-M-b"       . puni-backward-sexp-or-up-list)
              ("C-M-<right>" . puni-slurp-forward)
              ("C-M-<left>"  . puni-slurp-backward)
              ("C-M-<up>"    . puni-barf-forward)
              ("C-M-<down>"  . puni-barf-backward)
              ("C-M-SPC"     . puni-mark-sexp-at-point)
              ("C-M-@"       . puni-mark-sexp-around-point)
              ("C-M-r"       . puni-raise)
              ("C-M-0"       . puni-splice)
              ("C-M-z"       . puni-squeeze)
              ("C-M-t"       . puni-transpose)
              ("C-M-="       . puni-expand-region)
              ("C-M--"       . puni-contract-region)
              :repeat-map puni-e/c-repeat-map
              ("=" . puni-expand-region)
              ("-" . puni-contract-region)))

(use-package combobulate
  :disabled
  :vc (:url "https://github.com/mickeynp/combobulate")
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode))
  :custom
  (combobulate-key-prefix "C-x c o"))

(use-package color-rg
  ;; :vc (:url "https://github.com/manateelazycat/color-rg")
  :ensure nil
  :defer 2
  :custom
  (color-rg-recenter-match-line t)
  (color-rg-search-no-ignore-file nil)
  (color-rg-mac-load-path-from-shell nil)
  :init
  (unless (package-installed-p 'color-rg)
    (package-vc-install "https://github.com/manateelazycat/color-rg"))
  (when IS-WIN
    (setq color-rg-command-prefix "powershell")))

(use-package eglot
  :ensure nil
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-sync-connect nil)
  (eglot-report-progress nil)
  (eglot-events-buffer-size 0)
  (eglot-send-changes-idle-time 0.5)
  (eglot-auto-display-help-buffer nil)
  :config
  (add-to-list 'eglot-stay-out-of 'yasnippet)
  (fset #'jsonrpc--log-event #'ignore)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (defun yx/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'tempel-expand
                       #'cape-file))))
  (add-hook 'eglot-managed-mode-hook #'yx/eglot-capf)
  (defun yx/eglot-maybe-format-buffer ()
    (when (bound-and-true-p eglot-managed-p)
      (eglot-format-buffer)))
  (add-hook 'after-save-hook #'yx/eglot-maybe-format-buffer)
  (use-package consult-eglot :demand t)
  :bind (:map eglot-mode-map
              ("M-c r"   . eglot-rename)
              ("M-c M-f" . eglot-format)
              ("M-c M-a" . eglot-code-actions)
              ("M-c s"   . consult-eglot-symbols)))

(use-package citre
  :custom
  (citre-prompt-language-for-ctags-command t)
  (citre-use-project-root-when-creating-tags t)
  (citre-default-create-tags-file-location 'global-cache)
  (citre-auto-enable-citre-mode-modes '(c-ts-mode
                                        python-ts-mode))
  :config
  (with-eval-after-load 'cc-mode
    (require 'citre-lang-c))
  (with-eval-after-load 'dired
    (require 'citre-lang-fileref)))

(use-package pyvenv
  :config
  (with-eval-after-load 'python
    (pyvenv-mode +1))
  (add-to-list 'pyvenv-post-activate-hooks
               (lambda () (setq python-shell-interpreter
                                (executable-find "python"))))
  (add-to-list 'pyvenv-post-deactivate-hooks
               (lambda () (setq python-shell-interpreter "python"))))

(use-package inheritenv
  :demand t
  :config
  (inheritenv-add-advice #'org-babel-eval)
  (inheritenv-add-advice #'with-temp-buffer)
  (inheritenv-add-advice #'async-shell-command)
  (inheritenv-add-advice #'shell-command-to-string))

(use-package buffer-env
  :unless IS-WIN
  :init
  (add-hook 'comint-mode-hook #'buffer-env-update)
  (add-hook 'hack-local-variables-hook #'buffer-env-update)
  :config
  (add-to-list 'buffer-env-command-alist '("/\\.envrc\\'" . "direnv exec . env -0")))

(use-package prog-mode
  :ensure nil
  :custom
  (prettify-symbols-unprettify-at-point 'right-edge)
  :config
  (defun yx/prog-mode-setup ()
    (hs-minor-mode +1)
    (setq line-spacing 0.15
          show-trailing-whitespace t))
  (add-hook 'prog-mode-hook #'yx/prog-mode-setup))

(use-package macrostep
  :custom
  (macrostep-expand-in-separate-buffer t)
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

(use-package cc-mode
  :ensure nil
  :hook (c-mode . eglot-ensure)
  :config
  (add-to-list 'c-default-style '(c-mode . "linux"))
  (defun yx/cc-mode-common-h ()
    (setq tab-width 8
          indent-tabs-mode t
          comment-start "// "
          comment-end ""
          c-basic-offset 8
          c-electric-pound-behavior 'alignleft)
    (c-toggle-auto-hungry-state 1))
  (add-hook 'c-mode-common-hook #'yx/cc-mode-common-h))

(use-package c-ts-mode
  :ensure nil
  :hook (c-ts-mode . eglot-ensure)
  :init (setq c-ts-mode-indent-offset 8))

(use-package python
  :ensure nil
  :config
  (setq python-shell-dedicated t
        python-skeleton-autoinsert t
        python-indent-block-paren-deeper t
        python-indent-guess-indent-offset t
        python-indent-guess-indent-offset-verbose nil
        python-shell-completion-native-disabled-interpreters '("ipython" "jupyter"))
  (defun yx/python-mode-setup ()
    (setq-local tab-width 4
                python-indent-offset 4
                electric-indent-inhibit t
                devdocs-current-docs '("python~3.12"
                                       "pytorch~2"
                                       "numpy~1.23"))
    (setq-local imenu-create-index-function #'python-imenu-create-flat-index)
    (eglot-ensure)
    (flymake-mode 1))
  (add-hook 'python-base-mode-hook 'yx/python-mode-setup)
  (reformatter-define black-format :program "black" :args '("-q" "-"))
  (reformatter-define ruff-format :program "ruff" :args '("--fix-only" "-")))

(use-package python-mls
  :hook (inferior-python-mode . python-mls-mode))

(use-package jupyter
  :after org
  :demand t
  :config
  (setq jupyter-eval-use-overlays nil)
  ;; @see https://github.com/emacs-jupyter/jupyter/issues/478
  (setf (alist-get "python" org-src-lang-modes nil nil #'equal) 'python-ts))

(use-package ess-site
  :ensure ess
  :hook (R-mode . eglot-ensure)
  :config
  (setq ess-eval-visibly-p 'nowait
        ess-local-process-name "R"
        ess-ask-for-ess-directory nil)
  (keymap-set ess-r-mode-map ";" 'ess-insert-assign)
  (keymap-set inferior-ess-r-mode-map ";" 'ess-insert-assign))

(use-package julia-mode)
(use-package julia-ts-mode
  :config
  (defun yx/julia-ts-setup ()
    (eglot-ensure)
    (setq-local devdocs-current-docs '("julia~1.9")))
  (add-hook 'julia-ts-mode-hook #'yx/julia-ts-setup))

(use-package eglot-jl
  :init
  (with-eval-after-load 'eglot
    (eglot-jl-init)))

(use-package julia-snail
  :custom
  (julia-snail-terminal-type :eat)
  (julia-snail-extensions '(ob-julia formatter))
  :hook
  (julia-mode . julia-snail-mode)
  (julia-ts-mode . julia-snail-mode))

(use-package code-cells
  :hook ((julia-mode python-ts-mode) . code-cells-mode-maybe)
  :config
  (setq code-cells-eval-region-commands
        '((python-ts-mode . python-shell-send-region)
          (emacs-lisp-mode . eval-region)))
  (let ((map code-cells-mode-map))
    (keymap-set map "C-c % w" #'code-cells-write-ipynb)
    (keymap-set map "C-c % C-e" #'code-cells-eval-above)
    (keymap-set map "n" (code-cells-speed-key #'code-cells-forward-cell))
    (keymap-set map "p" (code-cells-speed-key #'code-cells-backward-cell))
    (keymap-set map "e" (code-cells-speed-key #'code-cells-eval))
    (keymap-set map "C-e" (code-cells-speed-key #'code-cells-eval-above))
    (keymap-set map "TAB" (code-cells-speed-key #'outline-cycle)))
  (with-eval-after-load 'jupyter
    (defalias 'adopt-jupyter-eval-region (apply-partially 'jupyter-eval-region nil))
    (add-to-list 'code-cells-eval-region-commands
                 '(jupyter-repl-interaction-mode . adopt-jupyter-eval-region)))
  (with-eval-after-load 'julia-snail
    (add-to-list 'code-cells-eval-region-commands
                 '(julia-snail-mode . julia-snail-send-code-cell))))

(setq scheme-program-name "chez"
      inferior-lisp-program "sbcl")

(use-package geiser-chez
  :mode ("\\.sc\\'" . scheme-mode)
  :hook (scheme-mode . turn-on-geiser-mode)
  :custom (geiser-chez-binary "chez"))

(use-package sly
  :hook (lisp-mode . sly-mode))

(use-package haskell-mode
  :custom
  (haskell-stylish-on-save t)
  (haskell-process-log t)
  (haskell-process-auto-import-loaded-modules t)
  :config
  (defun yx/haskell-mode-setup ()
    (haskell-collapse-mode 1)
    (haskell-decl-scan-mode 1)
    (haskell-auto-insert-module-template)
    (speedbar-add-supported-extension ".hs")
    (eval-after-load "which-func"
      '(add-to-list 'which-func-modes 'haskell-mode))
    (eglot-ensure))
  (add-hook 'haskell-mode-hook #'yx/haskell-mode-setup))

(use-package sh-script
  :ensure nil
  :hook (sh-mode . yx/sh-mode-setup)
  :config
  (defun yx/sh-mode-setup ()
    (electric-pair-local-mode -1)
    (compilation-shell-minor-mode 1))
  :custom
  (sh-indentation 2)
  (sh-basic-offset 2))

(use-package sgml-mode
  :ensure nil
  :hook
  (html-mode . sgml-name-8bit-mode)
  (html-mode . sgml-electric-tag-pair-mode)
  :custom
  (sgml-basic-offset 2))

(use-package nxml-mode
  :ensure nil
  :custom
  (nxml-slash-auto-complete-flag t)
  (nxml-auto-insert-xml-declaration-flag t))

(use-package csv-mode)

(use-package vimrc-mode
  :mode "\\.?vim\\(rc\\)?\\'")

(use-package gnuplot-mode
  :mode "\\.gp$")

(use-package graphviz-dot-mode)

(provide 'y-develop)
;;; y-develop.el ends here
