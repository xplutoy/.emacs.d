;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-09-15 22:10:42
;; Modified: <2024-01-08 20:51:00 yx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(use-package ws-butler
  :hook ((prog-mode conf-mode) . ws-butler-mode)
  )

;; ediff
(setq
 diff-default-read-only t
 diff-update-on-the-fly t)

(setq
 ediff-keep-variants nil
 ediff-show-clashes-only t
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-split-window-function 'split-window-horizontally)

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; %% formatter & linter & profiler
(setq
 flymake-start-on-save-buffer t
 flymake-start-on-flymake-mode t
 flymake-fringe-indicator-position 'right-fringe)
(with-eval-after-load 'flymake
  (bind-keys :map flymake-mode-map
             ("s-; s"   . flymake-start)
             ("s-; d"   . flymake-show-buffer-diagnostics)
             ("s-; M-d" . flymake-show-project-diagnostics)
             ("s-; M-n" . flymake-goto-next-error)
             ("s-; M-p" . flymake-goto-prev-error))
  )

(use-package apheleia
  :init
  (apheleia-global-mode +1))

;; %% snippet
(use-package tempel
  :defer 2
  :bind
  (("M-+" . tempel-insert)
   ("M-=" . tempel-complete)
   :map tempel-map
   ([tab] . tempel-next)
   ([backtab] . tempel-previous))
  :hook
  ((prog-mode text-mode) . tempel-setup-capf)
  :init
  (setq tempel-path
        (expand-file-name "templates/tempel.eld" no-littering-etc-directory))
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons 'tempel-expand
                      completion-at-point-functions)))
  )

(use-package yasnippet
  :ensure t
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode) . yas-minor-mode-on)
  )

(use-package yasnippet-snippets)

;; %% version control
(use-package diff-hl
  :hook
  (after-init . global-diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (setq
   diff-hl-disable-on-remote t
   diff-hl-show-staged-changes t)
  (diff-hl-flydiff-mode 1)
  (global-diff-hl-show-hunk-mouse-mode 1)
  )

(use-package git-modes)

(use-package magit
  :init
  (setq
   magit-diff-refine-hunk t
   magit-show-long-lines-warning nil)
  )

;; %% indent
(use-package editorconfig
  :defer 2
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  :config
  (editorconfig-mode 1))

(use-package snap-indent
  :hook (prog-mode . snap-indent-mode)
  :custom
  (snap-indent-format '(delete-trailing-whitespace)))

(use-package indent-guide
  :hook (prog-mode . indent-guide-mode)
  :custom
  (indent-guide-recursive nil)
  )

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  )

;; %% doc
(use-package devdocs
  :init
  (add-hook 'julia-ts-mode-hook
            (lambda () (setq-local devdocs-current-docs '("julia~1.9"))))
  (add-hook 'python-base-mode-hook
            (lambda () (setq-local devdocs-current-docs '("python~3.12" "pytorch" "numpy~1.23"))))
  )

;; %% symbol highlight
(use-package rainbow-mode
  :custom
  (rainbow-x-colors nil)
  :hook (emacs-lisp . rainbow-mode))

(use-package hl-todo
  :hook ((text-mode prog-mode) . hl-todo-mode)
  :init
  (setq hl-todo-keyword-faces
        '(("TODO"  . "#cc9393")
          ("NEXT"  . "#dca3a3")
          ("OKAY"  . "#7cb8bb")
          ("DONT"  . "#5f7f5f")
          ("FAIL"  . "#8c5353")
          ("HACK"  . "#d0bf8f")
          ("FIXME" . "#cc9393")
          ("ISSUE" . "#e45649")
          ("TRICK" . "#d0bf8f")))
  (setq hl-todo-require-punctuation t
        hl-todo-highlight-punctuation ":")

  (defun yx/hl-todo-rg-project ()
    "Use `rg' to find all TODO or similar keywords."
    (interactive)
    (unless (require 'color-rg nil t)
      (error "`color-rg' is not installed"))
    (let* ((regexp (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp))))
      (color-rg-search-input regexp (color-rg-project-root-dir))
      ))
  )

(use-package symbol-overlay
  :custom
  (symbol-overlay-priority 0)
  :hook ((prog-mode conf-mode) . symbol-overlay-mode)
  :bind (:map symbol-overlay-map
              ("u" . symbol-overlay-remove-all))
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package treesit
  :unless IS-WIN
  :ensure nil
  :preface
  (defun yx/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist
        (grammar
         '((c      "https://github.com/tree-sitter/tree-sitter-c")
           (cpp    "https://github.com/tree-sitter/tree-sitter-cpp")
           (lua    "https://github.com/MunifTanjim/tree-sitter-lua")
           (org    "https://github.com/milisims/tree-sitter-org")
           (julia  "https://github.com/tree-sitter/tree-sitter-julia")
           (python "https://github.com/tree-sitter/tree-sitter-python")))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        ;; (treesit-install-language-grammar (car grammar) (car treesit-extra-load-path))
        (treesit-install-language-grammar (car grammar)))))
  :init
  (setq
   treesit-extra-load-path (list (no-littering-expand-var-file-name "tree-sitter"))
   treesit-load-name-override-list '((c++ "libtree-sitter-cpp")))
  (dolist
      (mapping
       '((c-mode . c-ts-mode)
         (c++-mode . c++-ts-mode)
         (python-mode . python-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (yx/setup-install-grammars)
  )

;; %% refoctor
(use-package color-rg
  :load-path "site-lisp/color-rg"
  :defer 2
  :custom
  (color-rg-search-no-ignore-file nil)
  (color-rg-mac-load-path-from-shell nil)
  )

;; %% structured edit
(use-package surround
  :defer 2
  :bind-keymap ("M-'" . surround-keymap))

(use-package puni
  :hook ((tex-mode
          prog-mode
          sgml-mode
          nxml-mode) . puni-mode)
  :bind
  (:map puni-mode-map
        ("DEL"     . nil)
        ("C-d"     . nil)
        ("C-w"     . nil)
        ("s-' r"   . puni-raise)
        ("s-' u"   . puni-splice)
        ("s-' M-s" . puni-squeeze)
        ("s-' l"   . puni-slurp-forward)
        ("s-' h"   . puni-slurp-backward)
        ("s-' M-l" . puni-barf-forward)
        ("s-' M-h" . puni-barf-backward)
        ("s-' m"   . puni-mark-sexp-at-point)
        ("s-' M-m" . puni-mark-sexp-around-point)
        ("s-' ="   . puni-expand-region)
        ("s-' -"   . puni-contract-region)
        )
  )

(use-package combobulate
  :ensure nil
  :load-path "site-lisp/combobulate"
  :custom
  (combobulate-key-prefix "M-l l")
  :hook ((python-ts-mode
          ) . combobulate-mode)
  )

;; %% lsp
(use-package eglot
  :ensure nil
  :hook
  ((c-mode
    c-ts-mode
    R-mode
    python-mode
    python-ts-mode
    julia-mode
    julia-ts-mode
    LaTeX-mode
    haskell-mode) . eglot-ensure)
  :init
  (setq
   eglot-autoshutdown t
   eglot-extend-to-xref t
   eglot-sync-connect nil
   eglot-report-progress nil
   eglot-events-buffer-size 0)
  :bind
  (:map
   eglot-mode-map
   ("s-; r" . eglot-rename)
   ("s-; f" . eglot-format)
   ("s-; a" . eglot-code-actions)
   ("s-; g" . consult-eglot-symbols))
  :config
  (fset #'jsonrpc--log-event #'ignore) ; massive perf boost---don't log every event
  )

(use-package consult-eglot
  :after consult
  )

(use-package sideline
  :hook
  (flymake-mode . yx/sideline-flymake-mode-setup)
  :preface
  (defun yx/sideline-flymake-mode-setup ()
    (setq-local
     sideline-backends-right '(sideline-flymake))
    (sideline-mode 1)))
(use-package sideline-flymake)

(use-package dape
  :init
  (setq
   dape-adapter-dir
   (no-littering-expand-var-file-name "dape-debug-adapters")
   dape-buffer-window-arrangment 'right)
  )

(use-package quickrun
  :custom
  (quickrun-focus-p nil)
  )

(provide 'init-ide)
;;; init-ide.el ends here
