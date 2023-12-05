;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-09-15 22:10:42
;; Modified: <2023-12-05 19:37:13 yx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(use-package ws-butler
  :hook ((prog-mode conf-mode) . ws-butler-mode)
  )

;; ediff
(setq ediff-keep-variants nil
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      )

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(add-hook 'diff-mode-hook
          (lambda ()
            (setq-local whitespace-style
                        '(face
                          tabs
                          tab-mark
                          spaces
                          space-mark
                          trailing
                          indentation::space
                          indentation::tab
                          newline
                          newline-mark))
            (whitespace-mode 1)))

;; %% formatter & linter & profiler
(setq flymake-start-on-save-buffer t
      flymake-start-on-flymake-mode nil
      flymake-fringe-indicator-position 'right-fringe)
(with-eval-after-load 'flymake
  (bind-keys :map flymake-mode-map
             ("C-c e n" . flymake-goto-next-error)
             ("C-c e p" . flymake-goto-prev-error)
             ("C-c e l" . flymake-show-buffer-diagnostics)
             ("C-c e L" . flymake-show-project-diagnostics))
  )

(use-package apheleia
  :init
  (apheleia-global-mode +1))

;; %% snippet
(use-package tempel
  :defer 2
  :custom
  (tempel-trigger-prefix "<")
  :bind
  (("M-=" . tempel-insert)
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
  :config
  (global-tempel-abbrev-mode)
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
(setq vc-handled-backends '(Git))

(use-package diff-hl
  :hook
  (after-init . global-diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (setq diff-hl-disable-on-remote t)
  (diff-hl-flydiff-mode 1)
  :bind (:map diff-hl-mode-map
              ("<left-fringe> <mouse-1>" . diff-hl-diff-goto-hunk))
  )

(use-package git-modes)

(use-package magit
  :custom
  (magit-diff-refine-hunk t)
  )

(use-package magit-todos
  :after magit
  :demand t
  :config (magit-todos-mode 1))

;; %% indent
(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  )

(use-package indent-guide
  :hook (prog-mode . indent-guide-mode)
  :custom
  (indent-guide-recursive nil)
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
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("HOLD"  . "#d0bf8f")
          ("TODO"  . "#cc9393")
          ("NEXT"  . "#dca3a3")
          ("OKAY"  . "#7cb8bb")
          ("DONT"  . "#5f7f5f")
          ("FAIL"  . "#8c5353")
          ("DONE"  . "#afd8af")
          ("NOTE"  . "#d0bf8f")
          ("HACK"  . "#d0bf8f")
          ("FIXME" . "#cc9393")
          ("ISSUE" . "#e45649")
          ("TRICK" . "#d0bf8f")
          ("DEBUG" . "#7cb8bb")
          ))
  :bind
  (:map hl-todo-mode-map
        ("C-c t t" . hl-todo-occur)
        ("C-c t g" . hl-todo-rgrep)
        ("C-c t i" . hl-todo-insert)
        ("C-c t n" . hl-todo-next)
        ("C-c t p" . hl-todo-previous))
  )

(use-package idle-highlight-mode
  :hook ((prog-mode text-mode) . idle-highlight-mode))

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
         (c-or-c++-mode . c-or-c++-ts-mode)
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
(use-package puni
  :hook
  ((tex-mode
    prog-mode
    sgml-mode
    nxml-mode) . puni-mode)
  )

(use-package combobulate
  :ensure nil
  :load-path "site-lisp/combobulate"
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
    LaTeX-mode) . eglot-ensure)
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
   ("s-e r" . eglot-rename)
   ("s-e f" . eglot-format)
   ("s-e a" . eglot-code-actions)
   ("s-e s" . consult-eglot-symbols))
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


(provide 'init-ide)
;;; init-ide.el ends here
