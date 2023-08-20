;;; -*- coding: utf-8; lexical-binding: t; -*-
(add-hook
 #'prog-mode-hook
 (lambda ()
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
    show-trailing-whitespace t)
   (whitespace-mode 1)
   (local-set-key (kbd "RET") 'newline-and-indent)
   (add-hook 'before-save-hook 'delete-trailing-whitespace)
   )
 )

;; ediff
(setq
 ediff-keep-variants nil
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-split-window-function 'split-window-horizontally)

(use-package eglot
  :ensure nil
  :init
  (setq
   eglot-autoshutdown t
   eglot-extend-to-xref t
   eglot-report-progress nil
   )
  :hook ((c-mode
          c-ts-mode
          R-mode
          python-mode
          python-ts-mode
          julia-mode
          julia-ts-mode
          LaTeX-mode) . eglot-ensure)
  )

(use-package treesit
  :unless -is-win
  :ensure nil
  :preface
  (defun yx/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist
        (grammar
         '((c "https://github.com/tree-sitter/tree-sitter-c/")
           (cpp "https://github.com/tree-sitter/tree-sitter-cpp/")
           (julia "https://github.com/tree-sitter/tree-sitter-julia")
           (python "https://github.com/tree-sitter/tree-sitter-python")))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        ;; (treesit-install-language-grammar (car grammar) (car treesit-extra-load-path))
        (treesit-install-language-grammar (car grammar)))))
  :init
  (setq
   treesit-extra-load-path (list (no-littering-expand-var-file-name "tree-sitter")))
  (dolist
      (mapping
       '((c-mode . c-ts-mode)
         (c++-mode . c++-ts-mode)
         (python-mode . python-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (yx/setup-install-grammars)
  )

(use-package reformatter
  :defer 2
  :config
  ;; python
  (reformatter-define python-isort
    :program "isort" :args '("--stdout" "--atomic" "-"))
  (add-hook 'python-ts-mode-hook 'python-isort-on-save-mode)
  (reformatter-define python-black
    :program "black" :args '("-"))
  (add-hook 'python-ts-mode-hook 'python-black-on-save-mode)
  )

(use-package tempel
  :bind
  ("M-*" . tempel-insert)
  ("M-=" . tempel-complete)
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons 'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-abbrev-mode)
  )
(use-package tempel-collection)

;; diff-hl
(use-package diff-hl
  :defer 2
  :config
  (setq diff-hl-disable-on-remote t)
  (global-diff-hl-mode 1)
  :hook (dired-mode . diff-hl-dired-mode)
  )

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;; aggressive-indent
(use-package aggressive-indent
  :hook (prog-mode . aggressive-indent-mode)
  )

;; indent-guide
(use-package indent-guide
  :init
  (setq indent-guide-recursive nil)
  :hook (prog-mode . indent-guide-mode)
  )

;; symbol-overlay
(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode)
  )

;; magit
(use-package magit)

(use-package color-rg
  :defer 2
  :load-path "site-lisp/color-rg"
  :init
  (setq color-rg-search-no-ignore-file nil
        color-rg-mac-load-path-from-shell nil)
  )

(use-package combobulate
  :ensure nil
  :after treesit
  :load-path "site-lisp/combobulate"
  :hook
  ((c-ts-mode
    c++-ts-mode
    python-ts-mode
    ) . combobulate-mode)
  )

;; %% jupyter
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

(use-package jupyter
  :after org
  :demand
  :config
  (setq jupyter-eval-use-overlays nil)
  ;; @see https://github.com/emacs-jupyter/jupyter/issues/478
  (setf (alist-get "python" org-src-lang-modes nil nil #'equal) 'python-ts)
  )

;; %% python
(defvar yx/default-python-env "~/workspace/.venv/")

(add-hook
 'python-ts-mode-hook
 (lambda()
   (setq-local
    tab-width 2
    python-indent-offset 4
    imenu-create-index-function 'python-imenu-create-flat-index
    )
   (semantic-mode 1)
   ))

(setq
 python-shell-dedicated t
 python-skeleton-autoinsert t
 python-indent-guess-indent-offset-verbose nil
 python-shell-virtualenv-root yx/default-python-env
 python-shell-interpreter "jupyter"
 python-shell-interpreter-args "console --simple-prompt"
 python-shell-completion-native-disabled-interpreters '("ipython" "jupyter"))

;; %% pyvenv
(use-package pyvenv
  :defer 2
  :config
  (pyvenv-activate yx/default-python-env)
  )

(use-package pyvenv-auto
  :hook (python-ts-mode . pyvenv-auto-run))
(use-package poetry
  :hook (python-ts-mode . poetry-tracking-mode))

;; %% Julia
(use-package julia-mode)
(use-package eglot-jl
  :init
  (with-eval-after-load 'eglot
    (eglot-jl-init))
  )

(use-package julia-snail
  :custom
  (julia-snail-terminal-type :eat)
  (julia-snail-extensions '(ob-julia formatter))
  :hook (julia-mode . julia-snail-mode)
  )

;; %% R/julia
(use-package ess-site
  :ensure ess
  )

;; yaml
(use-package yaml-mode)

;; sh-script
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

;; vim-script
(use-package vimrc-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.?vim\\(rc\\)?\\'" . vimrc-mode))
  )

;; %% maxima
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)
(add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode))

;; %% end
(provide 'init-lang)
