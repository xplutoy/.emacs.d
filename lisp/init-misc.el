;;; -*- coding: utf-8; lexical-binding: t; -*-
(use-package gcmh
  :defer 1
  :config
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (gcmh-mode 1))

(use-package server
  :ensure nil
  :commands (server-running-p)
  :config (or (server-running-p) (server-mode)))

(when (featurep 'xwidget-internal)
  (use-package xwidget
    :ensure nil
    :bind (:map xwidget-webkit-mode-map
                ("W" . xwidget-webkit-fit-width))
    )
  )

(use-package crux-yx
  :defer 1
  :load-path "site-lisp/crux-yx"
  :config
  (add-hook
   'eww-mode-hook
   (lambda ()
     (setq-local
      imenu-create-index-function
      'unpackaged/imenu-eww-headings))
   )
  )

(use-package which-key
  :defer 1
  :init
  (setq
   which-key-idle-delay 1.5
   which-key-show-early-on-C-h t
   which-key-show-remaining-keys t
   which-key-idle-secondary-delay 0.05)
  :config
  (which-key-setup-minibuffer)
  (which-key-mode 1)
  )

(use-package avy
  :init
  (setq avy-style 'at
        avy-timeout-seconds 0.8)
  )

(use-package posframe)

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark))
  )

(use-package no-littering
  :demand
  :init
  (setq no-littering-var-directory yx/var-dir
        no-littering-etc-directory yx/etc-dir
        )
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-var-directory))
    )
  :config
  (no-littering-theme-backups)
  )

(use-package jinx
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (setq jinx-languages "en_US")
  )

(use-package flyspell-correct
  :after flyspell)

(use-package pdf-tools)
(use-package emacsql-sqlite-builtin)

;; dict
(use-package osx-dictionary
  :if -is-mac
  :bind (("C-c D" . osx-dictionary-search-input)
         ("C-c d" . osx-dictionary-search-pointer))
  )

(use-package fanyi
  :unless -is-mac
  :bind (("C-c D" . fanyi-dwim)
         ("C-c d" . fanyi-dwim2))
  :custom
  (fanyi-providers '(fanyi-haici-provider))
  )

(provide 'init-misc)
;;; init-misc.el ends here
