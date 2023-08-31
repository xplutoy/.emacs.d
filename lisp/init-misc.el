;;; init-misc.el --- misc  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 22:55:50
;; Modified: <2023-08-31 22:26:31 yx>
;; Licence: GPLv3

;;; Commentary:

;; misc

;;; Code:

;; %% misc
(use-package gcmh
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-high-cons-threshold (* 128 1024 1024))
  )

(use-package server
  :defer 2
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

(use-package posframe)
(use-package emacsql-sqlite-builtin)

;; %% auxiliary tool
(use-package crux-yx
  :defer 2
  :autoload (unpackaged/def-org-maybe-surround)
  :load-path "site-lisp/crux-yx"
  :config
  (add-hook 'eww-mode-hook
            (lambda ()
              (setq-local imenu-create-index-function
                          'unpackaged/imenu-eww-headings)))
  (crux-with-region-or-buffer indent-region)
  (crux-reopen-as-root-mode 1)
  )

(use-package which-key
  :defer 2
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

(use-package goggles
  :hook ((text-mode prog-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)
  )

;; %% pulse the target line of navigate
(defun yx/pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command
                                     recenter-top-bottom other-window))
  (advice-add command :after #'yx/pulse-line))

;; %% edit enhencement
(use-package vundo)

(use-package avy
  :init
  (setq avy-style 'at
        avy-timeout-seconds 0.8)
  :bind (:map isearch-mode-map
              ("C-'" . avy-isearch))
  :config
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)
  )

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark))
  )

(use-package drag-stuff
  :hook (prog-mode . yx/drag-stuff-setup)
  :preface
  (defun yx/drag-stuff-setup ()
    (keymap-local-set "s-<up>" 'drag-stuff-up)
    (keymap-local-set "s-<down>" 'drag-stuff-down)
    (keymap-local-set "s-<left>" 'drag-stuff-left)
    (keymap-local-set "s-<right>" 'drag-stuff-right)
    )
  )

;; %% spell
(use-package jinx
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (setq jinx-languages "en_US")
  )

(use-package flyspell-correct
  :after flyspell
  )

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

;; %% end
(provide 'init-misc)
;;; init-misc.el ends here
