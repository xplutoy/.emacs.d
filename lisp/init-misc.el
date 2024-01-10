;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 22:55:50
;; Modified: <2024-01-10 08:02:19 yx>
;; Licence: GPLv3

;;; Commentary:

;; misc

;;; Code:

;; %% misc
(use-package gcmh
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-high-cons-threshold (* 32 1024 1024)))

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

(use-package engine-mode
  :hook (after-init . engine-mode)
  :custom
  (engine/browser-function 'browse-url-generic)
  :config
  (yx/define-enines
   '(("c" "https://github.com/search?q=%s")
     ("g" "https://www.google.com/search?q=%s")
     ("b" "https://cn.bing.com/search?q=%s&ensearch=1")
     ("w" "https://zh.wikipedia.org/w/index.php?search=%s")
     ("a" "https://www.wolframalpha.com/input/?i=%s")
     ("z" "https://www.zhihu.com/search?q=%s")
     ("d" "https://search.douban.com/book/subject_search?search_text=%s")))
  :preface
  (defun yx/extract-name-from-url (url)
    (let* ((host (url-host (url-generic-parse-url url)))
           (host-trimmed (split-string host  "\\.")))
      (car (last host-trimmed 2))))

  (defun yx/define-enines (engines)
    (dolist (engi engines)
      (let* ((key  (car engi))
             (url  (cadr engi))
             (name (yx/extract-name-from-url url))
             (symn (make-symbol name)))
        (eval `(defengine ,symn ,url :keybinding ,key)))))
  )

(use-package posframe)

;; %% auxiliary tool
(use-package crux-yx
  :defer 2
  :autoload (crux-yx/def-org-maybe-surround)
  :load-path "site-lisp/crux-yx"
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-reopen-as-root-mode 1)
  )

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.2)
  :config
  (which-key-setup-minibuffer)
  )

(use-package goggles
  :hook ((text-mode prog-mode) . goggles-mode)
  :custom
  (goggles-pulse t)
  )

(use-package ace-link
  :hook (after-init . ace-link-setup-default))

(use-package helpful
  :bind
  (:map helpful-mode-map
        ("q" . delete-window)
        ("b" . yx/helpful-next-buffer)
        ("f" . yx/helpful-prev-buffer))
  :preface
  (defun yx/helpful-next-buffer ()
    (interactive)
    (cl-letf ((bufname (buffer-name))
              (switch-to-prev-buffer-skip-regexp nil))
      (next-buffer)
      (unless (string= (buffer-name) bufname)
        (while (not (eq major-mode 'helpful-mode))
          (next-buffer)))))
  (defun yx/helpful-prev-buffer ()
    (interactive)
    (cl-letf ((bufname (buffer-name))
              (switch-to-prev-buffer-skip-regexp nil))
      (previous-buffer)
      (unless (string= (buffer-name) bufname)
        (while (not (eq major-mode 'helpful-mode))
          (previous-buffer))))
    ))

(use-package command-log-mode
  :custom
  (command-log-mode-key-binding-open-log nil))

;; %% pulse the target line of navigate
(defun yx/pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command
                   scroll-down-command
                   recenter-top-bottom other-window))
  (advice-add command :after #'yx/pulse-line))

;; %% edit enhencement
(use-package vundo)
(use-package goto-chg)

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

(use-package hungry-delete
  :hook (after-init . global-hungry-delete-mode)
  :custom
  (hungry-delete-join-reluctantly t)
  )

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark))
  )

(use-package drag-stuff
  :hook ((text-mode
          prog-mode) . drag-stuff-mode)
  :bind (:map drag-stuff-mode-map
              ("M-J" . drag-stuff-down)
              ("M-K" . drag-stuff-up)
              ("M-H" . drag-stuff-left)
              ("M-L" . drag-stuff-right))
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
  :bind
  (:map flyspell-mode-map
        ("M-$" . flyspell-correct-wrapper))
  )

;; %% chinese
(use-package sis
  :demand t
  :init
  (setq
   sis-external-ism "im-select"
   sis-respect-restore-triggers
   '(isearch-exit isearch-abort)
   sis-respect-go-english-triggers
   '(iserach-forward isearch-backward)
   sis-respect-minibuffer-triggers
   `(,(cons 'denote (lambda () 'other))
     ,(cons 'denote-template (lambda () 'other))
     ,(cons 'denote-open-or-create (lambda () 'other)))
   )
  :config
  (add-to-list 'sis-prefix-override-keys "M-s")
  (add-to-list 'sis-prefix-override-keys "M-g")

  (when IS-WIN
    (sis-ism-lazyman-config "1033" "2052" 'im-select))

  (sis-global-inline-mode 1)
  (sis-global-respect-mode 1)
  (add-hook 'org-capture-mode-hook 'sis-set-other))

(use-package osx-dictionary
  :if IS-MAC
  :bind (("M-s M-d " . osx-dictionary-search-input)
         ("M-s d"    . osx-dictionary-search-pointer))
  )

(use-package fanyi
  :unless IS-MAC
  :bind (("M-s M-d" . fanyi-dwim)
         ("M-s d"   . fanyi-dwim2))
  :custom
  (fanyi-providers '(fanyi-haici-provider))
  )

;; %% tools
(use-package tldr)

(use-package dwim-shell-command
  :bind
  (([remap shell-command] . dwim-shell-command)
   :map dired-mode-map
   ([remap dired-do-async-shell-command] . dwim-shell-command)
   ([remap dired-do-shell-command] . dwim-shell-command)
   ([remap dired-smart-shell-command] . dwim-shell-command)))


(provide 'init-misc)
;;; init-misc.el ends here
