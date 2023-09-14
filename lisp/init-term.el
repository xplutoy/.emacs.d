;;; init-term.el --- terminal  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:10:40
;; Modified: <2023-09-14 09:46:18 yx>
;; Licence: GPLv3

;;; Commentary:

;; terminal shell

;;; Code:

;; %% eshell
(defun eshell/gst (&rest args)
  (magit-status (pop args) nil)
  (eshell/echo))

(defun eshell/sudo-open (filename)
  "Open a file as root in Eshell"
  (let ((qual-filename (if (string-match "^/" filename)
                           filename
                         (concat (expand-file-name (eshell/pwd)) "/" filename))))
    (switch-to-buffer
     (find-file-noselect
      (concat "/sudo::" qual-filename)))
    )
  )

(use-package eshell
  :init
  (setq
   eshell-hist-ignoredups t
   eshell-error-if-no-glob t
   eshell-save-history-on-exit t
   eshell-prefer-lisp-functions t
   eshell-scroll-to-bottom-on-input 'all
   eshell-destroy-buffer-when-process-dies t
   )
  (add-hook
   'eshell-mode-hook
   (lambda ()
     (define-key eshell-mode-map (kbd "C-l") 'eshell/clear)
     (setq
      eshell-visual-commands
      '("vim" "ssh" "tail" "top" "htop" "tmux" "less" "more")
      eshell-visual-subcommands '(("git" "log" "diff" "show")))

     (eshell/alias "q"    "exit")
     (eshell/alias "r"    "consult-recent-file")
     (eshell/alias "d"    "dired $1")
     (eshell/alias "f"    "find-file $1")
     (eshell/alias "gs"   "magit-status")
     (eshell/alias "gv"   "magit-dispatch")
     (eshell/alias "ll"   "ls -AlohG --color=always")
     )
   )
  )

(use-package eshell-git-prompt-yx
  :load-path "site-lisp/eshell-git-prompt-yx"
  :autoload eshell-git-prompt-multiline
  :init
  (setq eshell-prompt-function 'eshell-git-prompt-multiline)
  )

(use-package pcmpl-args
  :after eshell
  :demand
  )

;; %% vterm
(use-package vterm
  :unless IS-WIN
  :bind (:map vterm-mode-map
              ("C-y" . vterm-yank)
              ("M-y" . vterm-yank-pop)
              ("C-k" . vterm-send-C-k-and-kill))
  :config
  (setq vterm-always-compile-module t)
  (defun vterm-send-C-k-and-kill ()
    (kill-ring-save (point) (vterm-end-of-line))
    (vterm-send-key "k" nil nil t)
    )
  )
(use-package vterm-toggle
  :after vterm
  :custom
  (vterm-toggle-hide-method 'delete-window)
  (vterm-toggle-cd-auto-create-buffer nil)
  (vterm-toggle-reset-window-configration-after-exit t)
  )

(use-package eshell-vterm
  :after vterm
  :demand
  :config
  (eshell-vterm-mode)
  (defalias 'eshell/v 'eshell-exec-visual))

;; %% eat
(use-package eat
  :load-path "site-lisp/emacs-eat"
  :hook ((eshell-load . eat-eshell-mode)
         (eshell-load . eat-eshell-visual-command-mode))
  :init
  (setq
   eat-kill-buffer-on-exit t
   eat-enable-yank-to-terminal t)
  )

;; %% end
(provide 'init-term)
;;; init-term.el ends here
