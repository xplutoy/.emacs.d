;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:10:40
;; Modified: <2024-01-03 05:50:11 yx>
;; Licence: GPLv3

;;; Commentary:

;; terminal shell

;;; Code:
(setq
 comint-input-ignoredups t
 comint-prompt-read-only t
 comint-completion-autolist t
 comint-completion-addsuffix t
 comint-buffer-maximum-size 9999
 comint-scroll-to-bottom-on-input t
 comint-scroll-show-maximum-output t
 comint-scroll-to-bottom-on-output nil)

(setq
 shell-kill-buffer-on-exit t
 shell-highlight-undef-enable t
 shell-command-prompt-show-cwd t)

(use-package eshell
  :init
  (setq
   eshell-kill-on-exit t
   eshell-hist-ignoredups t
   eshell-error-if-no-glob t
   eshell-save-history-on-exit t
   eshell-prefer-lisp-functions t
   eshell-kill-processes-on-exit t
   eshell-rm-removes-directories t
   eshell-scroll-to-bottom-on-input 'all
   eshell-scroll-to-bottom-on-output 'all
   eshell-destroy-buffer-when-process-dies t)
  :config
  (dolist (m '(eshell-rebind
               eshell-tramp
               eshell-xtra
               eshell-elecslash))
    (add-to-list 'eshell-modules-list m))
  (add-hook 'eshell-mode-hook 'yx/eshell-setup)

  :preface
  (defun yx/esheell-cape-capf()
    (dolist (ele '(cape-file cape-history cape-elisp-symbol))
      (add-to-list 'completion-at-point-functions ele)))
  (defun yx/eshell-setup ()
    (keymap-set eshell-mode-map "C-l" 'yx/eshell-clear)
    (keymap-set eshell-mode-map "C-r" 'consult-history)
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
    (yx/eshell-cape-capf)
    )

  (defun yx/eshell-clear ()
    "Clear the current Eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)
      )
    )
  (defun eshell/z ()
    (let ((dir (completing-read "Directory: " (ring-elements eshell-last-dir-ring) nil t)))
      (eshell/cd dir)
      ))

  (defun eshell/F (filename)
    "Open a file as root from Eshell"
    (let ((qual-filename (if (string-match "^/" filename)
                             filename
                           (concat (expand-file-name (eshell/pwd)) "/" filename))))
      (switch-to-buffer
       (find-file-noselect
        (concat "/sudo::" qual-filename)))
      )
    )
  )

(use-package eshell-git-prompt-yx
  :load-path "site-lisp/eshell-git-prompt-yx"
  :autoload eshell-git-prompt-multiline
  :init
  (setq eshell-prompt-function 'eshell-git-prompt-multiline)
  )

(use-package eshell-syntax-highlighting
  :after eshell-mode
  :demand t
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package pcmpl-args
  :after eshell
  :demand t
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


(provide 'init-terminal)
;;; init-terminal.el ends here
