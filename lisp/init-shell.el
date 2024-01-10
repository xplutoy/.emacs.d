;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:10:40
;; Modified: <2024-01-11 05:08:38 yx>
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
   eshell-save-history-on-exit t
   eshell-kill-processes-on-exit 'ask
   eshell-destroy-buffer-when-process-dies nil
   eshell-hist-ignoredups t
   eshell-error-if-no-glob t
   eshell-prefer-lisp-functions t
   eshell-rm-removes-directories t
   eshell-scroll-to-bottom-on-input 'all
   eshell-scroll-to-bottom-on-output 'all
   eshell-prompt-function 'yx/eshell-prompt)
  :config
  (dolist (m '(eshell-tramp
               eshell-rebind
               eshell-elecslash))
    (add-to-list 'eshell-modules-list m))
  (add-hook 'eshell-mode-hook 'yx/eshell-setup)

  :preface
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
    (eshell/alias "gd"   "magit-diff-unstaged")
    (eshell/alias "gds"  "magit-diff-staged")
    (eshell/alias "gv"   "magit-dispatch")
    (eshell/alias "ll"   "ls -AlohG --color=always")
    (setq-local completion-at-point-functions
                '(cape-file
                  pcomplete-completions-at-point
                  cape-elisp-symbol
                  t))
    )

  (defun yx/eshell-prompt()
    (setq eshell-prompt-regexp "^[^#$\n]*[#$] ")
    (concat (yx/pwd-replace-home (eshell/pwd))
            (if (fboundp 'magit-get-current-branch)
                (if-let ((branch (magit-get-current-branch)))
                    (format " [git:%s]" branch)
                  "")
              "")
            (if (= (user-uid) 0) "\n# " "\n$ ")))

  (defun yx/eshell-clear ()
    "Clear the current Eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (defun yx/eshell-here ()
    (interactive)
    (let* ((project (project-current))
           (name (if project (project-name project) "#"))
           (dir (if (buffer-file-name)
                    (file-name-directory (buffer-file-name))
                  default-directory))
           (old-name eshell-buffer-name)
           (height (/ (frame-height) 3)))
      (setq eshell-buffer-name (concat "*Eshell-" name "*"))
      (eshell)
      (setq eshell-buffer-name old-name)
      (eshell/clear)
      (eshell/cd dir)))

  (defun eshell/z ()
    (let ((dir (completing-read "Directory: " (ring-elements eshell-last-dir-ring) nil t)))
      (eshell/cd dir)))

  (defun eshell/F (filename)
    "Open a file as root from Eshell"
    (let ((qual-filename (if (string-match "^/" filename)
                             filename
                           (concat (expand-file-name (eshell/pwd)) "/" filename))))
      (switch-to-buffer
       (find-file-noselect
        (concat "/sudo::" qual-filename))))))

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

(use-package pcmpl-args
  :after eshell
  :demand t)

(use-package eshell-syntax-highlighting
  :after eshell-mode
  :demand t
  :config
  (eshell-syntax-highlighting-global-mode +1))

(provide 'init-shell)
;;; init-shell.el ends here
