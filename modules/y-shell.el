;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 13:03:19
;; Modified: <2024-07-07 15:15:46 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(use-package comint
  :ensure nil
  :bind (:map comint-mode-map
	      ([remap kill-region] . comint-kill-regio)
	      ([remap kill-whole-line] . comint-kill-whole-line))
  :custom
  (comint-input-ignoredups t)
  (comint-prompt-read-only t)
  (comint-input-autoexpand 'input)
  (comint-completion-autolist t)
  (comint-scroll-to-bottom-on-input t)
  (comint-scroll-to-bottom-on-output t))

(use-package shell
  :ensure nil
  :custom
  (shell-kill-buffer-on-exit t)
  (shell-highlight-undef-enable t))

(use-package eshell
  :custom
  (eshell-kill-on-exit t)
  (eshell-history-append t)
  (eshell-save-history-on-exit t)
  (eshell-hist-ignoredups t)
  (eshell-error-if-no-glob t)
  (eshell-prefer-lisp-functions t)
  (eshell-scroll-to-bottom-on-input  'all)
  (eshell-scroll-to-bottom-on-output 'all)
  (eshell-prompt-function 'yx/eshell-prompt)
  :config
  (setenv "PAGER" "cat")
  (add-to-list 'eshell-modules-list #'eshell-tramp)
  (add-to-list 'eshell-modules-list #'eshell-rebind)
  (add-to-list 'eshell-modules-list #'eshell-elecslash)

  (with-eval-after-load 'em-alias
    (eshell/alias "q"    "exit")
    (eshell/alias "r"    "consult-recent-file")
    (eshell/alias "d"    "dired $1")
    (eshell/alias "f"    "find-file $1")
    (eshell/alias "gs"   "magit-status")
    (eshell/alias "gd"   "magit-diff-unstaged")
    (eshell/alias "gds"  "magit-diff-staged")
    (eshell/alias "gv"   "magit-dispatch")
    (eshell/alias "la"   "ls -laAFh $*")
    (eshell/alias "ll"   "ls -AlohG --color=always"))

  (with-eval-after-load 'em-term
    (appendq! eshell-visual-subcommands '(("git" "log" "diff" "show"))))

  (defun yx/eshell-setup ()
    (keymap-set eshell-mode-map "C-l" #'yx/eshell-clear)
    (keymap-set eshell-mode-map "M-h" #'consult-history)
    (set-window-fringes nil 0 0)
    (set-window-margins nil 1 nil)
    (setq-local corfu-auto nil
                corfu-quit-at-boundary t
                corfu-quit-no-match t)
    (corfu-mode +1))
  (add-hook 'eshell-mode-hook #'yx/eshell-setup)

  (defun yx/eshell-clear ()
    "Clear the current Eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (defun yx/eshell-prompt()
    (setq eshell-prompt-regexp "^[^#$\n]*[#$] ")
    (concat (x-common-pwd-replace-home (eshell/pwd))
            (if (fboundp 'magit-get-current-branch)
                (if-let ((branch (magit-get-current-branch)))
                    (format " [git:%s]" branch)
                  "")
              "")
            (if (= (user-uid) 0) "\n# " "\n$ "))))

(use-package pcmpl-args
  :after eshell
  :demand t)

(use-package eshell-syntax-highlighting
  :after eshell
  :demand t
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package eat
  :ensure nil
  ;; :vc (:url "https://codeberg.org/akib/emacs-eat")
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-enable-yank-to-terminal t)
  :init
  (unless (package-installed-p 'emacs-eat)
    (package-vc-install "https://codeberg.org/akib/emacs-eat"))
  :hook ((eshell-load . eat-eshell-mode)
         (eshell-load . eat-eshell-visual-command-mode)))

(use-package vterm
  :unless IS-WIN
  :custom (vterm-always-compile-module t))

(provide 'y-shell)
;;; y-shell.el ends here
