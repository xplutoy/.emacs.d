;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-11-29 12:57:20
;; Modified: <2023-12-01 01:54:22 yx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(use-package evil
  :hook
  (after-init . evil-mode)
  :init
  (setq
   evil-default-state 'emacs
   evil-move-beyond-eol t
   evil-want-integration t
   evil-want-keybinding nil
   evil-want-C-u-scroll nil
   evil-motion-state-modes nil
   evil-want-fine-undo t
   evil-undo-system 'undo-redo
   evil-respect-visual-line-mode t
   evil-disable-insert-state-bindings t)
  :bind (([remap evil-quit] . kill-this-buffer))
  :config
  (defvar yx-initial-evil-state-setup
    '((conf-mode . normal)
      (prog-mode . normal)
      (text-mode . insert)
      (color-rg-mode . emacs))
    "Default evil state per major mode.")
  (dolist (p yx-initial-evil-state-setup)
    (evil-set-initial-state (car p) (cdr p)))
  (keymap-unset evil-normal-state-map "C-.")
  (evil-define-key '(normal visual insert) 'global
    "\C-p"  'previous-line
    "\C-n"  'next-line
    "\C-a"  'crux-move-beginning-of-line
    "\C-e"  'end-of-line
    "\C-y"  'yank
    "\C-w"  'kill-region
    "\M-."  'xref-find-definitions
    )
  )

(use-package evil-collection
  :after evil
  :demand t
  :custom
  (evil-collection-mode-list nil)
  (evil-collection-setup-debugger-keys nil)
  :config
  (evil-collection-init
   '(consult
     sh-script
     yaml-mode
     python
     org
     imenu
     hungry-delete
     elisp-mode
     eglot
     unimpaired
     diff-hl ))
  )

(use-package evil-commentary
  :after evil
  :hook (prog-mode . evil-commentary-mode)
  )

(use-package vimish-fold)
(use-package evil-vimish-fold
  :after evil
  :hook (prog-mode . evil-vimish-fold-mode)
  )

(use-package evil-surround
  :after evil
  :hook ((prog-mode
          text-mode) . turn-on-evil-surround-mode)
  )

(provide 'init-evil)
;;; init-evil.el ends here
