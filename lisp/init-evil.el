;;; init-evil.el --- keymap  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:00:08
;; Modified: <2023-09-08 17:08:57 yx>
;; Licence: GPLv3

;;; Commentary:

;; keymap

;;; Code:

;; %% simple
(defalias 'qrr ''query-replace-regexp)

;; %% 全局按键
(use-package key-chord
  :init
  (key-chord-mode 1)
  (key-chord-define-global "zz"     'zoom)
  (key-chord-define-global "jk"     'scratch-buffer)
  (key-chord-define-global "jj"     'avy-goto-char-timer)
  (key-chord-define-global "ji"     'consult-imenu)
  (key-chord-define-global "jl"     'avy-goto-line)
  (with-eval-after-load 'org
    (key-chord-define org-mode-map
                    "jh" 'avy-org-goto-heading-timer))
  )

(bind-keys
 ([remap move-beginning-of-line]        . crux-move-beginning-of-line)
 ([remap goto-line]                     . consult-goto-line) ;M-g g
 ([remap switch-to-buffer]              . consult-buffer)
 ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
 ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
 ([remap yank-pop]                      . consult-yank-pop) ;M-y
 ([remap apropos]                       . consult-apropos)
 ([remap bookmark-jump]                 . consult-bookmark) ;C-x r b
 ([remap imenu]                         . consult-imenu) ;M-g i
 ([remap describe-function]             . helpful-callable) ; C-h f
 ([remap describe-key]                  . helpful-key) ; C-h k
 ([remap describe-variable]             . helpful-variable) ; C-h v
 )

(bind-keys
 ("<f10>"     . org-agenda-list)
 ("s-,"       . winner-undo)
 ("s-."       . winner-redo)
 ("s-]"       . ns-next-frame)
 ("s-["       . ns-prev-frame)
 ("M-]"       . tab-next)
 ("M-["       . tab-previous)
 ("s-k"       . crux-kill-other-buffers)
 ("C-'"       . vterm-toggle-cd)
 ("C-;"       . crux-yx/toggle-eshell)
 ("C-."       . embark-act)
 ("C-,"       . embark-dwim)
 ("C-/"       . undo-only)
 ("M-#"       . consult-register-load)
 ("M-'"       . consult-register-store)
 ("C-M-#"     . consult-register)
 ("M-i"       . consult-imenu-multi)
 ("M-z"       . vg-quick-zap-up-to-char)
 ("M-;"       . evil-commentary-line)
 ("M-r"       . consult-recent-file)
 ("M-o"       . ace-window)
 ("M-g ;"     . goto-last-change)
 ("M-g f"     . consult-flymake)
 ("M-g o"     . consult-outline)
 ("M-g m"     . consult-mark)
 ("M-g M-m"   . consult-global-mark)
 ("M-s l"     . consult-focus-lines)
 ("M-s g"     . color-rg-search-symbol)
 ("M-s M-g"   . color-rg-search-input)
 ("M-s p"     . color-rg-search-symbol-in-project)
 ("M-s M-p"   . color-rg-search-input-in-project)
 ("M-s s"     . color-rg-search-symbol-in-current-file)
 ("M-s M-s"   . color-rg-search-input-in-current-file)
 ("C-c ;"     . flyspell-correct-wrapper)
 ("C-c '"     . flyspell-correct-next)
 ("C-c k"     . kill-buffer-and-window)
 ("C-c C-k"   . crux-kill-other-buffers)
 ("C-c v"     . magit-file-dispatch)
 ("C-c C-v"   . magit-dispatch)
 ("C-c C-d"   . helpful-at-point)
 ("C-c g"     . consult-ripgrep)
 ("C-c f"     . consult-find)
 ("C-c a"     . org-agenda)
 ("C-c c"     . org-capture)
 ("C-c b"     . org-switchb)
 ("C-c l"     . org-store-link)
 ("C-c w o"   . burly-open-bookmark)
 ("C-c w r"   . burly-reset-tab)
 ("C-c w w"   . burly-bookmark-windows)
 ("C-c w f"   . burly-bookmark-frames)
 ("C-c n c"   . denote)
 ("C-c n t"   . denote-template)
 ("C-c n C-c" . citar-create-note)
 ("C-c n C-o" . citar-denote-dwim)
 ("C-c n n"   . denote-open-or-create)
 ("C-c n i"   . denote-link-or-create)
 ("C-c n l"   . denote-backlinks)
 ("C-c n f"   . denote-find-link)
 ("C-c n b"   . denote-find-backlink)
 ("C-c n t"   . org-transclusion-add)
 ("C-c n C-t" . org-transclusion-add-all)
 ("C-x c j"   . citre-jump)
 ("C-x c k"   . citre-jump-back)
 ("C-x c p"   . citre-peek)
 ("C-x c a"   . citre-ace-peek)
 ("C-x c r"   . citre-peek-references)
 ("C-x c u"   . citre-update-this-tags-file)
 ("C-h b"     . embark-bindings)
 ("C-h M"     . which-key-show-major-mode)
 ("C-h B"     . embark-bindings-at-point)
 ("C-x p b"   . consult-project-buffer)
 )

;; %% evil
(use-package evil
  :hook
  (after-init . evil-mode)
  :init
  (setq
   evil-move-beyond-eol t
   evil-want-keybinding nil
   evil-want-integration t
   evil-want-C-u-scroll nil
   evil-default-state 'emacs
   evil-motion-state-modes nil
   evil-want-fine-undo t
   evil-undo-system 'undo-redo
   evil-respect-visual-line-mode t
   evil-disable-insert-state-bindings t
   )
  :config
  (defvar yx-initial-evil-state-setup
    '((conf-mode . normal)
      (text-mode . normal)
      (prog-mode . normal)
      (color-rg-mode . emacs))
    "Default evil state per major mode.")
  (dolist (p yx-initial-evil-state-setup)
    (evil-set-initial-state (car p) (cdr p)))
  (evil-define-key 'normal org-mode-map
    [tab]   'org-cycle
    [S-tab] 'org-shifttab)
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
  ;; leader-key <SPC>
  (evil-define-key '(normal visual) 'global
    (kbd "SPC")
    (define-keymap
      "SPC"  'execute-extended-command-for-buffer
      "!"    'shell-command
      "/"    'consult-ripgrep

      "f O"  'crux-open-with
      "f r"  'rename-visited-file
      "f F"  'crux-sudo-edit
      "f E"  'crux-reopen-as-root
      "f K"  'crux-kill-other-buffers
      "f D"  'crux-delete-file-and-buffer

      "j j"  'evil-avy-goto-char-timer
      "j w"  'evil-avy-goto-word-or-subword-1
      "j l"  'consult-line
      "j L"  'consult-line-multi
      "j i"  'consult-imenu
      "j o"  'consult-outline
      "j m"  'consult-mark
      "j M"  'consult-global-mark

      "h h"  'symbol-overlay-put
      "h c"  'symbol-overlay-remove-all
      "h t"  'hl-todo-occur
      )
    )
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

;; %% end
(provide 'init-evil)
;;; init-evil.el ends here
