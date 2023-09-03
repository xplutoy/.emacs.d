;;; init-evil.el --- keymap  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:00:08
;; Modified: <2023-09-03 16:01:57 yx>
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
  (key-chord-define-global "jj"     'avy-goto-char-timer)
  (key-chord-define-global "ji"     'consult-imenu)
  (key-chord-define-global "jl"     'avy-goto-line)
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
 )

(bind-keys
 ("<f10>"     . org-agenda-list)
 ("s-,"       . winner-undo)
 ("s-."       . winner-redo)
 ("s-<right>" . ns-next-frame)
 ("s-<left>"  . ns-prev-frame)
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
 ("M-z"       . vg-quick-zap-to-char)
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
 ("M-s P"     . color-rg-search-input-in-project)
 ("M-s ,"     . color-rg-search-symbol-in-current-file)
 ("M-s ."     . color-rg-search-input-in-current-file)
 ("C-c ;"     . flyspell-correct-wrapper)
 ("C-c '"     . flyspell-correct-next)
 ("C-c k"     . kill-buffer-and-window)
 ("C-c C-k"   . crux-kill-other-buffers)
 ("C-c v"     . magit-file-dispatch)
 ("C-c C-v"   . magit-dispatch)
 ("C-c g"     . consult-ripgrep)
 ("C-c f"     . consult-find)
 ("C-c a"     . org-agenda)
 ("C-c c"     . org-capture)
 ("C-c b"     . org-switchb)
 ("C-c l"     . org-store-link)
 ("C-c n l"   . org-roam-buffer-toggle)
 ("C-c n n"   . org-roam-node-find)
 ("C-c n i"   . org-roam-node-insert)
 ("C-c n c"   . org-roam-capture)
 ("C-c n j"   . org-roam-dailies-capture-today)
 ("C-c n C-j" . org-roam-dailies-goto-today)
 ("C-c n J"   . org-roam-dailies-goto-date)
 ("C-c n t"   . org-transclusion-add)
 ("C-c n T"   . org-transclusion-add-all)
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

      "f f"  'find-file
      "f t"  'find-file-other-tab
      "f o"  'find-file-other-window
      "f O"  'crux-open-with
      "f r"  'rename-visited-file
      "f F"  'crux-sudo-edit
      "f E"  'crux-reopen-as-root
      "f k"  'kill-current-buffer
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

      "g g"  'consult-xref
      "g s"  'consult-eglot-symbols
      "g d"  'xref-find-definitions
      "g r"  'xref-find-references
      "g o"  'xref-find-definitions-other-window

      "e f"  'eglot-format
      "e r"  'eglot-rename
      "e h"  'eglot-help-at-point
      "e a"  'eglot-code-actions
      "e n"  'flymake-goto-next-erroer
      "e p"  'flymake-goto-prev-error
      "e b"  'flymake-show-buffer-diagnostics
      "e B"  'flymake-show-project-diagnostics

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
  :hook (prog-mode . turn-on-evil-surround-mode)
  )

;; %% end
(provide 'init-evil)
;;; init-evil.el ends here
