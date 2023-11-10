;;; core-keymaps.el --- keymap  -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:00:08
;; Modified: <2023-11-10 20:19:57 yx>
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
  (key-chord-define-global "jl"     'avy-goto-line)
  (with-eval-after-load 'org
    (key-chord-define org-mode-map
                      "jh" 'avy-org-goto-heading-timer))
  )

(bind-keys
 ([remap move-beginning-of-line]        . crux-move-beginning-of-line) ; C-a
 ([remap goto-line]                     . consult-goto-line) ;M-g g
 ([remap switch-to-buffer]              . consult-buffer) ; C-x b
 ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
 ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
 ([remap yank-pop]                      . consult-yank-pop) ;M-y
 ([remap bookmark-jump]                 . consult-bookmark) ;C-x r b
 ([remap imenu]                         . consult-imenu) ;M-g i
 ([remap describe-function]             . helpful-callable) ; C-h f
 ([remap describe-key]                  . helpful-key) ; C-h k
 ([remap describe-command]              . helpful-command) ; C-h x
 ([remap describe-variable]             . helpful-variable) ; C-h v
 ([remap list-directory]                . zoxide-travel) ; C-x C-d
 ([remap dired-at-point]                . consult-dir) ; C-x d
 ([remap isearch-forward]               . consult-line) ; C-s
 )

(bind-keys
 ("<f10>"     . yx/transient-global-odd)
 ("s-<f10>"   . yx/transient-global-even)
 ("s-<return>" . toggle-frame-maximized)
 ("s-,"       . winner-undo)
 ("s-."       . winner-redo)
 ("s-]"       . ns-next-frame)
 ("s-["       . ns-prev-frame)
 ("M-]"       . tab-next)
 ("M-["       . tab-previous)
 ("s-k"       . crux-kill-other-buffers)
 ("C-;"       . crux-yx/toggle-eshell)
 ("C-:"       . vterm-toggle-cd)
 ("C-."       . embark-act)
 ("C-,"       . embark-dwim)
 ("C-/"       . undo-only)
 ("M-0"       . delete-window)
 ("M-#"       . consult-register-load)
 ("M-'"       . consult-register-store)
 ("C-M-#"     . consult-register)
 ("M-i"       . consult-imenu-multi)
 ("M-z"       . vg-quick-zap-up-to-char)
 ("M-;"       . evil-commentary-line)
 ("M-r"       . consult-recent-file)
 ("M-o"       . ace-window)
 ("M-g ;"     . goto-last-change)
 ("M-g M-f"   . consult-flymake)
 ("M-g M-e"   . consult-compile-error)
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
 ("C-c n C-f" . denote-org-dblock-insert-links)
 ("C-c n b"   . denote-find-backlink)
 ("C-c n C-b" . denote-org-dblock-insert-backlinks)
 ("C-c n t"   . org-transclusion-add)
 ("C-c n C-t" . org-transclusion-add-all)
 ("C-h b"     . embark-bindings)
 ("C-h M"     . which-key-show-major-mode)
 ("C-h B"     . embark-bindings-at-point)
 ("C-x p b"   . consult-project-buffer)
 )

;; %% transient key
(transient-define-prefix yx/transient-global-even ()
  "Global transient for infrequently used functions."
  [["Size"
    ("0" "text-scale-adjust"   text-scale-adjust)
    ("+" "text-scale-increase" text-scale-increase)
    ("-" "text-scale-decrease" text-scale-decrease)
    ]]
  )

(transient-define-prefix yx/transient-global-odd ()
  "Global transient for frequently used functions."
  [["Misc"
    ("a" "agenda" org-agenda-list)
    ("o" "crux-open-with" crux-open-with)
    ("n" "evil-buffer-new" evil-buffer-new)
    ("v" "magit-file-dispatch" magit-file-dispatch)
    ("z" "zoom" zoom)
    ("%" "query-replace-regexp" query-replace-regexp)
    ("!" "shell-command" shell-command)
    ("C" "desktop-clear" desktop-clear)
    ("D" "crux-delete-file-and-buffer" crux-delete-file-and-buffer)
    ("G" "magit-status" magit-status)
    ("V" "magit-dispatch" magit-dispatch)
    ("S" "Scratch" scratch-buffer)
    ("I" "Clock In" yx/org-clock-in)
    ("T" "Toggle" consult-minor-mode-menu)
    ("R" "rename-visited-file" rename-visited-file)
    ("K" "crux-kill-other-buffers" crux-kill-other-buffers)
    ("E" "crux-sudo-edit" crux-sudo-edit)]
   ["TT"
    ("t0" "tab-close" tab-close)
    ("t1" "tab-close-other" tab-close-other)
    ("t2" "tab-new" tab-new)
    ("tl" "toggle-command-log-buffer" clm/toggle-command-log-buffer)
    ("f0" "delete-frame" delete-frame)
    ("f1" "delete-other-frames" delete-other-frames)
    ("f2" "make-frame" make-frame)
    ("ga" "consult-org-agenda" consult-org-agenda)
    ("ge" "consult-compile-error" consult-compile-error)
    ("gf" "consult-flymake" consult-flymake)
    ("si" "consult-emacs-info" yx/consult-emacs-info)]
   ]
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
      (prog-mode . normal)
      (text-mode . insert)
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
(provide 'core-keymaps)
;;; core-keymaps.el ends here
