;;; init-evil.el --- keymap  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:00:08
;; Modified: <2023-09-12 17:39:11 yx>
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
  (key-chord-define-global "df"     'desktop-clear)
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
 ([remap describe-command]              . helpful-command) ; C-h x
 ([remap describe-variable]             . helpful-variable) ; C-h v
 ([remap list-directory]                . zoxide-cd) ; C-x C-d
 ([remap dired-at-point]                . consult-dir) ; C-x d
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

;; %% transient key
(transient-define-prefix yx/transient-global-even ()
  "Global transient for infrequently used functions."
  [[]]
  )

(transient-define-prefix yx/transient-global-odd ()
  "Global transient for frequently used functions."
  [["Misc"
    ("o" "crux-open-with" crux-open-with)
    ("%" "query-replace-regexp" query-replace-regexp)
    ("!" "shell-command" shell-command)
    ("D" "crux-delete-file-and-buffer" crux-delete-file-and-buffer)
    ("R" "rename-visited-file" rename-visited-file)
    ("K" "crux-kill-other-buffers" crux-kill-other-buffers)
    ("E" "crux-sudo-edit" crux-sudo-edit)
    ]
   ["Jumping"
    ("j l" "consult-line" consult-line)
    ("j L" "consult-line-multi" consult-line-multi)
    ("j o" "consult-outline" consult-outline)]
   ]
  )

(transient-define-prefix yx/transient-dired ()
  "Dired commands."
  [["Action"
    ("RET" "Open file"            dired-find-file)
    ("o"   "Open in other window" dired-find-file-other-window)
    ("C-o" "Open in other window (No select)" dired-display-file)
    ("v"   "Open file (View mode)"dired-view-file)
    ("="   "Diff"                 dired-diff)
    ("e"   "wdired"               wdired-change-to-wdired-mode)
    ("w"   "Copy filename"        dired-copy-filename-as-kill)
    ("W"   "Open in browser"      browse-url-of-dired-file)
    ("y"   "Show file type"       dired-show-file-type)]
   ["Attribute"
    ("R"   "Rename"               dired-do-rename)
    ("G"   "Group"                dired-do-chgrp)
    ("M"   "Mode"                 dired-do-chmod)
    ("O"   "Owner"                dired-do-chown)
    ("T"   "Timestamp"            dired-do-touch)]
   ["Navigation"
    ("j"   "Goto file"            dired-goto-file)
    ("+"   "Create directory"     dired-create-directory)
    ("<"   "Jump prev directory"  dired-prev-dirline)
    (">"   "Jump next directory"  dired-next-dirline)
    ("^"   "Move up directory"    dired-up-directory)]
   ["Display"
    ("g" "  Refresh buffer"       revert-buffer)
    ("l" "  Refresh file"         dired-do-redisplay)
    ("k" "  Remove line"          dired-do-kill-lines)
    ("s" "  Sort"                 dired-sort-toggle-or-edit)
    ("(" "  Toggle detail info"   dired-hide-details-mode)
    ("i" "  Insert subdir"        dired-maybe-insert-subdir)
    ("$" "  Hide subdir"          dired-hide-subdir)
    ("M-$" "Hide subdir all"      dired-hide-subdir)]
   ["Act on Marked"
    ("x"   "Do action"            dired-do-flagged-delete)
    ("C"   "Copy"                 dired-do-copy)
    ("D"   "Delete"               dired-do-delete)
    ("S"   "Symlink"              dired-do-symlink)
    ("H"   "Hardlink"             dired-do-hardlink)
    ("P"   "Print"                dired-do-print)
    ("A"   "Find"                 dired-do-find-regexp)
    ("Q"   "Replace"              dired-do-find-regexp-and-replace)
    ("B"   "Elisp bytecompile"    dired-do-byte-compile)
    ("L"   "Elisp load"           dired-do-load)
    ("X"   "Shell command"        dired-do-shell-command)
    ("Z"   "Compress"             dired-do-compress)
    ("c"   "Compress to"          dired-do-compress-to)
    ("!"   "Shell command"        dired-do-shell-command)
    ("&"   "Async shell command"  dired-do-async-shell-command)]]
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
(provide 'init-evil)
;;; init-evil.el ends here
