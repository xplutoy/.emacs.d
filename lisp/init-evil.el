;;; -*- coding: utf-8; lexical-binding: t; -*-
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
 ("s-,"     . winner-undo)
 ("s-."     . winner-redo)
 ("s-<"     . previous-buffer)
 ("s->"     . next-buffer)
 ("s-k"     . crux-kill-other-buffers)
 ("s-<f10>" . toggle-frame-maximized)
 ("C-'"     . vterm-toggle-cd)
 ("C-;"     . crux-yx/toggle-eshell)
 ("C-."     . embark-act)
 ("C-,"     . embark-dwim)
 ("C-/"     . undo-only)
 ("M-i"     . consult-imenu-multi)
 ("M-z"     . vg-quick-zap-to-char)
 ("M-;"     . evil-commentary-line)
 ("M-r"     . consult-recent-file)
 ("M-o"     . ace-window)
 ("M-g ;"   . goto-last-change)
 ("M-s l"   . consult-focus-lines)
 ("M-s g"   . color-rg-search-symbol)
 ("M-s G"   . color-rg-search-input)
 ("M-s p"   . color-rg-search-symbol-in-project)
 ("M-s P"   . color-rg-search-input-in-project)
 ("M-s ,"   . color-rg-search-symbol-in-current-file)
 ("M-s ."   . color-rg-search-input-in-current-file)
 ("C-c ;"   . flyspell-correct-wrapper)
 ("C-c '"   . flyspell-correct-next)
 ("C-c k"   . kill-buffer-and-window)
 ("C-c v"   . magit-file-dispatch)
 ("C-c V"   . magit-dispatch)
 ("C-c g"   . consult-ripgrep)
 ("C-c f"   . consult-find)
 ("C-c a"   . org-agenda)
 ("C-c c"   . org-capture)
 ("C-c b"   . org-switchb)
 ("C-c l"   . org-store-link)
 ("C-c n l" . org-roam-buffer-toggle)
 ("C-c n n" . org-roam-node-find)
 ("C-c n i" . org-roam-node-insert)
 ("C-c n c" . org-roam-capture)
 ("C-c n j" . org-roam-dailies-capture-today)
 ("C-c n t" . org-transclusion-add)
 ("C-c n T" . org-transclusion-add-all)
 ("C-c t t" . hl-todo-occur)
 ("C-c t n" . hl-todo-next)
 ("C-c t p" . hl-todo-previous)
 ("C-c t i" . hl-todo-insert)
 ("C-x c j" . citre-jump)
 ("C-x c k" . citre-jump-back)
 ("C-x c p" . citre-peek)
 ("C-x c a" . citre-ace-peek)
 ("C-x c r" . citre-peek-references)
 ("C-x c u" . citre-update-this-tags-file)
 ("C-h b"   . embark-bindings)
 ("C-h M"   . which-key-show-major-mode)
 ("C-h B"   . embark-bindings-at-point)
 )

;; %% evil
(use-package evil
  :init
  (setq
   evil-want-fine-undo t
   evil-want-C-u-scroll t
   evil-move-beyond-eol t
   evil-want-integration t
   evil-want-C-w-delete nil
   evil-want-keybinding nil
   evil-default-state 'emacs
   evil-undo-system 'undo-redo
   evil-respect-visual-line-mode t
   evil-disable-insert-state-bindings t
   )
  :hook
  (after-init . evil-mode)
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
    "\C-a"  'move-beginning-of-line
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
      "g f"  'consult-flymake
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
  :hook (prog-mode . evil-commentary-mode)
  )

(use-package vimish-fold)
(use-package evil-vimish-fold
  :hook (prog-mode . evil-vimish-fold-mode)
  )

;; %% end
(provide 'init-evil)
