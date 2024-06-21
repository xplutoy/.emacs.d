;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 12:08:47
;; Modified: <2024-06-21 10:29:49 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(keymap-global-unset "M-m")   ; as major mode transient
(keymap-global-unset "M-c")   ; as programing leader-key
(keymap-global-unset "M-l")   ; as major mode leader-key

;; unset boring keybindings
(keymap-global-set "<pinch>" #'ignore)
(keymap-global-unset "C-<wheel-up>" #'ignore)
(keymap-global-unset "C-<wheel-down>" #'ignore)

(defvar-keymap yx/file-prefix-map
  :doc "Prefix map for file."
  :name "File"
  "f"   #'find-file
  "M-f" #'ffap
  "d"   #'consult-dir
  "o"   #'x-window-other-find-file
  "p"   #'find-file-at-point
  "t"   #'find-file-other-tab
  "r"   #'consult-recent-file
  "R"   #'rename-visited-file
  "D"   #'x-simple-delete-file-and-buffer)
(keymap-global-set "C-c f" yx/file-prefix-map)

(defvar-keymap yx/buffer-prefix-map
  :doc "Prefix map for buffer"
  :name "Buffer"
  "b" #'tabspaces-switch-to-buffer
  "s" #'scratch-buffer
  "k" #'kill-buffer-and-window
  "C" #'desktop-clear)

(keymap-global-set "C-c b" yx/buffer-prefix-map)

(defvar-keymap x-window-prefix-map
  :doc "Prefix map for window and workspace"
  :name "Workspace"
  "u"   #'winner-undo
  "r"   #'winner-redo
  "C-b" #'burly-open-bookmark
  "C-t" #'burly-reset-tab
  "M-w" #'burly-bookmark-windows
  "M-f" #'burly-bookmark-frames)

(keymap-global-set "C-c w" x-window-prefix-map)
(defvar-keymap yx/note-prefix-map
  :doc "Prefix map for note taking"
  :name "Note"
  "c"   #'denote
  "t"   #'denote-template
  "n"   #'denote-open-or-create
  "i"   #'denote-link-or-create
  "m"   #'denote-menu-list-notes
  "g"   #'consult-denote-grep
  "f"   #'consult-denote-find
  "C-l" #'denote-backlinks
  "C-f" #'denote-find-link
  "C-b" #'denote-find-backlink
  "M-f" #'denote-org-dblock-insert-links
  "M-b" #'denote-org-dblock-insert-backlinks
  "C-c" #'citar-create-note
  "C-d" #'citar-denote-dwim)

(keymap-global-set "C-c n" yx/note-prefix-map)

(defvar-keymap yx/ctrl-c-q-prefix-map
  :doc "Prefix map for `C-c q'"
  :name "Query"
  "a"   #'org-ql-find-in-agenda
  "d"   #'org-ql-find-in-org-directory
  "s"   #'org-ql-search
  "v"   #'org-ql-view
  "k"   #'which-key-show-full-major-mode
  "C-a" #'ace-link-addr
  "C-l" #'ace-link)

(keymap-global-set "C-c q" yx/ctrl-c-q-prefix-map)

(defvar-keymap yx/ctrl-c-t-prefix-map
  :doc "Prefix map for toggle mirror mode or others"
  :name "Toggle"
  "f"   #'follow-mode
  "d"   #'drag-stuff-mode
  "l"   #'flymake-mode
  "s"   #'flyspell-mode
  "e"   #'toggle-debug-on-error
  "C-l" #'clm/toggle-command-log-buffer
  "C-f" #'toggle-frame-maximized)

(keymap-global-set "C-c t" yx/ctrl-c-t-prefix-map)

(bind-keys :map global-map
           :prefix-map yx/ctrl-z-prefix-map
           :prefix "C-z"
           ("."   . repeat)
           (":"   . vaper-ex)
           ("f"   . follow-delete-other-windows-and-split)
           ("a"   . org-agenda-list)
           ("C-a" . org-agenda)
           ("c"   . org-capture)
           ("l"   . org-store-link)
           ("d"   . dirvish-side)
           ("C-d" . dirvish-quick-access)
           ("s"   . scratch-buffer)
           ("C-s" . x-scratch-buffer)
           ("z"   . zoom)
           ("g"   . gptel)
           ("C-c" . gptel-send)
           ("/ /" . webjump)
           ("/ o" . browse-url-at-point))

(bind-keys :map global-map
           :prefix-map yx/ctrl-m-prefix-map
           :prefix "<f1>"
           ("a"   . casual-avy-tmenu)
           ("e"   . yx/transient-emms))

(bind-keys ([remap move-beginning-of-line]        . x-simple-begin-of-line-dwim) ; C-a
           ([remap goto-line]                     . consult-goto-line)           ;M-g g
           ([remap switch-to-buffer]              . consult-buffer)              ; C-x b
           ([remap other-window]                  . x-window-other-mru)         ; C-x o
           ([remap list-buffers]                  . ibuffer)                 ; C-x C-b
           ([dabbrev-expand]                      . hippie-expand)           ; M-/
           ([remap repeat-complex-command]        . consult-complex-command) ; C-x M-:
           ([remap switch-to-buffer-other-window] . consult-buffer-other-window) ; C-x 4 b
           ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame) ; C-x 5 b
           ([remap project-switch-to-buffer]      . consult-project-buffer)     ; C-x p b
           ([remap yank-pop]                      . consult-yank-pop)           ;M-y
           ([remap bookmark-jump]                 . consult-bookmark)           ;C-x r b
           ([remap imenu]                         . consult-imenu)              ;M-g i
           ([remap describe-function]             . helpful-callable)           ; C-h f
           ([remap describe-key]                  . helpful-key)                ; C-h k
           ([remap describe-command]              . helpful-command)            ; C-h x
           ([remap describe-variable]             . helpful-variable)           ; C-h v
           ([remap list-directory]                . dirvish)                    ; C-x C-d
           ([remap dired-at-point]                . consult-dir)                ; C-x d
           ([remap keyboard-quit]                 . yx/keyboard-quit-dwim)      ; C-g
           ([remap kill-buffer]                   . x-simple-kill-buffer-current) ; C-x k
           ([remap save-buffers-kill-emacs]       . delete-frame)               ; s-q
           ([remap open-line]                     . x-simple-new-line-below)   ; C-o
           ([remap fill-paragraph]                . x-simple-fill-dwim)        ; M-q
           ([remap comment-dwim]                  . x-simple-comment-dwim)     ; M-;
           ([remap upcase-word]                   . upcase-dwim)                ; M-u
           ([remap downcase-word]                 . downcase-dwim)              ; M-l
           ([remap capitalize-word]               . capitalize-dwim)            ; M-c
           ([remap default-indent-new-line]       . avy-goto-word-1)            ; M-j
           ([remap goto-char]                     . avy-goto-char-timer)        ; M-g c
           ([remap text-scale-adjust]             . global-text-scale-adjust)   ; C-x C-+
           ([remap global-text-scale-adjust]      . text-scale-adjust) ; C-x C-M-+
           ([remap isearch-forward-regexp]        . x-window-other-isearch-forward) ; C-M-s
           ([remap isearch-backward-regexp]       . x-window-other-isearch--backward) ; C-M-s
           ([set-selective-display]               . x-simple-selective-display))    ; C-x $
(bind-keys ("C-;"       . iedit-mode)
           ("C-."       . embark-act)
           ("C-,"       . embark-dwim)
           ("C-/"       . undo-only)
           ("C-M-/"     . vundo)
           ("M-o"       . ace-window)
           ("M-r"       . consult-recent-file)
           ("C-#"       . consult-register-load)
           ("M-#"       . consult-register-store)
           ("C-M-#"     . consult-register)
           ("C-c #"     . consult-register)
           ("M-z"       . avy-zap-up-to-char-dwim)
           ("M-Z"       . avy-zap-to-char-dwim)
           ("M-g ;"     . goto-last-change)
           ("M-g M-;"   . goto-last-change-reverse)
           ("M-g a"     . consult-org-agenda)
           ("M-g M"     . consult-man)
           ("M-g I"     . consult-info)
           ("M-g M-i"   . consult-imenu-multi)
           ("M-g M-e"   . consult-compile-error)
           ("M-g o"     . consult-outline)
           ("M-g k"     . consult-kmacro)
           ("M-g m"     . consult-mark)
           ("M-g M-m"   . consult-global-mark)
           ("M-g l"     . avy-goto-line)
           ("M-s t"     . yx/hl-todo-rg-project)
           ("M-s M-t"   . hl-todo-occur)
           ("M-s f"     . consult-fd)
           ("M-s M-f"   . dirvish-fd)
           ("M-s l"     . consult-line)
           ("M-s M l"   . consult-line-multi)
           ("M-s k"     . consult-focus-lines)
           ("M-s M-k"   . consult-keep-lines)
           ("M-s g"     . consult-grep)
           ("M-s M-g"   . consult-git-grep)
           ("M-s r"     . consult-ripgrep)
           ("M-s s"     . color-rg-search-input)
           ("M-s M-s"   . color-rg-search-symbol)
           ("M-s p"     . color-rg-search-input-in-project)
           ("M-s M-p"   . color-rg-search-symbol-in-project)
           ("M-s d"     . bing-dict-brief)
           ("M-s M-d"   . stardict-define-at-point)
           ("C-c v"     . magit-file-dispatch)
           ("C-c C-v"   . magit-dispatch)
           ("C-c C-d"   . helpful-at-point)
           ("C-c d"     . duplicate-dwim)
           ("C-c s"     . x-scratch-buffer)
           ("C-c r"     . query-replace-regexp)
           ("C-c z"     . hs-toggle-hiding)
           ("C-c C-z"   . hs-show-all)
           ("C-x !"     . delete-other-windows-vertically)
           ("C-x a a"   . align)
           ("C-x a r"   . align-regexp)
           ("C-x w t"   . tab-window-detach)
           ("C-x w f"   . tear-off-window)
           ("C-x t R"   . burly-reset-tab)
           ("C-h b"     . embark-bindings)
           ("C-h C-m"   . which-key-show-full-major-mode)
           ("C-h B"     . embark-bindings-at-point)
           :map prog-mode-map
           ("<f5> "   . quickrun)
           ("M-<f5>"  . dape)
           ("M-c f"   . consult-flymake)
           ("M-c d"   . devdocs-lookup)
           ("M-c h"   . symbol-overlay-put)
           ("M-c ."   . citre-jump)
           ("M-c ,"   . citre-jump-back)
           ("M-c p"   . citre-peek)
           ("M-c M-s" . symbols-outline-show)
           ("M-c M-e" . consult-compile-error))

(set-register ?S '(buffer . "*scratch*"))
(set-register ?I `(file . ,(expand-file-name "init.el" user-emacs-directory)))

(provide 'y-keymap)
;;; y-keymap.el ends here
