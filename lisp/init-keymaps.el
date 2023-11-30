;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:00:08
;; Modified: <2023-12-01 05:08:55 yx>
;; Licence: GPLv3

;;; Commentary:

;; keymap

;;; Code:

;; %% simple
(defalias 'qrr ''query-replace-regexp)

;; %% 全局按键
;; This avoids the ugly accidentally action of scaling text with using the trackpad
(unbind-key "<C-wheel-up>")
(unbind-key "<C-wheel-down>")

(use-package key-chord
  :init
  (key-chord-mode 1)
  (key-chord-define-global "zz"     'zoom)
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
 ([remap list-buffers]                  . ibuffer) ; C-x C-b
 ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
 ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
 ([remap project-switch-to-buffer]      . consult-project-buffer) ; C-x p b
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
 ("s-SPC"     . sis-switch)
 ("s-,"       . winner-undo)
 ("s-."       . winner-redo)
 ("s-j"       . avy-goto-char-timer)
 ("s-s"       . dirvish-side)
 ("s-r"       . consult-recent-file)
 ("s-<right>" . ns-next-frame)
 ("s-<left>"  . ns-prev-frame)
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
 ("M-z"       . vg-quick-zap-up-to-char)
 ("M-;"       . evil-commentary-line)
 ("M-o"       . ace-window)
 ("M-g ;"     . goto-last-change)
 ("M-g a"     . consult-org-agenda)
 ("M-g M"     . consult-man)
 ("M-g I"     . consult-info)
 ("M-g M-i"   . consult-imenu-multi)
 ("M-g M-f"   . consult-flymake)
 ("M-g M-e"   . consult-compile-error)
 ("M-g o"     . consult-outline)
 ("M-g k"     . consult-kmacro)
 ("M-g m"     . consult-mark)
 ("M-g M-m"   . consult-global-mark)
 ("M-s f"     . consult-fd)
 ("M-s l"     . consult-line)
 ("M-s M l"   . consult-line-multi)
 ("M-s k"     . consult-focus-lines)
 ("M-s M-k"   . consult-keep-lines)
 ("M-s g"     . consult-grep)
 ("M-s M-g"   . consult-git-grep)
 ("M-s r"     . consult-ripgrep)
 ("M-s s"     . color-rg-search-symbol)
 ("M-s M-s"   . color-rg-search-input)
 ("M-s p"     . color-rg-search-symbol-in-project)
 ("M-s M-p"   . color-rg-search-input-in-project)
 ("C-c ;"     . flyspell-correct-wrapper)
 ("C-c '"     . flyspell-correct-next)
 ("C-c k"     . kill-buffer-and-window)
 ("C-c C-k"   . crux-kill-other-buffers)
 ("C-c v"     . magit-file-dispatch)
 ("C-c C-v"   . magit-dispatch)
 ("C-c C-d"   . helpful-at-point)
 ("C-c a"     . org-agenda)
 ("C-c c"     . org-capture)
 ("C-c l"     . org-store-link)
 ("C-c b"     . tabspaces-switch-to-buffer)
 ("C-c d"     . devdocs-lookup)
 ("C-c f"     . dirvish-fd)
 ("C-c / o"   . browse-url-at-point)
 ("C-c / a"   . ace-link-addr)
 ("C-c / l"   . ace-link)
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
 ;; ("C-x p b"   . consult-project-buffer)
 )

;; %% transient key
(with-eval-after-load 'transient
  (transient-bind-q-to-quit)
  (keymap-set transient-map "<escape>" 'transient-quit-all))

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
  [["]-"
    ("a" "agenda" org-agenda-list)
    ("o" "crux-open-with" crux-open-with)
    ("n" "evil-buffer-new" evil-buffer-new)
    ("s" "scratch-buffer" scratch-buffer)
    ("v" "magit-file-dispatch" magit-file-dispatch)
    ("%" "query-replace-regexp" query-replace-regexp)
    ("!" "shell-command" shell-command)]
   ["[-"
    ("C" "desktop-clear" desktop-clear)
    ("D" "crux-delete-file-and-buffer" crux-delete-file-and-buffer)
    ("G" "magit-status" magit-status)
    ("V" "magit-dispatch" magit-dispatch)
    ("I" "Clock In" yx/org-clock-in)
    ("T" "consult-minor-mode-menu" consult-minor-mode-menu)
    ("L" "toggle-command-log-buffer" clm/toggle-command-log-buffer)
    ("R" "rename-visited-file" rename-visited-file)
    ("K" "crux-kill-other-buffers" crux-kill-other-buffers)
    ("E" "crux-sudo-edit" crux-sudo-edit)]
   ]
  )


(provide 'init-keymaps)
;;; init-keymaps.el ends here
