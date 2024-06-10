;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 12:00:36
;; Modified: <2024-06-10 12:57:53 yangx>
;; Licence: GPLv3

;;; Commentary:

;; 缓冲区、窗口、标签设置

;;; Code:
(use-package window
  :ensure nil
  :custom
  (even-window-sizes t)
  (window-sides-vertical nil)
  (split-width-threshold 120)
  (split-height-threshold 50)
  (switch-to-buffer-obey-display-actions t)
  (switch-to-buffer-in-dedicated-window nil)
  (switch-to-buffer-preserve-window-point t)
  (switch-to-prev-buffer-skip 'visible)
  (switch-to-prev-buffer-skip-regexp "^\\*\\|^magit.*")
  (display-comint-buffer-action '(display-buffer-at-bottom
                                  (inhibit-same-window . nil)))
  :config
  (setq display-buffer-alist
        `(;; no window
          ("\\`\\*Async Shell Command\\*\\'"
           (display-buffer-no-window))
          ("\\`\\*\\(Warnings\\|Compile-Log\\|Org Links\\)\\*\\'"
           (display-buffer-no-window)
           (allow-no-window . t))
          ;; bottom side window
          ("\\*Org \\(Select\\|Note\\)\\*"
           (display-buffer-in-side-window)
           (dedicated . t)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((mode-line-format . none))))
          ;; bottom buffer (NOT side window)
          ((or . ((derived-mode . flymake-diagnostics-buffer-mode)
                  (derived-mode . flymake-project-diagnostics-mode)
                  (derived-mode . messages-buffer-mode)
                  (derived-mode . backtrace-mode)
                  "\\*\\(tldr\\|quickrun\\|diff-hl\\).*"
                  "\\*\\(Agenda Commands\\|Dictionary\\|wclock\\)"))
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (window-height . 0.45)
           (dedicated . t)
           (preserve-size . (t . t)))
          ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))
          ("\\(\\*Capture\\*\\|CAPTURE-.*\\)"
           (display-buffer-reuse-mode-window display-buffer-below-selected))
          ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . 0.1)
           (dedicated . t)
           (preserve-size . (t . t)))
          ("\\*\\(Output\\|Register Preview\\).*"
           (display-buffer-reuse-mode-window display-buffer-at-bottom))
          ("\\`\\(\\*Calendar\\|\\*Bookmark\\|\\*stardict\\*\\)"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (dedicated . t)
           (window-height . fit-window-to-buffer))
          ((or . ((major-mode . help-mode)
                  (major-mode . helpful-mode)
                  (major-mode . apropos-mode)))
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (mode . (help-mode helpful-mode apropos-mode))
           (window-height . 0.45))
          ((or . ((derived-mode . occur-mode)
                  (derived-mode . grep-mode)
                  (derived-mode . color-rg-mode)
                  (derived-mode . Buffer-menu-mode)
                  (derived-mode . log-view-mode)
                  "\\*\\(Occur\\|color-rg\\)\\*"
                  "\\*\\(|Buffer List\\|vc-change-log\\|eldoc.*\\).*"
                  yx/window-shell-or-term-p))
           (yx/window-display-buffer-below-or-pop)
           (mode . (occur-mode grep-mode color-rg-mode))
           (window-height . 0.45))
          ;; full-frame
          ("\\*\\(Ibuffer\\|Org Agenda\\|Proced\\|info\\)\\*"
           (display-buffer-full-frame)))))

(use-package ibuffer
  :ensure nil
  :hook ((ibuffer-mode . ibuffer-auto-mode)
         (ibuffer-mode . ibuffer-do-sort-by-recency))
  :custom
  (ibuffer-expert t)
  (ibuffer-display-summary nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-default-sorting-mode 'major-mode))

(use-package ibuffer-vc
  :init
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))

(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :custom
  (winner-dont-bind-my-keys t)
  (winner-boring-buffers-regexp "^\\*")
  :config
  (setq buffer-quit-function 'winner-undo))

(use-package tab-line
  :ensure nil
  :custom
  (tab-line-switch-cycling t)
  (tab-line-close-button-show 'selected)
  (tab-line-close-tab-function 'kill-buffer)
  (tab-line-tab-name-function #'tab-line-tab-name-truncated-buffer)
  (tab-line-tab-name-truncated-max 25)
  (tab-line-tabs-function #'tab-line-tabs-buffer-groups)
  (tab-line-tabs-buffer-group-function #'yx/tab-line-buffer-group)
  (tab-line-tabs-buffer-list-function #'yx/tab-line-tabs-buffer-list)
  (tab-line-exclude-modes '(completion-list-mode
                            special-mode
                            lisp-interaction-mode
                            messages-buffer-mode)))

(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-mode)
  :custom
  (tab-bar-show 1)
  (tab-bar-auto-width nil)
  (tab-bar-format '(tab-bar-format-menu-bar
                    tab-bar-format-tabs-groups
                    tab-bar-format-align-right
                    tab-bar-format-global))
  (tab-bar-tab-hints t)
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-tab-name-truncated-max 20)
  (tab-bar-select-tab-modifiers '(super))
  :config
  (tab-bar-history-mode +1)
  (keymap-unset tab-bar-map "<wheel-up>")
  (keymap-unset tab-bar-map "<wheel-down>"))

(use-package sr-speedbar
  :ensure nil
  :custom
  (speedbar-use-images nil)
  (sr-speedbar-width 28)
  (sr-speedbar-max-width 40)
  (sr-speedbar-skip-other-window-p t)
  (sr-speedbar-use-frame-root-window t)
  :config
  (keymap-set speedbar-mode-map "q" #'sr-speedbar-close))

(use-package windmove
  :ensure nil
  :defer 2
  :custom
  (windmove-wrap-around t)
  (windmove-create-window t)
  :config
  (windmove-delete-default-keybindings) ; C-x S-left
  (windmove-default-keybindings 'control)
  ;; (windmove-display-default-keybindings '(shift control)) # dont work
  (windmove-swap-states-default-keybindings '(shift control)))

(use-package ace-window
  :custom
  (aw-scope 'frame)
  (aw-background nil)
  (aw-dispatch-always t)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package burly
  :hook (after-init . burly-tabs-mode))

(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :custom
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-use-filtered-buffers-as-default nil)
  (tabspaces-default-tab "Main")
  (tabspaces-session t)
  (tabspaces-session-auto-restore nil)
  (tabspaces-session-file (nol-expand-var "tabsession.el")))

(use-package zoom
  :custom
  (zoom-size '(0.618 . 0.618)))

(use-package popper
  :hook (after-init . popper-mode)
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-echo-lines 1)
  (popper-display-control nil)
  (popper-window-height 0.4)
  (popper-group-function #'popper-group-by-project)
  (popper-reference-buffers '("\\*quickrun\\*$"
                              "\\*Embark Collect.*\\*"
                              "\\*Embark Export.*\\*"
                              "\\*Flymake diagnostics.*\\*"
                              "^\\*shell.*\\*$"  shell-mode
                              "^\\*term.*\\*$"   term-mode
                              "^\\*vterm.*\\*$"  vterm-mode
                              grep-mode
                              occur-mode
                              color-rg-mode
                              bookmark-bmenu-mode
                              comint-mode
                              compilation-mode
                              devdocs-mode))
  :config
  (popper-echo-mode +1))

(provide 'y-layout)
;;; y-layout.el ends here
