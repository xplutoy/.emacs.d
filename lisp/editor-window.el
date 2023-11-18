;;; editor-window.el --- windows  -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:06:35
;; Modified: <2023-11-18 23:19:49 yx>
;; Licence: GPLv3

;;; Commentary:

;; window buffer frame

;;; Code:

;; %% @see http://yummymelon.com/devnull/using-bookmarks-in-emacs-like-you-do-in-web-browsers.html
(easy-menu-define yx/bookmarks-menu nil
  "Keymap for CC Bookmarks Menu"
  '("Bookmarks"
    ["Edit Bookmarks" list-bookmarks
     :help "Display a list of existing bookmarks."]
    ["--" nil]
    ["Add Bookmark…" bookmark-set-no-overwrite
     :help "Set a bookmark named NAME at the current location."]
    ["---" nil]
    ["Jump to Bookmark…" bookmark-jump
     :help "Jump to bookmark"]))

(easy-menu-add-item global-map '(menu-bar)
                    yx/bookmarks-menu
                    "Tools")

(define-key global-map [menu-bar edit bookmark] nil)

;; %% windows manager
(setq
 winner-dont-bind-my-keys t
 winner-boring-buffers-regexp "^\\*")
(add-hook 'after-init-hook 'winner-mode)
(add-hook 'after-init-hook 'temp-buffer-resize-mode)

(setq
 help-window-select t
 help-window-keep-selected t)

(setq
 switch-to-prev-buffer-skip-regexp "^\\*\\|^magit.*"
 switch-to-buffer-obey-display-actions t
 switch-to-buffer-in-dedicated-window nil
 switch-to-buffer-preserve-window-point t
 )

(use-package burly
  :hook (after-init . burly-tabs-mode))

;; %% tabbar
(setq
 tab-bar-show t
 tab-bar-format
 '(tab-bar-format-menu-bar
   tab-bar-format-tabs
   tab-bar-separator
   tab-bar-format-add-tab)
 tab-bar-tab-hints t
 tab-bar-new-tab-to 'rightmost
 tab-bar-new-button-show t
 tab-bar-close-button-show t
 tab-bar-new-tab-choice "*scratch*"
 tab-bar-tab-name-truncated-max 20
 tab-bar-select-tab-modifiers '(super))

(add-hook 'after-init-hook
          (lambda ()
            (tab-bar-mode 1)
            (tab-bar-history-mode 1)))

(use-package sr-speedbar
  :defer 2
  :load-path "site-lisp/sr-speedbar-yx"
  :init
  (setq
   speedbar-use-images nil
   sr-speedbar-width 30
   sr-speedbar-skip-other-window-p t)
  )

(use-package ace-window
  :init
  (setq
   aw-scope 'frame
   aw-background nil
   aw-dispatch-always t
   aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )

;; %% tabspaces
(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :custom
  (tabspaces-use-filtered-buffers-as-default nil)
  (tabspaces-default-tab "main")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-session t)
  (tabspaces-session-file (no-littering-expand-var-file-name "tabsession.el"))
  (tabspaces-session-auto-restore t)
  )

;; %% buffer manager
(use-package zoom
  :custom
  (zoom-size '(0.618 . 0.618))
  )

(use-package midnight
  :ensure nil
  :defer 5
  :custom
  (midnight-delay 10800)
  (clean-buffer-list-kill-regexps '(".*"))
  (clean-buffer-list-kill-never-regexps
   '("^\\*scratch\\*$"
     "^\\*Messages\\*$"
     "^\\*Summary.*\\*$"
     "^\\*Group\\*$"
     "inbox.org$"
     "^\\*Org Agenda\\*$"
     "^\\*elfeed-.*"))
  :config
  (midnight-mode t))

(setq
 ibuffer-expert t
 ibuffer-display-summary nil
 ibuffer-show-empty-filter-groups nil
 ;; ibuffer-never-show-predicates '("^\\*")
 )
(add-hook
 'ibuffer-mode-hook
 (lambda ()
   (ibuffer-do-sort-by-recency)
   (ibuffer-auto-mode 1)))

(use-package ibuffer-vc
  :init
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))

(use-package popper
  :defer 2
  :bind (("C-`" . popper-toggle-latest)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :config
  (setq
   popper-display-control 'user
   popper-group-function #'popper-group-by-directory)
  (setq popper-reference-buffers
        '("^\\*Compile"
          "\\*Backtrace\\*"
          "\\*Bookmark List\\*"
          "\\*shell.*\\*$" shell-mode
          "\\*eshell.*\\*$" eshell-mode
          "\\*term.*\\*$" term-mode
          "\\*vterm.*\\*$" vterm-mode
          "\\*julia\\*$"
          "\\*color-rg\\*$"
          "\\*Python\\*$"
          "\\*org-roam\\*$"
          help-mode
          helpful-mode
          occur-mode
          compilation-mode
          ))
  (popper-mode 1)
  (popper-echo-mode 1)
  )

(use-package shackle
  :defer 2
  :custom
  (shackle-default-size 0.4)
  (shackle-select-reused-windows t)
  (shackle-default-alignment 'below)
  (shackle-inhibit-window-quit-on-same-windows nil)
  :config
  (setq
   shackle-rules
   '((("\\*Help\\*"
       "\\*[Wo]*Man.*\\*"
       "\\*Dictionary\\*"
       "\\*Bookmark List\\*"
       "\\*Flymake .*"
       "^CAPTURE-"
       "^\\*julia.*")
      :regexp t :select t :popup t :align t)
     (("\\*Warnings\\*"
       "\\*Messages\\*"
       "\\*evil-registers\\*"
       "\\*evil-owl\\*"
       "^\\*Compile"
       "\\*Agenda Commands\\*"
       "^\\*Org Note"
       "^\\*Org Select"
       "\\*Capture\\*"
       "^\\*Python\\*"
       "\\*Shell Command Output\\*")
      :regexp t :nonselect t :popup t :align t)
     ((color-rg-mode
       "^\\*.* eww\\*$")
      :regexp t :select t :popup t)
     (("^magit" magit-mode
       ibuffer-mode
       Info-mode
       dired-mode
       vterm-mode)
      :regexp t :select t :same t :inhibit-window-quit t)
     (("^\\*org-roam\\*$")
      :regexp t :align right :size 0.33)
     )
   )
  (shackle-mode 1)
  )

;; %% end
(provide 'editor-window)
;;; editor-window.el ends here