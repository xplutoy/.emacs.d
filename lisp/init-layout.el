;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:06:35
;; Modified: <2024-01-11 01:35:43 yx>
;; Licence: GPLv3

;;; Commentary:

;; window buffer frame

;;; Code:
(setq
 window-min-height 3
 window-min-width 30
 window-sides-vertical nil
 split-height-threshold 80
 split-width-threshold 125
 even-window-sizes 'height-only
 frame-resize-pixelwise t
 window-resize-pixelwise t
 window-combination-resize t
 fit-frame-to-buffer nil
 fit-window-to-buffer-horizontally nil)


(setq
 display-buffer-alist
 '(("\\`\\*[hH]elp"
    (display-buffer-reuse-mode-window
     display-buffer-in-direction)
    (window . root)
    (window-height . 0.4)
    (direction . bottom)
    (mode . (help-mode helpful-mode)))
   ("\\`\\*Async Shell Command\\*\\'"
    (display-buffer-no-window)
    (allow-no-window . t))
   ("\\`\\(\\*Capture\\|\\*CAPTURE\\)"
    (display-buffer-reuse-mode-window
     display-buffer-below-selected))
   ("\\`\\*Embark"
    (display-buffer-reuse-mode-window
     display-buffer-below-selected)
    (window-height . 0.4)
    (window-parameters . ((no-other-window . t)
                          (mode-line-format . none))))
   ("\\`\\(\\*Calendar\\|\\*Bookmark\\)"
    (display-buffer-reuse-mode-window
     display-buffer-below-selected)
    (dedicated . t)
    (window-height . fit-window-to-buffer))
   ("\\`\\*devdocs\\*\\'"
    (display-buffer-reuse-mode-window
     display-buffer-in-side-window)
    (side . right)
    (window-width . 0.4))
   ("\\`\\(\\*Ibuffer\\|\\*Man\\|\\*WoMan\\|\\*info\\|magit\\)"
    (display-buffer-full-frame))
   ))

(add-to-list
 'display-buffer-alist
 `(,(yx/buffer-names-to-regex
     '("\\*R"
       "\\*julia"
       "\\*[Ll]ua"
       "\\*Python"
       "\\*[Ee]shell"
       "\\*term"
       "\\*Occur"
       "\\*Backtrac"
       "\\*Flymake"
       "\\*vc-git"
       "\\*Warnings"
       "\\*Messages"
       "\\*quickrun"
       "\\*Dictionary"
       "\\*osx-dictionary"
       "\\*color-rg"))
   (display-buffer-reuse-mode-window
    display-buffer-in-side-window)
   (side . bottom)
   (window-height . 0.4)))

(defun yx/toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer))
  (force-window-update))

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
 switch-to-buffer-in-dedicated-window nil
 switch-to-buffer-obey-display-actions t
 switch-to-buffer-preserve-window-point t
 switch-to-prev-buffer-skip 'visible
 switch-to-prev-buffer-skip-regexp "^\\*\\|^magit.*")

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
 tab-bar-new-tab-to 'right
 tab-bar-new-button-show t
 tab-bar-close-button-show nil
 tab-bar-new-tab-choice "*scratch*"
 tab-bar-tab-name-truncated-max 20
 tab-bar-select-tab-modifiers '(super))

(defun yx/tab-bar-setup()
  (tab-bar-mode 1)
  (tab-bar-history-mode 1)
  (let ((map tab-bar-map))
    (keymap-unset map "<wheel-up>")
    (keymap-unset map "<wheel-down>")
    (keymap-unset map "<wheel-left>")
    (keymap-unset map "<wheel-right>")))
(add-hook 'after-init-hook #'yx/tab-bar-setup)

(use-package sr-speedbar
  :ensure nil
  :defer 2
  :init
  (setq
   speedbar-use-images nil
   sr-speedbar-width 30
   sr-speedbar-skip-other-window-p t))

(use-package ace-window
  :init
  (setq
   aw-scope 'frame
   aw-background nil
   aw-dispatch-always t
   aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; %% tabspaces
(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :custom
  (tabspaces-use-filtered-buffers-as-default nil)
  (tabspaces-default-tab "Main")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-session t)
  (tabspaces-session-file (no-littering-expand-var-file-name "tabsession.el"))
  (tabspaces-session-auto-restore t)
  )

;; %% buffer manager
(setq buffer-quit-function 'winner-undo)

(use-package zoom
  :custom
  (zoom-size '(0.618 . 0.618)))

(setq
 ibuffer-expert t
 ibuffer-display-summary nil
 ;; ibuffer-never-show-predicates '("^\\*")
 ibuffer-show-empty-filter-groups nil)

(defun yx/ibuffer-setup ()
  (hl-line-mode 1)
  (ibuffer-auto-mode 1)
  (ibuffer-do-sort-by-recency))
(add-hook 'ibuffer-mode-hook 'yx/ibuffer-setup)

(use-package ibuffer-vc
  :init
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))

(use-package popper
  :defer 2
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :config
  (setq
   popper-display-control 'user
   popper-group-function #'popper-group-by-directory)
  (setq popper-reference-buffers
        '("\\*julia\\*$"
          "\\*color-rg\\*$"
          "\\*Python\\*$"
          (compilation-mode . hide)
          help-mode
          helpful-mode
          occur-mode
          color-rg-mode
          ))
  (popper-mode 1)
  (popper-echo-mode 1)
  )

(provide 'init-layout)
;;; init-layout.el ends here
