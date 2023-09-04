;;; init-wind.el --- windows  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:06:35
;; Modified: <2023-09-04 22:47:00 yx>
;; Licence: GPLv3

;;; Commentary:

;; window buffer frame

;;; Code:

;; %%
(setq
 winner-dont-bind-my-keys t
 winner-boring-buffers-regexp "^\\*")
(add-hook 'after-init-hook 'winner-mode)
(add-hook 'after-init-hook 'temp-buffer-resize-mode)

(setq
 help-window-select t
 help-window-keep-selected t)

(setq
 switch-to-prev-buffer-skip-regexp "^\\*"
 switch-to-buffer-obey-display-actions t
 switch-to-buffer-in-dedicated-window nil
 switch-to-buffer-preserve-window-point t
 )

;; %% tabbar
(setq
 tab-bar-tab-hints t
 tab-bar-new-button-show nil
 tab-bar-close-button-show nil
 tab-bar-new-tab-choice "*scratch*"
 tab-bar-select-tab-modifiers '(super))
(add-hook 'after-init-hook
          (lambda ()
            (tab-bar-mode)
            (tab-bar-history-mode)))

;; %% dired
(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-mouse-drag-files t)
  (dired-omit-files "^\\..*$")
  (dired-recursive-copies 'always)
  (dired-guess-shell-alist-user yx/default-open-program)
  (dired-auto-revert-buffer 'dired-buffer-stale-p)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-alGgh")
  :config
  (add-hook
   'dired-mode-hook
   (lambda ()
     (hl-line-mode)
     (dired-omit-mode)
     (dired-hide-details-mode)))
  (add-hook 'wdired-mode-hook 'highlight-changes-mode)
  (put 'dired-find-alternate-file 'disabled nil)
  )

;; %% dired+
(use-package diredfl
  :hook (dired-mode . diredfl-mode)
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t)
  )

(use-package dired-narrow
  :bind (:map dired-mode-map
              ([remap dired-do-man] . dired-narrow-fuzzy)))
(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))
(use-package dired-filter
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-filter-mode))
  )

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
     "^\\*Group\\*$"))
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
        '("\\*Ibuffer\\*"
          "^\\*Compile"
          "\\*Backtrace\\*"
          "\\*shell.*\\*$" shell-mode
          "\\*eshell.*\\*$" eshell-mode
          "\\*term.*\\*$" term-mode
          "\\*vterm.*\\*$" vterm-mode
          "\\*julia\\*$"
          "\\*color-rg\\*$"
          "\\*Python\\*$"
          "\\*org-roam\\*$"
          help-mode
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
   '((("\\*Ibuffer\\*"
       "\\*Help\\*"
       "\\*[Wo]*Man.*\\*"
       "\\*Dictionary\\*"
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
(provide 'init-wind)
;;; init-wind.el ends here
