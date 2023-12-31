;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-09-15 22:20:40
;; Modified: <2024-01-06 03:32:13 yx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-mouse-drag-files t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask)
  (dired-guess-shell-alist-user yx/default-open-program)
  (dired-auto-revert-buffer 'dired-buffer-stale-p)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-alGgh")
  :config
  (add-hook 'dired-mode-hook 'yx/dired-setup)
  (add-hook 'wdired-mode-hook 'highlight-changes-mode)
  (put 'dired-find-alternate-file 'disabled nil)
  :preface
  (defun yx/dired-setup ()
    (setq
     dired-omit-files
     (concat dired-omit-files "\\|^\\..*$"))
    (hl-line-mode 1)
    (dired-omit-mode 1)
    (dired-hide-details-mode 1))
  )

;; %% dired+
(use-package diredfl
  :hook
  (dired-mode . diredfl-mode)
  (dirvish-directory-view-mode . diredfl-mode)
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t)
  )

(use-package zoxide)

(use-package dirvish
  :hook (after-init . dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"           "Home")
     ("d" "~/yxdocs/"    "yxdocs")
     ("D" "~/Downloads/" "Downloads")
     ("w" "~/workspace/" "workspace>")))
  :config
  (setq dirvish-side-width 30
        dirvish-use-mode-line t
        dirvish-default-layout '(0 0.4 0.6))
  (let ((height (/ yx/font-height 10.0)))
    (setq dirvish-mode-line-height height
          dirvish-header-line-height height))
  (setq dirvish-attributes
        '(file-time file-size collapse subtree-state vc-state))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-peek-mode 1)
  (dirvish-side-follow-mode 1)
  :bind
  (:map dirvish-mode-map
        ("a"   . dirvish-quick-access)
        ("f"   . dirvish-file-info-menu)
        ("y"   . dirvish-yank-menu)
        ("N"   . dirvish-narrow)
        ("h"   . dirvish-history-jump)
        ("s"   . dirvish-quicksort)
        ("v"   . dirvish-vc-menu)
        ("TAB" . dirvish-subtree-toggle)
        ("M-f" . dirvish-history-go-forward)
        ("M-b" . dirvish-history-go-backward)
        ("M-l" . dirvish-ls-switches-menu)
        ("M-m" . dirvish-mark-menu)
        ("M-t" . dirvish-layout-toggle)
        ("M-s" . dirvish-setup-menu)
        ("M-e" . dirvish-emerge-menu)
        ("M-j" . dirvish-fd-jump))
  )


(provide 'init-dired)
;;; init-dired.el ends here
