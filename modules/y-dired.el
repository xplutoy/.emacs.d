;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 14:06:24
;; Modified: <2024-06-19 14:56:53 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-<return>" . dired-do-open)
              ("C-+" . dired-create-empty-file))
  :custom
  (dired-dwim-target t)
  (dired-vc-rename-file t)
  (dired-mouse-drag-files t)
  (dired-movement-style 'cycle)
  (dired-ls-F-marks-symlinks t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask)
  (dired-auto-revert-buffer 'dired-buffer-stale-p)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-laFGgh")
  (wdired-create-parent-directories t)
  (wdired-allow-to-change-permissions t)
  :config
  (defun yx/dired-setup ()
    (setq dired-omit-files
          (concat dired-omit-files "\\|^\\..*$"))
    (setq-local mouse-1-click-follows-link 'double)
    (dired-omit-mode 1)
    (dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook 'yx/dired-setup)
  (add-hook 'wdired-mode-hook 'highlight-changes-mode)
  (put 'dired-find-alternate-file 'disabled nil))

(use-package diredfl
  :hook ((dired-mode . diredfl-mode)
         (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package casual-dired
  :after dired
  :bind (:map dired-mode-map ("C-o" . casual-dired-tmenu)))
(use-package dirvish
  :hook (after-init . dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"           "Home")
     ("d" "~/yxdocs/"    "yxdocs")
     ("c" "d:/Codes/"    "codes")
     ("w" "~/workspace/" "workspace")))
  :config
  (setq dirvish-side-width 30
        dirvish-use-mode-line t
        dirvish-default-layout '(0 0.4 0.6))
  (let ((height (/ yx/font-h 10.0)))
    (setq dirvish-mode-line-height height
          dirvish-header-line-height height))
  (setq dirvish-attributes
        '(file-time file-size collapse subtree-state vc-state))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-peek-mode -1)
  (dirvish-side-follow-mode 1)
  :bind (:map dirvish-mode-map
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
              ("M-t" . dirvish-layout-toggle)
              ("M-s" . dirvish-setup-menu)
              ("M-e" . dirvish-emerge-menu)
              ("M-j" . dirvish-fd-jump)))

(provide 'y-dired)
;;; y-dired.el ends here
