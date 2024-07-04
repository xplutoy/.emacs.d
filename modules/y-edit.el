;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 11:56:55
;; Modified: <2024-06-27 14:59:20 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package vundo)
(use-package goto-chg)

(use-package avy
  :custom
  (avy-style 'at)
  (avy-highlight-first t)
  (avy-timeout-seconds 0.8)
  :config
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?  avy-dispatch-alist) #'avy-action-mark-to-char)
  (setf (alist-get ?. avy-dispatch-alist) #'avy-action-embark)
  (keymap-set isearch-mode-map "M-j" #'avy-isearch))

(use-package avy-zap)
(use-package casual-avy)

(use-package speedrect
  :ensure nil
  ;; :vc (:url "https://github.com/jdtsmith/speedrect")
  :defer 3
  :init
  (unless (package-installed-p 'speedrect)
    (package-vc-install "https://github.com/jdtsmith/speedrect")))

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(use-package drag-stuff
  :autoload drag-stuff-define-keys
  :hook ((prog-mode conf-mode) . turn-on-drag-stuff-mode)
  :config
  (setq drag-stuff-modifier '(meta shift)
        drag-stuff-except-modes '(org-mode))
  (drag-stuff-define-keys))

(use-package hungry-delete
  :hook (after-init . global-hungry-delete-mode)
  :custom
  (hungry-delete-join-reluctantly t))

(use-package pulsar
  :defer 3
  :custom
  (pulsar-delay 0.05)
  (pulsar-iterations 8)
  :config
  (add-hook 'next-error-hook         #'pulsar-pulse-line-red)
  (add-hook 'imenu-after-jump-hook   #'pulsar-recenter-center)
  (add-hook 'imenu-after-jump-hook   #'pulsar-reveal-entry)
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-center)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)
  (pulsar-global-mode 1))

(use-package goggles
  :hook ((text-mode prog-mode) . goggles-mode)
  :custom (goggles-pulse t))

(use-package selected
  :ensure t
  :commands selected-minor-mode
  :hook ((prog-mode text-mode) . selected-minor-mode)
  :bind (:map selected-keymap
              ("q" . selected-off)
              (";" . comment-dwim)
              ("\\" . indent-region)
              ("<" . surround-insert)
	      (">" . surround-delete)
              ("=" . puni-expand-region)
              ("-" . puni-contract-region)
              ("!" . shell-command-on-region)
              ("@" . apply-macro-to-region-lines)
              ("×" . pyim-create-word-from-selection)
              ("*" . pyim-create-word-from-selection)
              ("U" . upcase-region)
              ("D" . downcase-region)
              ("S" . sort-lines)
              ("N" . narrow-to-region)
              :map selected-org-mode-map
              ("t" . org-table-convert-region))
  :init
  (setq selected-org-mode-map (make-sparse-keymap)))

(use-package dwim-shell-command
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command)))

(provide 'y-edit)
;;; y-edit.el ends here
