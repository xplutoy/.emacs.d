;;; editor-dired.el --- dired  -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-09-15 22:20:40
;; Modified: <2023-10-28 13:08:04 yx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
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
  :bind (:map dired-mode-map
              ("M-<f10>" . yx/transient-dired))
  )

(transient-define-prefix yx/transient-dired ()
  "Dired commands."
  [["Misc"
    ("=" "Diff" dired-diff)
    ("e" "wdired" wdired-change-to-wdired-mode)
    ("w" "Copy filename" dired-copy-filename-as-kill)
    ("R" "Rename" dired-do-rename)]
   ]
  )

;; %% dired+
(use-package diredfl
  :hook (dired-mode . diredfl-mode)
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t)
  )

(use-package dired-subtree
  :after dired
  :demand t
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-cycle))
  )

(use-package dired-narrow
  :after dired
  :demand t
  :bind (:map dired-mode-map
              ([remap dired-do-man] . dired-narrow-regexp)))

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

(use-package zoxide)


(provide 'editor-dired)
;;; editor-dired.el ends here
