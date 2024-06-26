;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 14:04:53
;; Modified: <2024-06-24 00:19:05 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(use-package outline
  :ensure nil
  :custom
  (outline-minor-mode-cycle t)
  (outline-minor-mode-highlight t))

(use-package doc-view
  :ensure nil
  :custom
  (doc-view-continuous t)
  (doc-view-resolution 300))

(use-package pdf-tools
  :hook (pdf-tools-enabled . pdf-isearch-minor-mode)
  :init
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (setq-default pdf-view-display-size 'fit-width)
  (pdf-loader-install))

(use-package olivetti
  :hook ((org-mode . olivetti-mode)
         (org-agenda-mode . olivetti-mode))
  :init
  (setq olivetti-style nil)
  (setq olivetti-mode-map nil)
  (add-hook 'olivetti-mode-hook
            (lambda ()
              (keymap-local-set "<left-margin> <mouse-1>" #'ignore)
              (keymap-local-set "<right-margin> <mouse-1>" #'ignore))))

(use-package elfeed
  :init
  (setq elfeed-feeds
        '(("https://spaces.ac.cn/feed" ai)
          ("https://www.inference.vc/rss" ai)
          ("https://liduos.com/atom.xml" ai)
          ("https://lilianweng.github.io/index.xml" ai)
          ("https://www.luxiangdong.com/rss2.xml" ai)
          ("https://tech.youzan.com/rss/" tech)
          ("https://tech.meituan.com/feed/" tech)
          ("https://ameow.xyz/feed/categories/weekly.xml" tech)
          ("https://www.ruanyifeng.com/blog/atom.xml" tech)
          ("https://wingolog.org/feed/atom" lang)
          ("https://www.juliabloggers.com/feed/" julia)
          ("https://karthinks.com/index.xml" emacs)
          ("https://planet.emacslife.com/atom.xml" emacs)
          ("https://manateelazycat.github.io/feed.xml" emacs)))
  (setq elfeed-search-filter "@3-months-ago +unread")
  (setq elfeed-search-title-max-width 79)
  (setq elfeed-search-print-entry-function #'x-elfeed-search-print-entry)
  :hook (elfeed-show . olivetti-mode)
  :config
  (require 'x-elfeed)
  (run-at-time nil (* 4 60 60) 'elfeed-update)
  (keymap-set elfeed-show-mode-map "w" #'elfeed-show-yank)
  (keymap-set elfeed-show-mode-map "%" #'x-elfeed-show-in-xwidget)
  (keymap-set elfeed-show-mode-map "q" #'x-elfeed-kill-entry)
  (keymap-set elfeed-search-mode-map "m" (x-elfeed-tag-as 'star))
  (keymap-set elfeed-search-mode-map "l" (x-elfeed-tag-as 'readlater)))
(use-package outli
  ;; :vc (:url "https://github.com/jdtsmith/outli")
  :ensure nil
  :hook ((prog-mode text-mode) . outli-mode)
  :init
  (unless (package-installed-p 'outli)
    (package-vc-install "https://github.com/jdtsmith/outli"))
  :config
  (setf (alist-get 'emacs-lisp-mode outli-heading-config ) '(";;;" ?\; t))
  :bind (:map outli-mode-map
              ("C-c C-u" . (lambda () (interactive) (outline-back-to-heading)))))

(provide 'y-reading)
;;; y-reading.el ends here
