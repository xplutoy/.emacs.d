;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:05:22
;; Modified: <2024-01-08 13:57:18 yx>
;; Licence: GPLv3

;;; Commentary:

;; reader

;;; Code:

;; %% doc-view
(setq
 doc-view-continuous t
 doc-view-resolution 300)

(use-package pdf-tools
  :if (display-graphic-p)
  :hook
  (pdf-tools-enabled . pdf-isearch-minor-mode)
  :init
  (setq
   pdf-view-use-scaling t
   pdf-view-use-imagemagick nil)
  (setq-default pdf-view-display-size 'fit-width)
  (pdf-loader-install)
  )

;; %% olivetti
(use-package olivetti
  :hook ((org-mode
          org-agenda-mode
          ) . olivetti-mode)
  :init
  (setq
   olivetti-style nil
   olivetti-mode-map nil
   olivetti-body-width 0.66
   olivetti-minimum-body-width (+ fill-column 2)
   )
  )

;; %% elfeed
(use-package elfeed
  :init
  (setq elfeed-search-print-entry-function
        'crux-yx/elfeed-search-print-entry--better-default)
  :custom
  (elfeed-feeds
   '(("https://36kr.com/feed" new)
     ("https://www.zhihu.com/rss" new)
     ("https://www.inference.vc/rss" ai)
     ("https://spaces.ac.cn/feed" ai wk)
     ("https://ruder.io/rss/index.rss" ai)
     ("https://lilianweng.github.io/index.xml" ai wk)
     ("https://yihui.org/en/index.xml" R)
     ("https://www.juliabloggers.com/feed/" julia)
     ("http://www.ruanyifeng.com/blog/atom.xml" tech wk)
     ("http://lambda-the-ultimate.org/rss.xml" lang)
     ("https://vimtricks.com/feed/" vim)
     ("https://egh0bww1.com/rss.xml" emacs)
     ("https://karthinks.com/index.xml" emacs)
     ("https://manateelazycat.github.io/feed.xml" emacs)
     ("https://planet.emacslife.com/atom.xml" emacs)
     ("https://matt.might.net/articles/feed.rss" emacs)
     ("https://andreyor.st/categories/emacs/feed.xml" emacs)
     ("https://sachachua.com/blog/category/emacs/feed/" emacs)))
  (elfeed-search-filter "@3-months-ago +unread -new")
  :hook (elfeed-show . olivetti-mode)
  :config
  (run-at-time nil (* 4 60 60) 'elfeed-update)
  (keymap-set elfeed-search-mode-map "m" (yx/elfeed-tag-selection-as 'star))
  (keymap-set elfeed-search-mode-map "l" (yx/elfeed-tag-selection-as 'readlater))
  :preface
  (defun yx/elfeed-kill-entry ()
    "Like `elfeed-kill-entry' but pop elfeed search"
    (interactive)
    (elfeed-kill-buffer)
    (switch-to-buffer "*elfeed-search*"))
  (defun yx/elfeed-tag-selection-as (mytag)
    (lambda ()
      "Toggle a tag on an Elfeed search selection"
      (interactive)
      (elfeed-search-toggle-all mytag)))
  :bind
  (:map elfeed-show-mode-map
        ("w" . elfeed-show-yank)
        ("%" . elfeed-webkit-toggle)
        ("q" . yx/elfeed-kill-entry)
        )
  )

(use-package elfeed-webkit
  :after elfeed
  :config
  (elfeed-webkit-auto-toggle-by-tag)
  :bind
  (:map elfeed-webkit-map
        ("q" . yx/elfeed-kill-entry))
  )


(provide 'init-reading)
;;; init-reading.el ends here
