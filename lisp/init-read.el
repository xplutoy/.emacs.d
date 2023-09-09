;;; init-read.el --- reader  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:05:22
;; Modified: <2023-09-08 19:36:07 yx>
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
  (pdf-loader-install)
  )

;; %% olivetti
(use-package olivetti
  :hook ((org-mode
          org-agenda-mode
          ) . olivetti-mode)
  :init
  (setq
   olivetti-mode-map nil
   olivetti-body-width 0.66
   olivetti-minimum-body-width (+ fill-column 2)
   )
  )

;; %% elfeed
(use-package elfeed
  :custom
  (elfeed-feeds
   '(("https://www.inference.vc/rss" ai)
     ("https://spaces.ac.cn/feed" ai webkit)
     ("https://ruder.io/rss/index.rss" ai)
     ("https://yihui.org/en/index.xml" R)
     ("https://www.juliabloggers.com/feed/" julia)
     ("http://www.ruanyifeng.com/blog/atom.xml" tech webkit)
     ("http://lambda-the-ultimate.org/rss.xml" lang)
     ("https://egh0bww1.com/rss.xml" emacs)
     ("https://manateelazycat.github.io/feed.xml" emacs)
     ("https://planet.emacslife.com/atom.xml" emacs)
     ("https://sachachua.com/blog/category/emacs/feed/" emacs)))
  :preface
  (defun yx/elfeed-kill-entry ()
    "Like `elfeed-kill-entry' but pop elfeed search"
    (interactive)
    (elfeed-kill-buffer)
    (switch-to-buffer "*elfeed-search*")
    )
  :hook (elfeed-show . olivetti-mode)
  :bind (:map elfeed-show-mode-map
              ("q" . yx/elfeed-kill-entry))
  )

(use-package elfeed-webkit
  :after elfeed
  :demand t
  :config
  (setq elfeed-webkit-auto-enable-tags '(webkit))
  (elfeed-webkit-auto-toggle-by-tag)
  :bind (:map elfeed-show-mode-map
              ("W" . elfeed-webkit-toggle))
  )

(provide 'init-read)
;;; init-read.el ends here
