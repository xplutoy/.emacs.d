;;; -*- coding: utf-8; lexical-binding: t; -*-
(setq
 elfeed-feeds
 '(("https://www.inference.vc/rss" ai)
   ("https://spaces.ac.cn/feed" ai webkit)
   ("https://ruder.io/rss/index.rss" ai)
   ("https://www.juliabloggers.com/feed/" julia)
   ("http://www.ruanyifeng.com/blog/atom.xml" tech)
   ("https://egh0bww1.com/rss.xml" emacs)
   ("https://planet.emacslife.com/atom.xml" emacs)
   ("https://sachachua.com/blog/category/emacs/feed/" emacs))
 )

(defun elfeed-eww-browse ()
  "Wrapper to open eww and mark elfeed as read"
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (eww-browse-url link))))

(use-package elfeed
  :config
  (bind-keys
   :map elfeed-show-mode-map
   ("B" . elfeed-eww-browse)
   ("q" . (lambda ()
            "Switch to *elfeed-search* buffer."
            (interactive)
            (switch-to-buffer "*elfeed-search*"))))
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

(provide 'init-elfeed)
