;;; -*- coding: utf-8; lexical-binding: t; -*-
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

;; %% elfeed
(use-package elfeed
  :custom
  (elfeed-feeds
   '(("https://www.inference.vc/rss" +ai)
     ("https://spaces.ac.cn/feed" +ai +webkit)
     ("https://ruder.io/rss/index.rss" +ai)
     ("https://www.juliabloggers.com/feed/" +julia)
     ("http://www.ruanyifeng.com/blog/atom.xml" +tech)
     ("https://egh0bww1.com/rss.xml" emacs)
     ("https://planet.emacslife.com/atom.xml" +emacs)
     ("https://sachachua.com/blog/category/emacs/feed/" +emacs)))
  :preface
  (defun yx/elfeed-eww-browse ()
    "Wrapper to open eww and mark elfeed as read"
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
        (eww-browse-url link)))
    )
  (defun yx/elfeed-kill-entry ()
    "Like `elfeed-kill-entry' but pop elfeed search"
    (interactive)
    (elfeed-kill-buffer)
    (switch-to-buffer "*elfeed-search*")
    )
  :config
  (bind-keys
   :map elfeed-show-mode-map
   ("B" . yx/elfeed-eww-browse)
   ("q" . yx/elfeed-kill-entry)
   )

  (add-hook
   'elfeed-new-entry-hook
   (elfeed-make-tagger :before "4 weeks ago" :remove 'unread))
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
