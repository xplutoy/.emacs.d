;;; editor-mail.el --- mail  -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 22:52:27
;; Modified: <2023-10-28 13:08:46 yx>
;; Licence: GPLv3

;;; Commentary:

;; mail settings

;;; Code:

(setq
 gnus-home-directory "~/.gnus.d/"
 gnus-default-directory gnus-home-directory
 mm-default-directory gnus-home-directory
 message-directory (expand-file-name "Mail/" gnus-home-directory))

(setq
 read-mail-command 'gnus
 message-confirm-send t
 message-from-style 'angles
 message-kill-buffer-on-exit t
 mail-user-agent 'gnus-user-agent
 mail-envelope-from 'header
 mail-specify-envelope-from t
 sendmail-program "/usr/local/bin/msmtp"
 send-mail-function 'message-send-mail-with-sendmail
 message-send-mail-function 'message-send-mail-with-sendmail
 )

(setq mm-inline-large-images t)

(setq
 mml-default-sign-method "pgpmime"
 mml-secure-openpgp-sign-with-sender t)

(use-package gnus
  :commands (gnus)
  :ensure nil
  :config
  (setq
   mail-sources
   '((imap
      :server "imap.qq.com"
      :port 993
      :user "yangxue.cs@foxmail.com"
      :mailbox "INBOX"
      :fetchflag "\\Seen"
      :stream tls
      :dontexpunge t)
     ))
  (setq
   gnus-select-method '(nnnil "")
   gnus-secondary-select-methods
   '((nnml "foxmail.cs")
     (nnimap
      "foxmail.wk"
      (nnimap-address "imap.qq.com")
      (nnimap-server-port 993)
      (nnimap-user "yangxue.wk@foxmail.com")
      (nnimap-stream ssl)
      (nnimap-expunge 'nerver)
      (nnimap-search-engine imap)
      (nnimap-inbox "INBOX")
      (nnimap-split-methods default))
     (nnimap
      "outlook.cs"
      (nnimap-user "yangxue.cs@outlook.com")
      (nnimap-stream ssl)
      (nnimap-server-port 993)
      (nnimap-address "outlook.office365.com")
      (nnimap-expunge 'nerver)
      (nnimap-search-engine imap)
      (nnimap-inbox "INBOX")
      (nnimap-split-methods default))
     ))
  (setq
   nnmail-expiry-wait '30
   nnmail-resplit-incoming t
   nnmail-split-fancy-match-partial-words t
   nnmail-split-methods 'nnmail-split-fancy
   nnmail-split-fancy
   '(|
     (: nnmail-split-fancy-with-parent)
     (to  "yangxue.cs@foxmail.com" "INBOX.foxmail.cs")
     (to  "yangxue.wk@foxmail.com" "INBOX.foxmail.wk")
     (to  "yangxue.cs@outlook.com" "INBOX.outlook.cs")
     (any "emacs-devel@gnu.org"    "INBOX.emacs-devel")
     (any "emacs-orgmode@gnu.org"  "INBOX.emacs-orgmode")
     (any "help-gnu-emacs@gnu.org" "INBOX.help-gnu-emacs")
     "INBOX.Misc")
   )

  (setq
   gnus-asynchronous t
   gnus-use-header-prefetch t
   gnus-use-cache t
   gnus-use-scoring t
   gnus-suppress-duplicates t
   gnus-novice-user nil
   gnus-expert-user t
   gnus-interactive-exit 'quiet
   gnus-inhibit-startup-message t)
  (setq
   gnus-save-newsrc-file nil
   gnus-read-newsrc-file nil
   gnus-save-killed-list nil
   gnus-read-active-file nil
   gnus-use-dribble-file t
   gnus-always-read-dribble-file t
   gnus-message-archive-group nil
   gnus-inhibit-user-auto-expire t
   gnus-search-use-parsed-queries t
   gnus-article-browse-delete-temp t
   gnus-check-new-newsgroups 'ask-server
   gnus-mime-display-multipart-related-as-mixed t
   gnus-subscribe-newsgroup-method 'gnus-subscribe-zombies)
  (setq
   gnus-cache-remove-articles '(read)
   gnus-cacheable-groups "^\\(nntp\\|nnimap\\)"
   gnus-cache-enter-articles '(ticked dormant unread))

  (setq
   nnrss-ignore-article-fields '(description guid pubData dc:creator link))

  (gnus-demon-add-handler 'gnus-demon-scan-mail nil 10)
  )

(use-package gnus-group
  :ensure nil
  :hook (gnus-select-group . gnus-group-set-timestamp)
  :config
  (setq
   gnus-sum-thread-tree-root            "◉ "
   gnus-sum-thread-tree-false-root      "◎ "
   gnus-sum-thread-tree-single-indent   "◌ "
   gnus-sum-thread-tree-vertical        "| "
   gnus-sum-thread-tree-indent          "  "
   gnus-sum-thread-tree-leaf-with-other "+-> "
   gnus-sum-thread-tree-single-leaf     "`-> "
   gnus-summary-line-format "%U%R%z%B%[%4L: %-10,10f%] %s\n")
  (setq
   gnus-summary-make-false-root 'adopt
   gnus-summary-ignore-duplicates t
   gnus-summary-gather-subject-limit 'fuzzy
   gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
   gnus-simplify-subject-functions '(gnus-simplify-subject-re gnus-simplify-whitespace))
  (setq
   gnus-use-trees t
   gnus-show-threads t
   gnus-fetch-old-headers t
   gnus-tree-minimize-window t
   gnus-generate-tree-function
   'gnus-generate-horizontal-tree
   gnus-build-sparse-threads 'some
   gnus-fetch-old-ephemeral-headers 2
   gnus-thread-indent-level 2
   gnus-thread-hide-subtree nil
   gnus-thread-sort-functions
   '(gnus-thread-sort-by-subject
     gnus-thread-sort-by-most-recent-number))
  (setq
   gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
   gnus-subthread-sort-functions '(gnus-thread-sort-by-date))
  (setq
   gnus-view-pseudos 'automatic
   gnus-view-pseudos-separately t
   gnus-view-pseudo-asynchronously t)
  (setq
   gnus-auto-select-first t
   gnus-auto-select-next nil
   gnus-paging-select-next nil)

  (setq
   gnus-group-sort-function '(gnus-group-sort-by-method)
   gnus-group-line-format "%M%S%p%P %0{%5y%} %B%{%G%}\n")

  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  )


;; ==== end =========
(provide 'editor-mail)
