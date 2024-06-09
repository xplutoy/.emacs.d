;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 11:50:36
;; Modified: <2024-06-07 15:13:06 yangx>
;; Licence: GPLv3

;;; Commentary:

;; 邮件

;;; Code:
(setq read-mail-command 'gnus
      message-confirm-send t
      message-kill-buffer-on-exit t
      mail-user-agent 'gnus-user-agent
      mail-envelope-from 'header
      mail-specify-envelope-from t
      send-mail-function 'message-send-mail-with-sendmail)

(setq mml-default-sign-method "pgpmime"
      mml-secure-openpgp-sign-with-sender t)

(setq gnus-home-directory no-littering-var-directory
      gnus-default-directory gnus-home-directory
      gnus-startup-file (nol-expand-var "newsrc"))

(use-package gnus
  :ensure nil
  :config
  (setq gnus-select-method '(nnnil "")
        gnus-secondary-select-methods
        '((nnimap "foxmail.cs"
                  (nnimap-address "imap.qq.com"))
          (nnimap "outlook.cs"
                  (nnimap-address "outlook.office365.com"))))

  (setq gnus-asynchronous t
        gnus-use-header-prefetch t
        gnus-use-cache t
        gnus-use-scoring t
        gnus-suppress-duplicates t
        gnus-novice-user nil
        gnus-expert-user t
        gnus-interactive-exit 'quiet
        gnus-inhibit-startup-message t)

  (setq gnus-save-newsrc-file nil
        gnus-read-newsrc-file nil
        gnus-save-killed-list nil
        gnus-read-active-file nil
        gnus-always-read-dribble-file t
        gnus-message-archive-group nil
        gnus-article-browse-delete-temp t
        gnus-mime-display-multipart-related-as-mixed t)

  (setq nnmail-expiry-wait '30
        nnmail-resplit-incoming t
        nnmail-split-fancy-match-partial-words t
        nnmail-split-methods 'nnmail-split-fancy
        nnmail-split-fancy '(| (: nnmail-split-fancy-with-parent)
                               (to  "yangxue.cs@foxmail.com" "INBOX.foxmail.cs")
                               (to  "yangxue.cs@outlook.com" "INBOX.outlook.cs")
                               (any "emacs-devel@gnu.org"    "INBOX.emacs-devel")
                               (any "emacs-orgmode@gnu.org"  "INBOX.emacs-orgmode")
                               (any "help-gnu-emacs@gnu.org" "INBOX.emacs-help")
                               "INBOX.Misc"))

  (setq nnrss-ignore-article-fields '(description guid pubData dc:creator link))

  (gnus-demon-add-handler 'gnus-demon-scan-mail nil 10))

(use-package gnus-group
  :ensure nil
  :hook (gnus-select-group . gnus-group-set-timestamp)
  :config
  (setq gnus-sum-thread-tree-root            "◉ "
        gnus-sum-thread-tree-false-root      "◎ "
        gnus-sum-thread-tree-single-indent   "◌ "
        gnus-sum-thread-tree-vertical        "| "
        gnus-sum-thread-tree-indent          "  "
        gnus-sum-thread-tree-leaf-with-other "+-> "
        gnus-sum-thread-tree-single-leaf     "`-> "
        gnus-summary-line-format "%U%R%z%B%[%4L: %-10,10f%] %s\n")

  (setq gnus-summary-make-false-root 'adopt
        gnus-summary-ignore-duplicates t
        gnus-newsgroup-maximum-articles 1500
        gnus-summary-gather-subject-limit 'fuzzy
        gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
        gnus-simplify-subject-functions '(gnus-simplify-subject-re gnus-simplify-whitespace))

  (setq gnus-use-trees t
        gnus-show-threads t
        gnus-fetch-old-headers t
        gnus-build-sparse-threads 'some
        gnus-thread-indent-level 2
        gnus-generate-tree-function #'gnus-generate-horizontal-tree
        gnus-thread-sort-functions '(gnus-thread-sort-by-subject
                                     gnus-thread-sort-by-most-recent-number))

  (setq gnus-group-sort-function '(gnus-group-sort-by-method)
        gnus-group-line-format "%M%S%p%P %0{%5y%} %B%{%G%}\n")

  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode))

(provide 'y-mail)
;;; y-mail.el ends here
