;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 14:44:18
;; Modified: <>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

(use-package browse-url
  :ensure nil
  :custom
  (browse-url-browser-function #'eww-browse-url)
  (browse-url-secondary-browser-function 'browse-url-default-browser)
  :config
  (setq browse-url-generic-program
        (cond (IS-MAC "open")
              (IS-LINUX "xdg-open")
              (t "powershell.exe")))
  (setq browse-url-generic-args
        (cond (IS-WIN '("start"))
              (IS-WSL '("start"))
              (t ""))))

(use-package eww
  :ensure nil
  :init
  (setq shr-max-image-proportion 0.6
        shr-use-xwidgets-for-media t)
  (setq eww-restore-desktop t
        eww-header-line-format nil
        eww-browse-url-new-window-is-tab nil
        eww-search-prefix "http://www.google.com/search?q="
        eww-auto-rename-buffer
        (lambda () (format "*eww: %s*" (or (plist-get eww-data :title) "..."))))
  :config
  (defun yx/eww-quit-all()
    "Kill all EWW buffers, then quit window."
    (interactive)
    (let ((list-buffers (buffer-list)))
      (dolist (buffer list-buffers)
        (when (with-current-buffer buffer
                (eq major-mode 'eww-mode))
          (kill-buffer buffer))))
    (quit-window))
  (add-hook 'eww-mode-hook #'tab-line-mode)
  (add-hook 'eww-after-render-hook #'eww-readable)
  (bind-keys :map eww-mode-map
             ("Q" . yx/eww-quit-all))
  )

(use-package webjump
  :ensure nil
  :custom
  (webjump-sites '(("Org"        . "https://orgmode.org")
                   ("Dicalab"    . "https://center.dicalab.cn")
                   ("Google"     . [simple-query "www.google.com" "www.google.com/search?q=" ""])
                   ("DuckDuckGo" . [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
                   ("Wikipedia"  . [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""]))))

(provide 'y-web)
;;; y-web.el ends here
