;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 14:44:18
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

(use-package engine-mode
  :defer 3
  :custom
  (engine/keybinding-prefix "C-z /")
  (engine/browser-function 'browse-url-generic)
  :config
  (defun yx/extract-name-from-url (url)
    (let* ((host (url-host (url-generic-parse-url url)))
           (host-trimmed (split-string host  "\\.")))
      (car (last host-trimmed 2))))
  (defun yx/define-enines (engines)
    (dolist (engi engines)
      (let* ((key  (car engi))
             (url  (cadr engi))
             (name (yx/extract-name-from-url url))
             (symn (make-symbol name)))
        (eval `(defengine ,symn ,url :keybinding ,key)))))
  (yx/define-enines
   '(("c" "https://github.com/search?q=%s")
     ("g" "https://www.google.com/search?q=%s")
     ("b" "https://cn.bing.com/search?q=%s&ensearch=1")
     ("w" "https://zh.wikipedia.org/w/index.php?search=%s")
     ("a" "https://www.wolframalpha.com/input/?i=%s")
     ("z" "https://www.zhihu.com/search?q=%s")
     ("d" "https://search.douban.com/book/subject_search?search_text=%s")))
  (engine-mode +1))

(provide 'y-web)
;;; y-web.el ends here
