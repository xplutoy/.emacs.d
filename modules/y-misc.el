;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 11:57:14
;; Modified: <2024-06-11 16:51:04 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(use-package gcmh
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold #x1000000))

(use-package tldr)

(use-package proxy-mode
  :defer 3
  :custom
  (proxy-mode-env-http-service "127.0.0.1:7890")
  :config
  (let ((proxy-mode-proxy-type 'env-http-proxy))
    (with-silent (proxy-mode +1))))

(use-package posframe)

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

(use-package which-key
  :defer 5
  :custom
  (which-key-idle-delay 0.8)
  (which-key-show-early-on-C-h t)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode +1))

(use-package helpful
  :custom
  (helpful-max-buffers 2)
  :config
  (keymap-set helpful-mode-map "q" #'kill-buffer-and-window))

(use-package ace-link
  :defer 5
  :config (ace-link-setup-default))

(use-package command-log-mode
  :custom
  (command-log-mode-key-binding-open-log nil))

(use-package casual
  :after calc
  :demand t
  :config
  (keymap-set calc-mode-map "M-m" #'casual-main-menu))

(use-package jinx
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (setq jinx-languages "en_US"))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("M-$" . flyspell-correct-wrapper)))

(use-package cal-china-x
  :defer 5
  :config
  (setq mark-holidays-in-calendar t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")
                                       (holiday-lunar 7 7  "七夕节")
                                       (holiday-fixed 3 8  "妇女节")
                                       (holiday-fixed 3 12 "植树节")
                                       (holiday-fixed 5 4  "青年节")
                                       (holiday-fixed 6 1  "儿童节")
                                       (holiday-fixed 9 10 "教师节")))
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays)))

(use-package stardict
  :ensure nil
  :defer 5
  :init
  (setq stardict-name "langdao-ec-gb"
        stardict-dir (nol-expand-etc "stardict-langdao-ec-gb-2.4.2")))

(use-package bing-dict
  :init
  (setq bing-dict-vocabulary-save t
        bing-dict-cache-auto-save nil
        bing-dict-show-thesaurus 'synonym
        bing-dict-vocabulary-file (nol-expand-var "bing-dict-vocabulary.org")))

(use-package pyim
  :defer 5
  :commands (pyim-create-word-from-selection)
  :custom
  (pyim-outcome-trigger  nil)
  (pyim-enable-shortcode nil)
  (pyim-page-tooltip 'posframe)
  (pyim-candidates-search-buffer-p nil)
  (pyim-dcache-backend 'pyim-dregcach)
  (pyim-indicator-list '(pyim-indicator-with-modeline))
  (pyim-english-input-switch-functions '(pyim-probe-auto-english
                                         pyim-probe-program-mode
                                         pyim-probe-isearch-mode
                                         pyim-probe-org-latex-mode
                                         pyim-probe-org-structure-template))
  :config
  (require 'pyim-dregcache)
  (require 'pyim-cstring-utils)
  (setq default-input-method "pyim")
  (with-silent
   (pyim-default-scheme 'xiaohe-shuangpin))
  (keymap-set org-mode-map "M-f" #'pyim-forward-word)
  (keymap-set org-mode-map "M-b" #'pyim-backward-word)
  (setq pyim-dicts (list (list :name "dict-x"
                               :file (nol-expand-etc "dict-x.pyim")))))

(use-package pyim-tsinghua-dict
  ;; :vc (:url "https://github.com/redguardtoo/pyim-tsinghua-dict")
  :ensure nil
  :after pyim
  :demand t
  :init
  (unless (package-installed-p 'pyim-tsinghua-dict)
    (package-vc-install "https://github.com/redguardtoo/pyim-tsinghua-dict"))
  :config
  (with-silent (pyim-tsinghua-dict-enable)))

(use-package erc
  :ensure nil
  :custom
  (erc-nick "yxzz")
  (erc-join-buffer 'bury)
  (erc-interpret-mirc-color t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 15)
  (erc-prompt-for-password nil)
  (erc-prompt-for-nickserv-password nil)
  (erc-use-auth-source-for-nickserv-password t)
  (erc-sasl-user "xplutoy")
  (erc-sasl-auth-source-function #'erc-auth-source-search)
  (erc-hide-list '("JOIN" "NICK" "PART" "QUIT"))
  (erc-autojoin-channels-alist '(("#emacs" "#org-mode")))
  :config
  (appendq! erc-modules '(sasl services))
  (erc-update-modules)
  (erc-services-mode 1)
  (defalias 'erc 'erc-tls))

(use-package emms
  :defer 5
  :hook ((emms-browser-mode . hl-line-mode)
         (emms-browser-mode . turn-on-follow-mode))
  :custom
  (emms-lyrics-dir "~/Music/lyrics/")
  (emms-source-file-default-directory "~/Music/")
  (emms-source-playlist-default-format 'm3u)
  (emms-player-list '(emms-player-mpv))
  (emms-player-mpv-update-metadata t)
  (emms-info-asynchronously t)
  (emms-info-functions '(emms-info-native))
  (emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (emms-browser-thumbnail-small-size 64)
  (emms-browser-thumbnail-medium-size 128)
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-history-load)
  (emms-playing-time-display-mode -1)

  (transient-define-prefix yx/transient-emms ()
    "EMMS music"
    :transient-non-suffix 'transient--do-quit-one
    ["EMMS"
     ["Controls"
      ("p" "Play/Pause" emms-pause)
      ("s" "Stop" emms-stop)
      ("S" "Seek to time" emms-seek-to)
      ("n" "Next" emms-next)
      ("B" "Back (Previous)" emms-previous)
      ("b" "Back rewind" emms-seek-backward :transient transient--do-stay)
      ("f" "Fast-Forward" emms-seek-forward :transient transient--do-stay)]
     ["Playlist"
      ("N" "Cue Next" emms-cue-previous)
      ("P" "Cue Previous" emms-cue-previous)
      ("r" "Play Random" emms-random)
      ("R" "Toggle shuffle" emms-toggle-random-playlist)]
     ["Global/External"
      ("d" "Emms Dired" emms-play-dired)
      ("m" "Modeline" emms-mode-line-mode)
      ("M" "Current info" emms-show)
      ("e" "Emms" emms)]]))

(provide 'y-misc)
;;; y-misc.el ends here
