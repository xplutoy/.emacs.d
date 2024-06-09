;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 14:57:53
;; Modified: <>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-org-branching-context t)
  :config
  (let* ((auth-info (car (auth-source-search :user "moonshot-apikey")))
         (host (plist-get auth-info :host))
         (key (plist-get auth-info :secret)))
    (setq-default gptel-model "moonshot-v1-32k"
                  gptel-backend (gptel-make-openai "Moonshot"
                                  :host host
                                  :key key
                                  :models '("moonshot-v1-32k"
                                            "moonshot-v1-128k"))))
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(provide 'y-llm)
;;; y-llm.el ends here
