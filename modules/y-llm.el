;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 14:57:53
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-org-branching-context t)
  :config
  (setq gptel-log-level 'debug)
  (gptel-make-openai "Kimi"
    :host "api.moonshot.cn"
    :key (x-common-auth-get-field "api.moonshot.cn" :secret)
    :models '("moonshot-v1-32k" "moonshot-v1-128k")
    :stream t)
  (setq gptel-model   "deepseek-chat"
        gptel-backend (gptel-make-openai "DeepSeek"
                        :host "api.deepseek.com"
                        :endpoint "/chat/completions"
                        :stream t
                        :key (x-common-auth-get-field "api.deepseek.com" :secret)
                        :models '("deepseek-chat" "deepseek-coder")))
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(provide 'y-llm)
;;; y-llm.el ends here
