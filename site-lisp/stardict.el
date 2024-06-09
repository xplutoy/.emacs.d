;;; -*- lexical-binding: t -*-

;; Author: Changyuan Yu <rei.vzy@gmail.com>
;; Maintainer: yangxue <yangxue.cs@foxmail.com>
;; Created: 2010-11-06
;; Modified: <2024-02-04 09:43:15 yx>
;; Licence: GPLv3

;; Example:
;;
;; (require 'stardict)
;; (setq dict
;;       (stardict-open "~/.stardict/dic/stardict-lazyworm-ec-2.4.2"
;;                      "lazyworm-ec"))
;; (stardict-word-exist-p dict "apple")
;; (stardict-lookup dict "apple")

;; (stardict-open-dict-file dict)
;; (mapcar (lambda (x) (stardict-lookup dict x)) (make-list 1000 "apple"))


;;; Code:

(defun stardict-str2int (str)
  "Convert string `STR' to integer.
\x21\x22 => 0x2122"
  (let ((sum 0))
    (mapc (lambda (c)
            (setq sum (+ (* sum #x100)
                         (mod c #x100))))
          str)
    sum))

(defun stardict-open (dir name &optional nocache)
  "Open stardict dictionary in directory `DIR' with name `NAME'.
When `NOCACHE' is not nil, don't load from cache and save to cache.
The return is used as `DICT' argument in other functions."
  (if nocache (stardict-open-1 dir name)
    (let ((cache (expand-file-name (concat name ".idx.emacs.bz2") dir)) ret)
      (if (file-exists-p cache)
          (with-temp-buffer
            (insert-file-contents cache)
            (read (current-buffer)))
        (setq ret (stardict-open-1 dir name))
        (with-temp-buffer
          (prin1 ret (current-buffer))
          (write-region nil nil cache))
        ret))))

(defun stardict-open-1 (dir name)
  "Internal function used by `stardict-open'.
`DIR' is dictionary location, `NAME' is dictionary name."
  (let ((ifo  (expand-file-name (concat name ".ifo") dir))
        (idx  (expand-file-name (concat name ".idx") dir))
        (dict (expand-file-name (concat name ".dict") dir))
        (idx-offset-bytes 4)
        (word-count 0)
        ifo-ht idx-ht)
    (unless (file-exists-p idx)
      (setq idx (concat idx ".gz")))
    (unless (file-exists-p dict)
      (setq dict (concat dict ".dz")))
    ;;(message "List %S" (list idx dict ifo))
    (unless (and (file-exists-p idx)
                 (file-exists-p dict)
                 (file-exists-p ifo))
      (error "File not found"))
    (setq ifo-ht (make-hash-table :test 'equal))
    (setq idx-ht (make-hash-table :test 'equal))
    ;; get info
    (with-temp-buffer
      (insert-file-contents ifo)
      (goto-char (point-min))
      (while (re-search-forward "^\\([a-zA-Z]+\\)=\\(.*\\)$" nil t)
        (puthash (match-string 1) (match-string 2) ifo-ht)))
    (when (gethash "idxoffsetbits" ifo-ht)
      (setq idx-offset-bytes
            (/ (string-to-number (gethash "idxoffsetbits" ifo-ht)) 8)))
    (setq word-count
          (string-to-number (gethash "wordcount" ifo-ht)))
    ;; get index
    (with-temp-buffer
      (insert-file-contents idx)
      (goto-char (point-min))
      (let ((rpt (make-progress-reporter "read index: " 0 (1- word-count))))
        (dotimes (i word-count)
          (progress-reporter-update rpt i)
          (let (p word offset size)
            (re-search-forward "\\([^\x00]+?\\)\x00" nil t)
            (setq p (point))
            (setq word (decode-coding-string (encode-coding-string (match-string 1) 'no-conversion) 'utf-8))
            (setq offset
                  (stardict-str2int
                   (buffer-substring-no-properties p
                                                   (+ p idx-offset-bytes))))
            (setq size
                  (stardict-str2int
                   (buffer-substring-no-properties (+ p idx-offset-bytes)
                                                   (+ p idx-offset-bytes 4))))
            (forward-char (+ idx-offset-bytes 4))
            (puthash word (cons offset size) idx-ht)
            )))
      (list ifo-ht idx-ht dict))))

(defun stardict-word-exist-p (dict word)
  "Checkout whether `WORD' existed in `DICT'."
  (gethash word (nth 1 dict)))

(defun stardict-lookup (dict word)
  "Lookup `WORD' in `DICT', return nil when not found."
  (let ((info (gethash word (nth 1 dict)))
        (file (nth 2 dict))
        buffer
        offset size begin end)
    (when info
      (setq offset (car info))
      (setq size (cdr info))
      ;; find any opened dict file
      (dolist (buf (buffer-list))
        (when (equal file (buffer-file-name buf))
          (setq buffer buf)))
      (if buffer
          (with-current-buffer buffer
            (buffer-substring-no-properties (byte-to-position (1+ offset))
                                            (byte-to-position (+ 1 offset size))))
        (with-temp-buffer
          (insert-file-contents (nth 2 dict) nil offset (+ offset size))
          (buffer-string))))))

(defun stardict-open-dict-file (dict)
  "Open dict file of `DICT' in Emacs to speed up word lookup.
You should close the dict file yourself."
  (with-current-buffer (find-file-noselect (nth 2 dict))
    (setq buffer-read-only t)))

(defvar stardict-dir nil)
(defvar stardict-name nil)
(defvar stardict-dict-hash nil)

(defun stardict--load-dict ()
  "load dictionary when call for first time."
  (unless stardict-dict-hash
    (setq stardict-dict-hash
          (stardict-open stardict-dir stardict-name))))

(defun stardict--lookup-and-display (word)
  (with-current-buffer (get-buffer-create "*stardict*")
    (erase-buffer)
    (insert (concat word "\n"))
    (insert (stardict-lookup stardict-dict-hash (string-trim (downcase word))))
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))
    (keymap-local-set "q" 'quit-window)))

;;;###autoload
(defun stardict-define-at-point ()
  "Define the word at point."
  (interactive)
  (stardict--load-dict)
  (let ((word (thing-at-point 'word)))
    ;; if word is not in dictionary, try to truncate
    (unless (stardict-word-exist-p stardict-dict-hash word)
      (let ((trunc-char
             (cond
              ((string-match "^.*ed$" word) -2)
              ((string-match "^.*ing$" word) -3)
              ((string-match "^.*es$" word) -2)
              ((string-match "^.*'s$" word) -2)
              ((string-match "^.*s$" word) -1))))
        (if trunc-char
            (setq word (substring word 0 trunc-char))
          (setq word nil))))
    (if word
        (stardict--lookup-and-display word)
      (message "No definition is found!"))))

;;;###autoload
(defun stardict-define (word)
  "Prompt for `WORD' and define it."
  (interactive "sWord: ")
  (stardict--load-dict)
  (if (stardict-word-exist-p stardict-dict-hash word)
      (stardict--lookup-and-display word)
    (message "No definition is found!")))

(provide 'stardict)
;;; stardict.el ends here
