;;; -*- lexical-binding: t -*-
;;; License:
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'thingatpt)
(require 'seq)
(require 'tramp)
(require 'subr-x)

(defcustom crux-line-start-regex-alist
  '((term-mode . "^[^#$%>\n]*[#$%>] ")
    (eshell-mode . "^[^$\n]*$ ")
    (org-mode . "^\\(\*\\|[[:space:]]*\\)* ")
    (default . "^[[:space:]]*"))
  "Alist of major modes and line starts.

The key is a major mode.  The value is a regular expression
matching the characters to be skipped over.  If no major mode is
found, use the regex specified by the default key.

Used by crux functions like `crux-move-beginning-of-line' to skip
over whitespace, prompts, and markup at the beginning of the line."
  :type '(repeat (cons symbol regexp))
  :group 'crux)


(declare-function dired-get-file-for-visit "dired")

;;;###autoload
(defun yx/insert-date ()
  "Insert a date according to `%F' format."
  (interactive)
  (insert (format-time-string "%F" (current-time))))

;;;###autoload
(defun yx/comment-dwim (n)
  "Comment N lines, defaulting to the current one.
When the region is active, comment its lines instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line n)))

;;;###autoload
(defun yx/fill-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command #'yx/fill-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

;;;###autoload
(defun yx/scratch-buffer ()
  "Switch to '*scratch*' buffer and maximize it"
  (interactive)
  (scratch-buffer)
  (delete-other-windows)
  )

;;;###autoload
(defun yx/new-empty-buffer ()
  "Create a new empty buffer
Warning: new buffer is not prompted for save when killed, see `kill-buffer'."
  (interactive)
  (let ((xbuf (generate-new-buffer "untitled")))
    (set-buffer-major-mode xbuf)
    (switch-to-buffer xbuf)
    ))

;;;###autoload
(defun yx/kill-buffer-dwim (&optional arg)
  (interactive "P")
  (if arg
      (call-interactively 'kill-buffer)
    (kill-current-buffer))
  )

;;;###autoload
(defun crux-top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

;;;###autoload
(defun crux-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (insert "\n")
  (if electric-indent-inhibit
      ;; We can't use `indent-according-to-mode' in languages like Python,
      ;; as there are multiple possible indentations with different meanings.
      (let* ((indent-end (progn (crux-move-to-mode-line-start) (point)))
             (indent-start (progn (move-beginning-of-line nil) (point)))
             (indent-chars (buffer-substring indent-start indent-end)))
        (forward-line -1)
        ;; This new line should be indented with the same characters as
        ;; the current line.
        (insert indent-chars))
    ;; Just use the current major-mode's indent facility.
    (forward-line -1)
    (indent-according-to-mode)))

;;;###autoload
(defun crux-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.

With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (crux-smart-open-line-above)
    (move-end-of-line nil)
    (newline-and-indent)))

;;;###autoload
(defun crux-move-to-mode-line-start ()
  "Move to the beginning, skipping mode specific line start regex."
  (interactive)

  (let ((line-start-regex (cdr (seq-find
                                (lambda (e) (derived-mode-p (car e)))
                                crux-line-start-regex-alist
                                (assoc 'default crux-line-start-regex-alist)))))
    (search-forward-regexp line-start-regex (line-end-position) t)))

;;;###autoload
(defun crux-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (crux-move-to-mode-line-start)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))


;;;###autoload
(defun crux-open-with (arg)
  "Open visited file in default external program.
When in dired mode, open file under the cursor.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
          (if (derived-mode-p 'dired-mode)
              (dired-get-file-for-visit)
            buffer-file-name))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (call-process program nil 0 nil current-file-name)))


;;;###autoload
(defun crux-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun crux-file-owner-uid (filename)
  "Return the UID of the FILENAME as an integer.

See `file-attributes' for more info."
  (nth 2 (file-attributes filename 'integer)))

(defun crux-file-owned-by-user-p (filename)
  "Return t if file FILENAME is owned by the currently logged in user."
  (equal (crux-file-owner-uid filename)
         (user-uid)))

(defun crux-already-root-p ()
  (let ((remote-method (file-remote-p default-directory 'method))
        (remote-user (file-remote-p default-directory 'user)))
    (and remote-method
         (or (member remote-method '("sudo" "su" "ksu" "doas"))
             (string= remote-user "root")))))

(defun crux-find-alternate-file-as-root (filename)
  "Wraps `find-alternate-file' with opening FILENAME as root."
  (let ((remote-method (file-remote-p default-directory 'method))
        (remote-host (file-remote-p default-directory 'host))
        (remote-localname (file-remote-p filename 'localname)))
    (find-alternate-file (format "/%s:root@%s:%s"
                                 (or remote-method (if (executable-find "doas")
                                                       "doas"
                                                     "sudo"))
                                 (or remote-host "localhost")
                                 (or remote-localname filename)))))


;;;###autoload
(defun crux-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (let ((remote-method (file-remote-p default-directory 'method))
            (remote-host (file-remote-p default-directory 'host))
            (remote-localname (file-remote-p default-directory 'localname)))
        (find-file (format "/%s:root@%s:%s"
                           (or remote-method (if (executable-find "doas")
                                                 "doas"
                                               "sudo"))
                           (or remote-host "localhost")
                           (or remote-localname
                               (read-file-name "Find file (as root): ")))))

    (if (crux-already-root-p)
        (message "Already editing this file as root.")
      (let ((place (point)))
        (crux-find-alternate-file-as-root buffer-file-name)
        (goto-char place)))))

;;;###autoload
(defun crux-reopen-as-root ()
  "Find file as root if necessary.

Meant to be used as `find-file-hook'.
See also `crux-reopen-as-root-mode'."
  (unless (or (tramp-tramp-file-p buffer-file-name)
              (derived-mode-p 'dired-mode)
              (not (file-exists-p (file-name-directory buffer-file-name)))
              (file-writable-p buffer-file-name)
              (crux-file-owned-by-user-p buffer-file-name))
    (crux-find-alternate-file-as-root buffer-file-name)))

;;;###autoload
(define-minor-mode crux-reopen-as-root-mode
  "Automatically reopen files as root if we can't write to them
as the current user."
  :global t
  :group 'crux
  (if crux-reopen-as-root-mode
      (add-hook 'find-file-hook #'crux-reopen-as-root)
    (remove-hook 'find-file-hook #'crux-reopen-as-root)))

;;;###autoload
(defun crux-insert-timestamp ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

;;;###autoload
(defun crux-kill-other-buffers ()
  "Kill all buffers but the current one.
Doesn't mess with special buffers."
  (interactive)
  (when (y-or-n-p "Are you sure you want to kill all buffers but the current one? ")
    (seq-each
     #'kill-buffer
     (delete (current-buffer) (seq-filter #'buffer-file-name (buffer-list))))))

;; %%
(defmacro crux-with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer.

Use to make commands like `indent-region' work on both the region
and the entire buffer (in the absense of a region)."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(defmacro crux-with-region-or-line (func)
  "When called with no active region, call FUNC on current line."
  `(defadvice ,func (before with-region-or-line activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (line-beginning-position) (line-beginning-position 2))))))

(defmacro crux-with-region-or-sexp-or-line (func)
  "When called with no active region, call FUNC on current sexp/string, or line."
  `(defadvice ,func (before with-region-or-sexp-or-line activate compile)
     (interactive
      (cond
       (mark-active (list (region-beginning) (region-end)))
       ((in-string-p) (flatten-list (bounds-of-thing-at-point 'string)))
       ((thing-at-point 'list) (flatten-list (bounds-of-thing-at-point 'list)))
       (t (list (line-beginning-position) (line-beginning-position 2)))))))

(defmacro crux-with-region-or-point-to-eol (func)
  "When called with no active region, call FUNC from the point to the end of line."
  `(defadvice ,func (before with-region-or-point-to-eol activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point) (line-end-position))))))

;; %% borrow from https://gist.github.com/VernonGrant/1341a3bdcded3fc3a3741427f706ca85
;;;###autoload
(defun yx/quick-zap-up-to-char (p c)
  "The same as zap up to char, but without the mini buffer prompt.
P: The prefix argument or the count.
C: The character to zap up to."
  (interactive "P\nc")
  (let ((cnt (cond ((null p) 1)
                   ((symbolp p) -1)
                   (t p))))
    (zap-up-to-char cnt c)))

;; %% yx/org-return-dwin
(defun unpackaged/org-element-descendant-of (type element)
  "Return non-nil if ELEMENT is a descendant of TYPE.
TYPE should be an element type, like `item' or `paragraph'.
ELEMENT should be a list like that returned by `org-element-context'."
  ;; MAYBE: Use `org-element-lineage'.
  (when-let* ((parent (org-element-property :parent element)))
    (or (eq type (car parent))
        (unpackaged/org-element-descendant-of type parent))))

;;;###autoload
(defun yx/org-return-dwim (&optional default)
  "A helpful replacement for `org-return'.  With prefix, call `org-return'.

On headings, move point to position after entry.  In
lists, insert a new item or end the list, with checkbox if
appropriate.  In tables, insert a new row or end the table."
  ;; Inspired https://github.com/alphapapa/unpackaged.el#org-return-dwim
  (interactive "P")
  (if default
      (org-return)
    (cond
     ((org-at-heading-p)
      (org-reveal t)
      (org-fold-show-entry)
      (goto-char (org-entry-end-position))
      (insert "\n")
      (forward-line -1)
      )

     ((org-at-item-checkbox-p)
      ;; Checkbox: Insert new item with checkbox.
      (org-insert-todo-heading nil))

     ((org-in-item-p)
      ;; Plain list.  Yes, this gets a little complicated...
      (let ((context (org-element-context)))
        (if (or (eq 'plain-list (car context))  ; First item in list
                (and (eq 'item (car context))
                     (not (eq (org-element-property :contents-begin context)
                              (org-element-property :contents-end context))))
                (unpackaged/org-element-descendant-of 'item context))  ; Element in list item, e.g. a link
            ;; Non-empty item: Add new item.
            (org-insert-item)
          ;; Empty item: Close the list.
          ;; TODO: Do this with org functions rather than operating on the text. Can't seem to find the right function.
          (delete-region (line-beginning-position) (line-end-position))
          (insert "\n"))))

     ((when (fboundp 'org-inlinetask-in-task-p)
        (org-inlinetask-in-task-p))
      ;; Inline task: Don't insert a new heading.
      (org-return))

     ((org-at-table-p)
      (cond ((save-excursion
               (beginning-of-line)
               ;; See `org-table-next-field'.
               (cl-loop with end = (line-end-position)
                        for cell = (org-element-table-cell-parser)
                        always (equal (org-element-property :contents-begin cell)
                                      (org-element-property :contents-end cell))
                        while (re-search-forward "|" end t)))
             ;; Empty row: end the table.
             (delete-region (line-beginning-position) (line-end-position))
             (org-return))
            (t
             ;; Non-empty row: call `org-return'.
             (org-return))))
     (t
      ;; All other cases: call `org-return'.
      (org-return)))))

;;;###autoload
(defmacro yx/def-org-maybe-surround (&rest keys)
  "Define and bind interactive commands for each of KEYS that surround the region or insert text.
Commands are bound in `org-mode-map' to each of KEYS.  If the
region is active, commands surround it with the key character,
otherwise call `org-self-insert-command'."
  `(progn
     ,@(cl-loop for key in keys
                for name = (intern (concat "unpackaged/org-maybe-surround-" key))
                for docstring = (format "If region is active, surround it with \"%s\", otherwise call `org-self-insert-command'." key)
                collect `(defun ,name ()
                           ,docstring
                           (interactive)
                           (if (region-active-p)
                               (let ((beg (region-beginning))
                                     (end (region-end)))
                                 (save-excursion
                                   (goto-char end)
                                   (insert ,key)
                                   (goto-char beg)
                                   (insert ,key)))
                             (call-interactively #'org-self-insert-command)))
                collect `(define-key org-mode-map (kbd ,key) #',name))))

;; https://emacstil.com/til/2021/09/09/fold-heading/
;;;###autoload
(defun yx/org-show-current-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (interactive)
  (if (save-excursion
        (end-of-line)
        (outline-invisible-p))
      (progn
        (org-fold-show-entry)
        (outline-show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (outline-hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-fold-show-entry)
    (outline-show-children))
  )

;;;###autoload
(defun yx/org-link-copy (&optional arg)
  "Extract URL from org-mode link and add it to kill ring."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
	 (type (org-element-property :type link))
	 (url (org-element-property :path link))
	 (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied URL: " url))))


;; https://emacs-china.org/t/org-link/13502
;;;###autoload
(defun yx/org-display-subtree-inline-images ()
  "Toggle the display of inline images.
INCLUDE-LINKED is passed to `org-display-inline-images'."
  (interactive)
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (let* ((beg (point-min))
             (end (point-max))
             (image-overlays (cl-intersection
                              org-inline-image-overlays
                              (overlays-in beg end))))
        (if image-overlays
            (progn
              (org-remove-inline-images)
              (message "Inline image display turned off"))
          (org-display-inline-images t t beg end)
          (setq image-overlays (cl-intersection
                                org-inline-image-overlays
                                (overlays-in beg end)))
          (if (and (org-called-interactively-p) image-overlays)
              (message "%d images displayed inline"
                       (length image-overlays))))))))

;;; https://emacs-china.org/t/elfeed-nerd-icons/26125
;;;###autoload
(defun yx/elfeed-search-print-entry--better-default (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (date-width (car (cdr elfeed-search-date-format)))
         (title (concat (or (elfeed-meta entry :title)
                            (elfeed-entry-title entry) "")
                        " "))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title (when feed (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat (lambda (s) (propertize s 'face 'elfeed-search-tag-face)) tags ","))
         (title-width (- (frame-width)
                         ;; (window-width (get-buffer-window (elfeed-search-buffer) t))
                         date-width elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width) :left))
         (align-to-feed-pixel (+ date-width
                                 (max elfeed-search-title-min-width
                                      (min title-width elfeed-search-title-max-width)))))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (insert (propertize title-column 'face title-faces 'kbd-help title))
    (put-text-property (1- (point)) (point) 'display `(space :align-to ,align-to-feed-pixel))
    (when feed-title (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when tags (insert "(" tags-str ")"))))


;; borrow from https://protesilaos.com/emacs/dotemacs
;;;###autoload
(defun yx/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit)))
  )


;; %% end
(provide 'crux)
