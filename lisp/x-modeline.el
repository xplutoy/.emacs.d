;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-09 11:46:38
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(require 'x-common)

(defgroup x-modeline nil
  "Custom modeline that is stylistically close to the default."
  :group 'mode-line)

(defgroup x-modeline-faces nil
  "Faces for my custom modeline."
  :group 'x-modeline)

(defcustom x-modeline-string-truncate-length 12
  "String length after which truncation should be done in small windows."
  :type 'natnum)

;;;; face

(defface x-modeline-indicator-button nil
  "Generic face used for indicators that have a background.
Modify this face to, for example, add a :box attribute to all
relevant indicators (combines nicely with `spacious-padding'
package).")

(defface x-modeline-indicator-red
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#880000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff9f9f")
    (t :foreground "red"))
  "Face for modeline indicators."
  :group 'x-modeline-faces)

(defface x-modeline-indicator-blue-bg
  '((default :inherit (bold x-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "black")
    (t :background "blue" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'x-modeline-faces)

(defface x-modeline-indicator-green-bg
  '((default :inherit (bold x-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#207b20" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77d077" :foreground "black")
    (t :background "green" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'x-modeline-faces)

(defface x-modeline-indicator-red-bg
  '((default :inherit (bold x-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#aa1111" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ff9090" :foreground "black")
    (t :background "red" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'x-modeline-faces)

(defface x-modeline-indicator-gray-bg
  '((default :inherit (bold x-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#808080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#a0a0a0" :foreground "black")
    (t :inverse-video t))
  "Face for modeline indicatovrs with a background."
  :group 'x-modeline-faces)

(defface x-modeline-indicator-cyan-bg
  '((default :inherit (bold prot-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#006080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#40c0e0" :foreground "black")
    (t :background "cyan" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'x-modeline-faces)

;;;; Buffer status

(defvar-local x-modeline-buffer-status
    '(:eval
      (when (file-remote-p default-directory)
        (propertize " @ "
                    'face 'x-modeline-indicator-red-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for showing remote file name.")


;;;; Helper functions
(defun x-modeline--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (if (string-empty-p str)
      str
    (and (x-common-window-narrow-p)
         (> (length str) x-modeline-string-truncate-length)
         (not (one-window-p :no-minibuffer)))))

(defun x-modeline-string-cut-end (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the end of STR by counting from its start up to
`x-modeline-string-truncate-length'."
  (if (x-modeline--string-truncate-p str)
      (concat (substring str 0 x-modeline-string-truncate-length) "...")
    str))

(defun x-modeline-string-cut-middle (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the middle of STR by counting half of
`x-modeline-string-truncate-length' both from its beginning
and end."
  (let ((half (floor x-modeline-string-truncate-length 2)))
    (if (x-modeline--string-truncate-p str)
        (concat (substring str 0 half) "..." (substring str (- half)))
      str)))

(defun x-modeline--first-char (str)
  "Return first character from STR."
  (substring str 0 1))

(defun x-modeline-string-abbreviate (str)
  "Abbreviate STR individual hyphen or underscore separated words.
Also see `x-modeline-string-abbreviate-but-last'."
  (if (x-modeline--string-truncate-p str)
      (mapconcat #'x-modeline--first-char (split-string str "[_-]") "-")
    str))

(defun x-modeline-string-abbreviate-but-last (str nthlast)
  "Abbreviate STR, keeping NTHLAST words intact.
Also see `x-modeline-string-abbreviate'."
  (if (x-modeline--string-truncate-p str)
      (let* ((all-strings (split-string str "[_-]"))
             (nbutlast-strings (nbutlast (copy-sequence all-strings) nthlast))
             (last-strings (nreverse (ntake nthlast (nreverse (copy-sequence all-strings)))))
             (first-component (mapconcat #'x-modeline--first-char nbutlast-strings "-"))
             (last-component (mapconcat #'identity last-strings "-")))
        (if (string-empty-p first-component)
            last-component
          (concat first-component "-" last-component)))
    str))

;;;; Keyboard macro indicator

(defvar-local x-modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face 'x-modeline-indicator-blue-bg)))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

;;;; Narrow indicator

(defvar-local x-modeline-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Narrow " 'face 'x-modeline-indicator-cyan-bg)))
  "Mode line construct to report the multilingual environment.")

;;;; Input method

(defvar-local x-modeline-input-method
    '(:eval
      (when current-input-method-title
        (propertize (format " %s " current-input-method-title)
                    'face 'x-modeline-indicator-green-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct to report the multilingual environment.")

;;;; Dedicated window

(defvar-local x-modeline-window-dedicated-status
    '(:eval
      (when (window-dedicated-p)
        (propertize " = "
                    'face 'x-modeline-indicator-gray-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for dedicated window indicator.")

;;;; Buffer name and modified status

(defun x-modeline-buffer-identification-face ()
  "Return appropriate face or face list for `x-modeline-buffer-identification'."
  (let ((file (buffer-file-name)))
    (cond
     ((and (mode-line-window-selected-p)
           file
           (buffer-modified-p))
      '(italic mode-line-buffer-id))
     ((and file (buffer-modified-p))
      'italic)
     ((mode-line-window-selected-p)
      'mode-line-buffer-id))))

(defun x-modeline--buffer-name ()
  "Return `buffer-name', truncating it if necessary.
See `x-modeline-string-cut-middle'."
  (when-let ((name (buffer-name)))
    (x-modeline-string-cut-middle name)))

(defun x-modeline-buffer-name ()
  "Return buffer name, with read-only indicator if relevant."
  (let ((name (x-modeline--buffer-name)))
    (if buffer-read-only
        (format "%s %s" (char-to-string #xE0A2) name)
      name)))

(defun x-modeline-buffer-name-help-echo ()
  "Return `help-echo' value for `x-modeline-buffer-identification'."
  (concat
   (propertize (buffer-name) 'face 'mode-line-buffer-id)
   "\n"
   (propertize
    (or (buffer-file-name)
        (format "No underlying file.\nDirectory is: %s" default-directory))
    'face 'font-lock-doc-face)))

(defvar-local x-modeline-buffer-identification
    '(:eval
      (propertize (x-modeline-buffer-name)
                  'face (x-modeline-buffer-identification-face)
                  'mouse-face 'mode-line-highlight
                  'help-echo (x-modeline-buffer-name-help-echo)))
  "Mode line construct for identifying the buffer being displayed.
Propertize the current buffer with the `mode-line-buffer-id'
face.  Let other buffers have no face.")

;;;; Major mode

(defun x-modeline-major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    (t "Ⓜ"))))
    (propertize indicator 'face 'shadow)))

(defun x-modeline-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defun x-modeline-major-mode-help-echo ()
  "Return `help-echo' value for `x-modeline-major-mode'."
  (if-let ((parent (get major-mode 'derived-mode-parent)))
      (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
    (format "Symbol: `%s'." major-mode)))

(defvar-local x-modeline-major-mode
    (list
     (propertize "%[" 'face 'x-modeline-indicator-red)
     '(:eval
       (concat
        (x-modeline-major-mode-indicator)
        " "
        (propertize
         (x-modeline-string-abbreviate-but-last
          (x-modeline-major-mode-name)
          2)
         'mouse-face 'mode-line-highlight
         'help-echo (x-modeline-major-mode-help-echo))))
     (propertize "%]" 'face 'x-modeline-indicator-red))
  "Mode line construct for displaying major modes.")


(defvar-local x-modeline-process
    (list '("" mode-line-process))
  "Mode line construct for the running process indicator.")

;;;; Git branch and diffstat

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defun x-modeline--vc-branch-name (file backend)
  "Return capitalized VC branch name for FILE with BACKEND."
  (when-let ((rev (vc-working-revision file backend))
             (branch (or (vc-git--symbolic-ref file)
                         (substring rev 0 7))))
    (capitalize branch)))

(declare-function vc-git-working-revision "vc-git" (file))

(defvar x-modeline-vc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'vc-diff)
    (define-key map [mode-line down-mouse-3] 'vc-root-diff)
    map)
  "Keymap to display on VC indicator.")

(defun x-modeline--vc-help-echo (file)
  "Return `help-echo' message for FILE tracked by VC."
  (format "Revision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
          (vc-working-revision file)))

(defun x-modeline--vc-text (file branch &optional face)
  "Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
  (concat
   (propertize (char-to-string #xE0A0) 'face 'shadow)
   " "
   (propertize branch
               'face face
               'mouse-face 'mode-line-highlight
               'help-echo (x-modeline--vc-help-echo file)
               'local-map x-modeline-vc-map)
   ))

(defun x-modeline--vc-details (file branch &optional face)
  "Return Git BRANCH details for FILE, truncating it if necessary.
The string is truncated if the width of the window is smaller
than `split-width-threshold'."
  (x-modeline-string-cut-end
   (x-modeline--vc-text file branch face)))

(defvar x-modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state))
  "VC state faces.")

(defun x-modeline--vc-get-face (key)
  "Get face from KEY in `x-modeline--vc-faces'."
  (alist-get key x-modeline--vc-faces 'up-to-date))

(defun x-modeline--vc-face (file backend)
  "Return VC state face for FILE with BACKEND."
  (x-modeline--vc-get-face (vc-state file backend)))

(defvar-local x-modeline-vc-branch
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (buffer-file-name))
                  (backend (vc-backend file))
                  ;; ((vc-git-registered file))
                  (branch (x-modeline--vc-branch-name file backend))
                  (face (x-modeline--vc-face file backend)))
        (x-modeline--vc-details file branch face)))
  "Mode line construct to return propertized VC branch.")

;;;; Flymake errors, warnings, notes

(declare-function flymake--severity "flymake" (type))
(declare-function flymake-diagnostic-type "flymake" (diag))

;; Based on `flymake--mode-line-counter'.
(defun x-modeline-flymake-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))

(defvar x-modeline-flymake-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'flymake-show-buffer-diagnostics)
    (define-key map [mode-line down-mouse-3] 'flymake-show-project-diagnostics)
    map)
  "Keymap to display on Flymake indicator.")

(defmacro x-modeline-flymake-type (type indicator &optional face)
  "Return function that handles Flymake TYPE with stylistic INDICATOR and FACE."
  `(defun ,(intern (format "x-modeline-flymake-%s" type)) ()
     (when-let ((count (x-modeline-flymake-counter
                        ,(intern (format ":%s" type)))))
       (concat
        (propertize ,indicator 'face 'shadow)
        (propertize count
                    'face ',(or face type)
                    'mouse-face 'mode-line-highlight
                    'local-map x-modeline-flymake-map
                    'help-echo "mouse-1: buffer diagnostics\nmouse-3: project diagnostics")))))

(x-modeline-flymake-type error "☣")
(x-modeline-flymake-type warning "!")
(x-modeline-flymake-type note "·" success)

(defvar-local x-modeline-flymake
    `(:eval
      (when (and (bound-and-true-p flymake-mode)
                 (mode-line-window-selected-p))
        (list
         ;; See the calls to the macro `x-modeline-flymake-type'
         '(:eval (x-modeline-flymake-error))
         '(:eval (x-modeline-flymake-warning))
         '(:eval (x-modeline-flymake-note)))))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")

;;;; Eglot

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(defvar-local x-modeline-eglot
    `(:eval
      (when (and (featurep 'eglot) (mode-line-window-selected-p))
        '(eglot--managed-mode eglot--mode-line-format)))
  "Mode line construct displaying Eglot information.
Specific to the current window's mode line.")

;;;; Mlscrol

(declare-function mlscroll-mode-line "mlscroll")

(defun x-modeline-window-selected-advice(orig-fun &rest args)
  (if (mode-line-window-selected-p)
      (apply orig-fun args)
    ""))

(advice-add 'mlscroll-mode-line :around #'x-modeline-window-selected-advice)

;;;; Miscellaneous

(defvar-local x-modeline-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")


;;;; Risky local variables

(dolist (construct '(x-modeline-kbd-macro
                     x-modeline-narrow
                     x-modeline-input-method
                     x-modeline-buffer-status
                     x-modeline-window-dedicated-status
                     x-modeline-buffer-identification
                     x-modeline-major-mode
                     x-modeline-process
                     x-modeline-vc-branch
                     x-modeline-flymake
                     x-modeline-eglot
                     x-modeline-misc-info))
  (put construct 'risky-local-variable t))



(provide 'x-modeline)
;;; x-modeline.el ends here
