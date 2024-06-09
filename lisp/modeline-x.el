;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-09 11:46:38
;; Modified: <2024-06-09 17:19:25 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(require 'common-x)

(defgroup modeline-x nil
  "Custom modeline that is stylistically close to the default."
  :group 'mode-line)

(defgroup modeline-x-faces nil
  "Faces for my custom modeline."
  :group 'modeline-x)

(defcustom yx-modeline-string-truncate-length 12
  "String length after which truncation should be done in small windows."
  :type 'natnum)

;;;; face

(defface yx-modeline-indicator-button nil
  "Generic face used for indicators that have a background.
Modify this face to, for example, add a :box attribute to all
relevant indicators (combines nicely with `spacious-padding'
package).")

(defface yx-modeline-indicator-red
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#880000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff9f9f")
    (t :foreground "red"))
  "Face for modeline indicators."
  :group 'modeline-x-faces)

(defface yx-modeline-indicator-blue-bg
  '((default :inherit (bold yx-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "black")
    (t :background "blue" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'modeline-x-faces)

(defface yx-modeline-indicator-green-bg
  '((default :inherit (bold yx-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#207b20" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77d077" :foreground "black")
    (t :background "green" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'modeline-x-faces)

(defface yx-modeline-indicator-red-bg
  '((default :inherit (bold yx-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#aa1111" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ff9090" :foreground "black")
    (t :background "red" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'modeline-x-faces)

(defface yx-modeline-indicator-gray-bg
  '((default :inherit (bold yx-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#808080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#a0a0a0" :foreground "black")
    (t :inverse-video t))
  "Face for modeline indicatovrs with a background."
  :group 'modeline-x-faces)

;;;; Buffer status

(defvar-local yx-modeline-buffer-status
    '(:eval
      (when (file-remote-p default-directory)
        (propertize " @ "
                    'face 'yx-modeline-indicator-red-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for showing remote file name.")


;;;; Helper functions
(defun yx/modeline--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (if (string-empty-p str)
      str
    (and (yx/common-window-narrow-p)
         (> (length str) yx-modeline-string-truncate-length)
         (not (one-window-p :no-minibuffer)))))

(defun yx/modeline-string-cut-end (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the end of STR by counting from its start up to
`yx-modeline-string-truncate-length'."
  (if (yx/modeline--string-truncate-p str)
      (concat (substring str 0 yx-modeline-string-truncate-length) "...")
    str))

(defun yx/modeline-string-cut-middle (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the middle of STR by counting half of
`yx-modeline-string-truncate-length' both from its beginning
and end."
  (let ((half (floor yx-modeline-string-truncate-length 2)))
    (if (yx/modeline--string-truncate-p str)
        (concat (substring str 0 half) "..." (substring str (- half)))
      str)))

(defun yx/modeline--first-char (str)
  "Return first character from STR."
  (substring str 0 1))

(defun yx/modeline-string-abbreviate (str)
  "Abbreviate STR individual hyphen or underscore separated words.
Also see `yx/modeline-string-abbreviate-but-last'."
  (if (yx/modeline--string-truncate-p str)
      (mapconcat #'yx/modeline--first-char (split-string str "[_-]") "-")
    str))

(defun yx/modeline-string-abbreviate-but-last (str nthlast)
  "Abbreviate STR, keeping NTHLAST words intact.
Also see `yx/modeline-string-abbreviate'."
  (if (yx/modeline--string-truncate-p str)
      (let* ((all-strings (split-string str "[_-]"))
             (nbutlast-strings (nbutlast (copy-sequence all-strings) nthlast))
             (last-strings (nreverse (ntake nthlast (nreverse (copy-sequence all-strings)))))
             (first-component (mapconcat #'yx/modeline--first-char nbutlast-strings "-"))
             (last-component (mapconcat #'identity last-strings "-")))
        (if (string-empty-p first-component)
            last-component
          (concat first-component "-" last-component)))
    str))

;;;; Keyboard macro indicator

(defvar-local yx-modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face 'yx-modeline-indicator-blue-bg)))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

;;;; Narrow indicator

(defvar-local yx-modeline-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Narrow " 'face 'prot-modeline-indicator-cyan-bg)))
  "Mode line construct to report the multilingual environment.")

;;;; Input method

(defvar-local yx-modeline-input-method
    '(:eval
      (when current-input-method-title
        (propertize (format " %s " current-input-method-title)
                    'face 'yx-modeline-indicator-green-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct to report the multilingual environment.")

;;;; Dedicated window

(defvar-local yx-modeline-window-dedicated-status
    '(:eval
      (when (window-dedicated-p)
        (propertize " = "
                    'face 'yx-modeline-indicator-gray-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for dedicated window indicator.")

;;;; Buffer name and modified status

(defun yx/modeline-buffer-identification-face ()
  "Return appropriate face or face list for `yx-modeline-buffer-identification'."
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

(defun yx/modeline--buffer-name ()
  "Return `buffer-name', truncating it if necessary.
See `yx/modeline-string-cut-middle'."
  (when-let ((name (buffer-name)))
    (yx/modeline-string-cut-middle name)))

(defun yx/modeline-buffer-name ()
  "Return buffer name, with read-only indicator if relevant."
  (let ((name (yx/modeline--buffer-name)))
    (if buffer-read-only
        (format "%s %s" (char-to-string #xE0A2) name)
      name)))

(defun yx/modeline-buffer-name-help-echo ()
  "Return `help-echo' value for `yx-modeline-buffer-identification'."
  (concat
   (propertize (buffer-name) 'face 'mode-line-buffer-id)
   "\n"
   (propertize
    (or (buffer-file-name)
        (format "No underlying file.\nDirectory is: %s" default-directory))
    'face 'font-lock-doc-face)))

(defvar-local yx-modeline-buffer-identification
    '(:eval
      (propertize (yx/modeline-buffer-name)
                  'face (yx/modeline-buffer-identification-face)
                  'mouse-face 'mode-line-highlight
                  'help-echo (yx/modeline-buffer-name-help-echo)))
  "Mode line construct for identifying the buffer being displayed.
Propertize the current buffer with the `mode-line-buffer-id'
face.  Let other buffers have no face.")

;;;; Major mode

(defun yx/modeline-major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    (t "Ⓜ"))))
    (propertize indicator 'face 'shadow)))

(defun yx/modeline-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defun yx/modeline-major-mode-help-echo ()
  "Return `help-echo' value for `yx-modeline-major-mode'."
  (if-let ((parent (get major-mode 'derived-mode-parent)))
      (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
    (format "Symbol: `%s'." major-mode)))

(defvar-local yx-modeline-major-mode
    (list
     (propertize "%[" 'face 'yx-modeline-indicator-red)
     '(:eval
       (concat
        (yx/modeline-major-mode-indicator)
        " "
        (propertize
         (yx/modeline-string-abbreviate-but-last
          (yx/modeline-major-mode-name)
          2)
         'mouse-face 'mode-line-highlight
         'help-echo (yx/modeline-major-mode-help-echo))))
     (propertize "%]" 'face 'yx-modeline-indicator-red))
  "Mode line construct for displaying major modes.")


(defvar-local yx-modeline-process
    (list '("" mode-line-process))
  "Mode line construct for the running process indicator.")

;;;; Git branch and diffstat

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defun yx/modeline--vc-branch-name (file backend)
  "Return capitalized VC branch name for FILE with BACKEND."
  (when-let ((rev (vc-working-revision file backend))
             (branch (or (vc-git--symbolic-ref file)
                         (substring rev 0 7))))
    (capitalize branch)))

(declare-function vc-git-working-revision "vc-git" (file))

(defvar yx-modeline-vc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'vc-diff)
    (define-key map [mode-line down-mouse-3] 'vc-root-diff)
    map)
  "Keymap to display on VC indicator.")

(defun yx/modeline--vc-help-echo (file)
  "Return `help-echo' message for FILE tracked by VC."
  (format "Revision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
          (vc-working-revision file)))

(defun yx/modeline--vc-text (file branch &optional face)
  "Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
  (concat
   (propertize (char-to-string #xE0A0) 'face 'shadow)
   " "
   (propertize branch
               'face face
               'mouse-face 'mode-line-highlight
               'help-echo (yx/modeline--vc-help-echo file)
               'local-map yx-modeline-vc-map)
   ))

(defun yx/modeline--vc-details (file branch &optional face)
  "Return Git BRANCH details for FILE, truncating it if necessary.
The string is truncated if the width of the window is smaller
than `split-width-threshold'."
  (yx/modeline-string-cut-end
   (yx/modeline--vc-text file branch face)))

(defvar yx-modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state))
  "VC state faces.")

(defun yx/modeline--vc-get-face (key)
  "Get face from KEY in `yx/modeline--vc-faces'."
  (alist-get key yx-modeline--vc-faces 'up-to-date))

(defun yx/modeline--vc-face (file backend)
  "Return VC state face for FILE with BACKEND."
  (yx/modeline--vc-get-face (vc-state file backend)))

(defvar-local yx-modeline-vc-branch
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (buffer-file-name))
                  (backend (vc-backend file))
                  ;; ((vc-git-registered file))
                  (branch (yx/modeline--vc-branch-name file backend))
                  (face (yx/modeline--vc-face file backend)))
        (yx/modeline--vc-details file branch face)))
  "Mode line construct to return propertized VC branch.")

;;;; Flymake errors, warnings, notes

(declare-function flymake--severity "flymake" (type))
(declare-function flymake-diagnostic-type "flymake" (diag))

;; Based on `flymake--mode-line-counter'.
(defun yx/modeline-flymake-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))

(defvar yx-modeline-flymake-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'flymake-show-buffer-diagnostics)
    (define-key map [mode-line down-mouse-3] 'flymake-show-project-diagnostics)
    map)
  "Keymap to display on Flymake indicator.")

(defmacro yx/modeline-flymake-type (type indicator &optional face)
  "Return function that handles Flymake TYPE with stylistic INDICATOR and FACE."
  `(defun ,(intern (format "yx/modeline-flymake-%s" type)) ()
     (when-let ((count (yx/modeline-flymake-counter
                        ,(intern (format ":%s" type)))))
       (concat
        (propertize ,indicator 'face 'shadow)
        (propertize count
                    'face ',(or face type)
                    'mouse-face 'mode-line-highlight
                    'local-map yx-modeline-flymake-map
                    'help-echo "mouse-1: buffer diagnostics\nmouse-3: project diagnostics")))))

(yx/modeline-flymake-type error "☣")
(yx/modeline-flymake-type warning "!")
(yx/modeline-flymake-type note "·" success)

(defvar-local yx-modeline-flymake
    `(:eval
      (when (and (bound-and-true-p flymake-mode)
                 (mode-line-window-selected-p))
        (list
         ;; See the calls to the macro `yx-modeline-flymake-type'
         '(:eval (yx/modeline-flymake-error))
         '(:eval (yx/modeline-flymake-warning))
         '(:eval (yx/modeline-flymake-note)))))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")

;;;; Eglot

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(defvar-local yx-modeline-eglot
    `(:eval
      (when (and (featurep 'eglot) (mode-line-window-selected-p))
        '(eglot--managed-mode eglot--mode-line-format)))
  "Mode line construct displaying Eglot information.
Specific to the current window's mode line.")

;;;; Mlscrol

(declare-function mlscroll-mode-line "mlscroll")

(defun yx/modeline-window-selected-advice(orig-fun &rest args)
  (if (mode-line-window-selected-p)
      (apply orig-fun args)
    ""))

(advice-add 'mlscroll-mode-line :around #'yx/modeline-window-selected-advice)

;;;; Miscellaneous

(defvar-local yx-modeline-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")


;;;; Risky local variables

(dolist (construct '(yx-modeline-kbd-macro
                     yx-modeline-narrow
                     yx-modeline-input-method
                     yx-modeline-buffer-status
                     yx-modeline-window-dedicated-status
                     yx-modeline-buffer-identification
                     yx-modeline-major-mode
                     yx-modeline-process
                     yx-modeline-vc-branch
                     yx-modeline-flymake
                     yx-modeline-eglot
                     yx-modeline-misc-info))
  (put construct 'risky-local-variable t))



(provide 'modeline-x)
;;; modeline-x.el ends here
