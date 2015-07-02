;;; ============================================================================
;;; General settings
;;; ============================================================================

;;; Personal package file directory
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;; Key bindings
;; Comments
(global-set-key (kbd "C-x /")     'comment-region)
(global-set-key (kbd "C-x C-/") 'uncomment-region)
(global-set-key (kbd "C-x C-_") 'uncomment-region)
;; Prop line file variables
(global-set-key (kbd "C-x C-a") 'add-file-local-variable-prop-line)

;;; Transpose window split
(defun transpose-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2)
    (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives split with the other window twice
    (switch-to-buffer nil))) ; restore original window in this part of the frame
(global-set-key (kbd "C-x 4") 'transpose-split)

;;; Mouse wheel scrolls one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;;; Overwrite active region when typing
(delete-selection-mode 1)

;; ;;; Enable word-wrapping and edit by visual lines
;; (global-visual-line-mode)

;;; Display line numbers alongside buffer
(global-linum-mode)
;; Line number format - right-aligned followed by vertical line
(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (propertize (format (format "%%%dd\u2502" w) line) 'face 'linum)))
(setq linum-format 'linum-format-func)

;;; Indentation settings
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;;; Enable upcase & downcase regions
(put   'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; File backup settings
(setq
 backup-by-copying t        ; don't clobber symlinks
 backup-directory-alist
  '(("." . "~/.backup/"))   ; backup directory
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)         ; versioned backups

;;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; ;;; Printing
;; (require 'printing)
;; (pr-update-menus)

;;; ============================================================================
;;; Mode settings
;;; ============================================================================

;;; ------
;;; c-mode
;;; ------

;; Brace indentation
(setq c-default-style "linux" c-basic-offset 2)
;; Comment style
(add-hook 'c-mode-hook (lambda () (setq comment-start "//"
                                        comment-end   "")))

;;; -------------------
;;; column-enforce-mode
;;; -------------------

;; (require 'column-enforce-mode)
;; (add-hook   'text-mode-hook 'column-enforce-mode)
;; (add-hook   'prog-mode-hook 'column-enforce-mode)
;; (add-hook 'prolog-mode-hook 'column-enforce-mode)
;; (add-hook 'csharp-mode-hook 'column-enforce-mode)

;;; -------------
;;; column-marker
;;; -------------

;; (require 'column-marker)
;; ;; Define red-background, white-foreground fill column face
;; (defface column-marker-4 '((t (:background "red" :foreground "white")))
;;   "Face used for the fill column marker." :group 'faces)
;; (defvar column-marker-4-face 'column-marker-4
;;                              "Face used for the fill column marker.")
;; (column-marker-create column-marker-4 column-marker-4-face)
;; ;; Use fill column face
;; (add-hook   'text-mode-hook (lambda () (column-marker-4 80)))
;; (add-hook   'prog-mode-hook (lambda () (column-marker-4 80)))
;; (add-hook 'prolog-mode-hook (lambda () (column-marker-4 80)))
;; (add-hook 'csharp-mode-hook (lambda () (column-marker-4 80)))

;;; -----------
;;; csharp-mode
;;; -----------

;; (require 'csharp-mode)
;; ;; Bind opening brace to c-electric-brace rather than csharp-insert-open-brace
;; (add-hook 'csharp-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "{") 'c-electric-brace)))
;; (setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;;; ------------
;;; ESS (R-mode)
;;; ------------

;; (add-hook 'ess-mode-hook (lambda () (setq ess-arg-function-offset nil)))
(if ( get-buffer "*ESS*")
    (kill-buffer "*ESS*"))

;;; ---------------------
;;; fill-column-indicator
;;; ---------------------

(require 'fill-column-indicator)
(setq fci-rule-column 80)
(setq fci-rule-color "DimGrey")
(add-hook   'text-mode-hook 'fci-mode)
(add-hook   'prog-mode-hook 'fci-mode)
(add-hook 'prolog-mode-hook 'fci-mode)
(add-hook 'csharp-mode-hook 'fci-mode)
(add-hook    'ess-mode-hook 'fci-mode)
;; ;; previous-line workaround
;; (make-variable-buffer-local 'line-move-visual)
;; (defadvice previous-line (around avoid-jumpy-fci activate)
;;   (if (and (symbol-value 'fci-mode) (> (count-lines 1 (point)) 0))
;;       (prog (fci-mode -1) ad-do-it (fci-mode 1))
;;     ad-do-it))

;;; ---------
;;; flex-mode
;;; ---------

;; (require 'flex-mode)

;;; ------------
;;; haskell-mode
;;; ------------

(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
;; Pretty lambda
;; (setq haskell-font-lock-symbols t)

;;; --------
;;; ido-mode
;;; --------

(require 'ido)
(ido-mode t)

;;; --------
;;; js2-mode
;;; --------

;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;; -------------
;;; markdown-mode
;;; -------------

(setq auto-mode-alist (append '(("\\.md$" . markdown-mode)
                                ("\\.markdown$" . markdown-mode))
                              auto-mode-alist))
(add-hook 'markdown-mode-hook
          (lambda () (local-set-key (kbd "TAB") 'markdown-cycle)))

;;; ------------
;;; minimap-mode
;;; ------------

;; (require 'minimap)

;;; -----------
;;; prolog-mode
;;; -----------

(autoload 'run-prolog   "prolog" "Start a Prolog sub-process."              t)
(autoload 'prolog-mode  "prolog" "Major mode for editing Prolog programs."  t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                              auto-mode-alist))

;;; --------------
;;; requirejs-mode
;;; --------------

;; (require 'requirejs-mode)
;; (add-hook 'javascript-mode-hook (lambda () (requirejs-mode)))

;;; -------
;;; sh-mode
;;; -------

(setq sh-basic-offset 2
      sh-indentation 2)

;;; -----------
;;; sr-speedbar
;;; -----------

(require 'sr-speedbar)
(global-set-key (kbd "C-x t") 'sr-speedbar-toggle)

;;; ----------
;;; todoo-mode
;;; ----------

(autoload 'todoo "todoo" "TODO Mode" t)
(add-to-list 'auto-mode-alist '("TODO" . todoo-mode))
(add-hook 'todoo-mode-hook (lambda () (electric-indent-local-mode -1)))
(defun toggle-todoo ()
  (interactive)
  (if (eq major-mode 'todoo-mode)
      (call-interactively 'todoo-save-and-exit)
    (call-interactively 'todoo)))
(global-set-key (kbd "<f12>") 'toggle-todoo)
