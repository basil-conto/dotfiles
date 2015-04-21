(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f"
    "#729fcf""#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("132ccc75b7fdcd9f5979329620a1151953a8f65efad06b988deed7cba9338eab"
     default)))
 '(doc-view-continuous t)
 '(font-lock-maximum-decoration 2)
 '(ido-enable-flex-matching t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(jit-lock-stealth-time 16)
 '(minimap-recenter-type (quote relative))
 '(scroll-conservatively 10000)
 '(scroll-error-top-bottom t)
 '(scroll-preserve-screen-position t)
 '(scroll-step 1)
 '(show-paren-mode t)
 '(tab-always-indent t)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76)))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal
                :weight normal :height 90 :width normal))))
 '(minimap-active-region-background ((t (:background "dim gray"))))
 '(minimap-font-face ((t (:height 8 :family "DejaVu Sans Mono")))))

;;; ============================================================================
;;; Personal settings
;;; ============================================================================

;;; Start emacs server
;; (server-start)

;;; C-mode settings
;; Brace indentation
(setq c-default-style "linux" c-basic-offset 2)
;; Comment style
(add-hook 'c-mode-hook (lambda () (setq comment-start "//"
                                        comment-end   "")))

;;; Sh-mode settings
(setq sh-basic-offset 2
      sh-indentation 2)

;;; Commenting key bindings
(global-set-key (kbd "C-x /")   'comment-region)
(global-set-key (kbd "C-x C-/") 'uncomment-region)
(global-set-key (kbd "C-x C-_") 'uncomment-region)

;;; Mouse wheel scrolls one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;;; Overwrite active region when typing
(delete-selection-mode 1)

;;; Enable word-wrapping and edit by visual lines
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
(put 'upcase-region 'disabled nil)
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

;;; Use Printing package
;; (require 'printing)
;; (pr-update-menus)

;;; ============================================================================
;;; Packages
;;; ============================================================================

;;; Personal package file directory
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;; Interactively Do Things
(require 'ido)
(ido-mode t)

;;; FillColumnIndicator - Enable via M-x fci-mode
(require 'fill-column-indicator)
(setq fci-rule-column 80)
(setq fci-rule-color "DimGrey")
(add-hook   'text-mode-hook 'fci-mode)
(add-hook   'prog-mode-hook 'fci-mode)
(add-hook 'prolog-mode-hook 'fci-mode)
(add-hook 'csharp-mode-hook 'fci-mode)
(add-hook    'ess-mode-hook 'fci-mode)

;;; ColumnEnforceMode
;; (require 'column-enforce-mode)
;; (add-hook 'text-mode-hook 'column-enforce-mode)
;; (add-hook 'prog-mode-hook 'column-enforce-mode)
;; (add-hook 'prolog-mode-hook 'column-enforce-mode)
;; (add-hook 'csharp-mode-hook 'column-enforce-mode)

;;; ColumnMarker
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

;; ESS
(add-hook 'ess-mode-hook (lambda () (setq ess-arg-function-offset nil)))

;;; CSharpMode
(require 'csharp-mode)
;; Bind opening brace to c-electric-brace rather than csharp-insert-open-brace
(add-hook 'csharp-mode-hook
          (lambda ()
            (local-set-key (kbd "{") 'c-electric-brace)))
(setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;;; HaskellMode
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; Pretty lambda
;; (setq haskell-font-lock-symbols t)

;;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;; Minimap
(require 'minimap)

;;; Prolog
(autoload 'run-prolog   "prolog" "Start a Prolog sub-process."              t)
(autoload 'prolog-mode  "prolog" "Major mode for editing Prolog programs."  t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                               auto-mode-alist))

;;; ToDooMode
;; (autoload 'todoo "todoo" "TODO Mode" t)
;; (add-to-list 'auto-mode-alist '("TODO$" . todoo-mode))

;;; VeryLargeFiles
;; (require 'vlf)

;;; Markdown Mode
(setq auto-mode-alist (append '(("\\.md$" . markdown-mode)
                                ("\\.markdown$" . markdown-mode))
                              auto-mode-alist))
