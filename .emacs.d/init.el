;;; =============
;;; Bootstrapping
;;; =============

;;; MELPA
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;; use-package
(unless (package-installed-p 'use-package)
  (if (y-or-n-p "use-package not installed, would you like to install it?")
      (progn
        (package-refresh-contents)
        (package-install 'use-package))))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;;; ============
;;; Key bindings
;;; ============

;;; Transpose window split
(defun transpose-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2)
    (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window)
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically))
    (switch-to-buffer nil)))

(bind-keys
 ;; Murder
 ("C-k"         . kill-whole-line)
 ("C-x C-k"     .       kill-line)
 ;; Windows
 ("C-x 4"       . transpose-split)
 ;; Buffers
 ("S-<prior>"   . previous-buffer)
 ("S-<next>"    .     next-buffer)
 ;; Mutatis mutandis within tmux
 ("M-[ 5 ; 2 ~" . previous-buffer)
 ("M-[ 6 ; 2 ~" .     next-buffer)
 ("M-[ 1 ; 5 C" .      right-word)
 ("M-[ 1 ; 5 D" .       left-word)
 ;; Prop line file variables
 ("C-c a"       . add-file-local-variable-prop-line))

;;; =========
;;; Scrolling
;;; =========

(setq
 ;; One line at a time
 mouse-wheel-scroll-amount       '(1 ((shift) . 1))
 scroll-conservatively           10000
 scroll-error-top-bottom         t
 scroll-preserve-screen-position t
 scroll-step                     1)

;;; =======
;;; General
;;; =======

(load-theme 'tango-dark)

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono 9"))

(setq
 font-lock-maximum-decoration 2
 jit-lock-stealth-time        4
 inhibit-startup-screen       1
 uniquify-buffer-name-style   'forward)

(    blink-cursor-mode 0)
(   column-number-mode  )
(delete-selection-mode  )
(      show-paren-mode  )

(if window-system
    (tool-bar-mode 0))

(put   'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(add-hook 'text-mode-hook (lambda () (setq fill-column 80
                                           sentence-end-double-space nil)))

;;; ===========
;;; Indentation
;;; ===========

(setq-default
 indent-tabs-mode  nil
 tab-always-indent t
 tab-width         2)

(setq indent-line-function 'insert-tab)

(defun fix-electric-indent ()
  (electric-indent-local-mode 0))

;;; ======
;;; Backup
;;; ======

(setq
 ;; Don't clobber symlinks
 backup-by-copying      t
 ;; Backup directory
 backup-directory-alist '(("." . "~/.backup/"))
 delete-old-versions    t
 kept-new-versions      6
 kept-old-versions      2
 ;; Versioned backups
 version-control        t)

;;; ================
;;; Package settings
;;; ================

(use-package ag
  :config
  (setq ag-highlight-search t)
  (add-to-list 'ag-arguments "-C 5"))

(use-package apt-sources
  :config
  (add-hook 'apt-sources-mode-hook 'fix-electric-indent))

(use-package tex
  :ensure auctex
  :config
  (setq TeX-PDF-mode t)
  (defun latexmk-pvc ()
    (interactive)
    (shell-command "latexmk -pvc &")))

(use-package bytecomp
  :preface
  (defconst init-file-src (concat user-emacs-directory "init.el"))
  :bind (("C-c c" . byte-compile-file)
         ("C-c f" . byte-recompile-file)
         ("C-c d" . byte-recompile-directory))
  :config
  (add-hook 'kill-emacs-hook
            (lambda () (byte-recompile-file init-file-src nil 0))))

(use-package cc-mode
  :config
  (setq c-default-style "linux"
        c-basic-offset  2)
  (font-lock-add-keywords 'c++-mode
                          '(("constexpr" . font-lock-keyword-face)))
  (loop for (k . v) in '((     access-label . / )
                         (       case-label . + )
                         (      innamespace . 0 )
                         (      inline-open . 0 )
                         (    arglist-close . 0 )
                         (member-init-intro . ++))
        do (c-set-offset k v))
  (add-hook 'c-mode-common-hook (lambda () (setq comment-start "//"
                                                 comment-end   ""))))

(use-package conf-mode
  :config
  (add-hook 'conf-mode-hook 'fix-electric-indent))

(use-package csharp-mode
  :ensure t
  :mode "\\.cs$"
  :config
  (add-hook 'csharp-mode-hook
            (lambda () (local-set-key (kbd "{") 'c-electric-brace))))

(use-package crontab-mode
  :ensure t
  :mode ("\\.cron\\(tab\\)?\\'" "cron\\(tab\\)?\\."))

(use-package dafny-mode
  :ensure boogie-friends
  :config
  (add-hook 'dafny-mode-hook (lambda ()
                               (fix-electric-indent)
                               (prettify-symbols-mode 0)
                               (flycheck-mode 0))))

(use-package doc-view
  :no-require t
  :disabled t
  :config
  (setq doc-view-continuous t))

(use-package ess
  :no-require t
  :disabled t
  :config
  (setq-default ess-default-style 'DEFAULT)
  (setq ess-arg-function-offset nil))

(if ( get-buffer "*ESS*")
    (kill-buffer "*ESS*"))

(use-package fic-mode
  :ensure t
  :config
  (dolist (hook '( prog-mode-hook
                    js3-mode-hook
                  LaTeX-mode-hook))
    (add-hook hook 'turn-on-fic-mode)))

(use-package ido
  :config
  (ido-mode)
  (setq ido-enable-flex-matching 1))

(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-column 80
        fci-rule-color "DimGrey")
  (dolist (hook '(     text-mode-hook
                       prog-mode-hook
                     prolog-mode-hook
                     csharp-mode-hook
                        ess-mode-hook
                        js3-mode-hook
                      todoo-mode-hook
                  gitconfig-mode-hook))
    (add-hook hook 'fci-mode)))

(use-package flex-mode
  :no-require t
  :disabled t
  :load-path "lisp")

(use-package gitconfig-mode
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(use-package haskell-mode
  :config
  ;; Pretty lambda
  ;; (setq haskell-font-lock-symbols t)
  (add-hook 'haskell-mode-hook (lambda ()
                                 (turn-on-haskell-simple-indent)
                                 (fix-electric-indent))))

(use-package js
  :config
  (setq js-enabled-frameworks '(javascript prototype dojo)
        js-indent-level 2
        js-switch-indent-offset 2))

(use-package js2-mode
  :no-require t
  :disabled t)

(use-package js3-mode
  :ensure t
  :config
  (setq-default
   js3-auto-indent-p                         t
   js3-enter-indents-newline                 t
   js3-indent-dots                           t
   js3-indent-on-enter-key                   t
   js3-consistent-level-indent-inner-bracket t)
  (setq
   js3-global-externs (mapcar 'symbol-name '(console define require))
   js3-include-browser-externs      nil
   js3-include-gears-externs        nil
   js3-include-rhino-externs        nil
   js3-skip-preprocessor-directives t)
  (set-face-attribute 'js3-function-param-face    nil :foreground "white")
  (set-face-attribute 'js3-external-variable-face nil :foreground "red"))

(use-package json-mode
  :ensure t)

(use-package linum
  :config
  (global-linum-mode)
  ;; Right-aligned followed by vertical line
  (setq linum-format
        (lambda (line)
          (let ((w (length (number-to-string (count-lines (point-min)
                                                          (point-max))))))
            (propertize (format (format "%%%dd\u2502" w) line) 'face 'linum)))))

(use-package lisp-mode
  :config
  (add-hook 'lisp-mode-hook 'fix-electric-indent))

(use-package list-unicode-display
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("\\.md$" "\\.markdown$")
  :config
  (add-hook 'markdown-mode-hook
            (lambda () (local-set-key (kbd "TAB") 'markdown-cycle))))

(use-package minimap
  :no-require t
  :disabled t
  :ensure t
  :config
  (setq minimap-highlight-line  nil
        minimap-recenter-type   'relative
        minimap-width-fraction  0.05
        minimap-window-location 'right)
  (set-face-attribute 'minimap-active-region-background nil
    :background "dim grey")
  (set-face-attribute 'minimap-font-face nil
    :height 8
    :family "DejaVu Sans Mono"))

(use-package pascal
  :no-require t
  :disabled t
  :config
  (add-hook 'pascal-mode-hook (lambda () (setq comment-start "//"
                                               comment-end   ""))))

(use-package perl-mode
  :mode "\\latexmkrc$")

(use-package prolog
  :mode ("\\.pl$" . prolog-mode)
  :config
  (setq prolog-system 'swi))

(use-package server
  :config
  (setq server-kill-new-buffers nil))

(use-package sh-script
  :config
  (setq sh-basic-offset 2
        sh-indentation  2))

(use-package speedbar
  :config
  (setq speedbar-show-unknown-files 1
        speedbar-update-flag        nil
        speedbar-use-images         1
        speedbar-vc-do-check        nil))

(use-package sr-speedbar
  :ensure t
  :bind ("C-x t" . sr-speedbar-toggle)
  :config
  (setq sr-speedbar-auto-refresh nil))

(use-package todoo
  :mode ("TODO"  .  todoo-mode )
  :bind ("<f12>" . toggle-todoo)
  :config
  (defun toggle-todoo ()
    (interactive)
    (if (eq major-mode 'todoo-mode)
        (call-interactively 'todoo-save-and-exit)
      (call-interactively 'todoo)))
  (add-hook 'todoo-mode-hook 'fix-electric-indent)
  (setq todoo-indent-column 2))

(use-package vlf
  :no-require t
  :disabled t
  :ensure t)

(use-package whitespace
  :config
  (global-whitespace-mode)
  (setq whitespace-style
        '(face tabs trailing empty tab-mark)))

(use-package windmove
  :bind (("S-<up>"      . windmove-up   )
         ("S-<down>"    . windmove-down )
         ("S-<left>"    . windmove-left )
         ("S-<right>"   . windmove-right)
         ;; Mutatis mutandis within tmux
         ("M-[ 1 ; 2 A" . windmove-up   )
         ("M-[ 1 ; 2 B" . windmove-down )
         ("M-[ 1 ; 2 D" . windmove-left )
         ("M-[ 1 ; 2 C" . windmove-right)))
