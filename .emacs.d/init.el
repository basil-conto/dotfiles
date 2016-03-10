;;; =============
;;; Bootstrapping
;;; =============

;;; MELPA
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;; use-package
(unless (package-installed-p 'use-package)
  (when (y-or-n-p "use-package not installed, would you like to install it?")
    (package-refresh-contents)
    (package-install 'use-package)))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;;; ============
;;; Key bindings
;;; ============

(defun transpose-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is split only in two."
  (interactive)
  (unless (= (length (window-list)) 2)
    (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window)
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically))
    (switch-to-buffer nil)))

(defun refresh-buffer ()
  "Reconcile the current buffer with what lives in the real world (the disk).
Offer to revert from the auto-save file, if it exists."
  (interactive)
  (revert-buffer nil t))

(defun kill-whole-paragraph ()
    "Similar to `kill-paragraph`, but kills from the start of the paragraph
instead of the current point, i.e. the region defined by `mark-paragraph`."
  (interactive)
  (mark-paragraph)
  (kill-region (region-beginning) (region-end) t))

(bind-keys
 ;; Murder
 ("C-x C-k"     . kill-line)
 ("C-k"         . kill-whole-line)
 ("M-k"         . kill-whole-paragraph)
 ;; Windows
 ("C-x 4"       . transpose-split)
 ;; Buffers
 ("<f5>"        .  refresh-buffer)
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
 scroll-conservatively           most-positive-fixnum
 scroll-error-top-bottom         t
 scroll-preserve-screen-position t
 scroll-step                     1
 scroll-margin                   1)

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

(when window-system
  (tool-bar-mode 0))

(put   #'upcase-region 'disabled nil)
(put #'downcase-region 'disabled nil)

(defun what-face (pos)
  "Describe face at current point."
  (interactive "d")
  (let ((face (or (get-char-property (point) #'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(add-hook 'text-mode-hook (lambda () (setq fill-column 80
                                           sentence-end-double-space nil)))

(defconst all-hooks
  '(haskell-cabal-mode-hook
        gitconfig-mode-hook
           prolog-mode-hook
           csharp-mode-hook
            todoo-mode-hook
             text-mode-hook
             prog-mode-hook
             conf-mode-hook
              ess-mode-hook
              js3-mode-hook)
  "Individual hooks to hang from for global effect.")

;;; ===========
;;; Indentation
;;; ===========

(setq-default
 indent-tabs-mode  nil
 tab-always-indent t
 tab-width         2)

(setq indent-line-function #'insert-tab)

(defun fix-electric-indent ()
  (electric-indent-local-mode 0))

(defun iwb ()
  "Indent Whole Buffer and delete trailing whitespace."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

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

(use-package 2048-game
  :ensure t)

(use-package ag
  :config
  (setq ag-highlight-search t)
  (add-to-list 'ag-arguments "-C 5"))

(use-package annoying-arrows-mode
  :ensure t
  :config
  (global-annoying-arrows-mode))

(use-package apt-sources
  :config
  (add-hook 'apt-sources-mode-hook #'fix-electric-indent))

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
  (font-lock-add-keywords 'c++-mode '(("constexpr" . font-lock-keyword-face)
                                      ("nullptr"   . font-lock-keyword-face)))
  (add-hook 'c-mode-common-hook (lambda () (setq comment-start "//"
                                                 comment-end   "")))

  (cl-loop
   for (k . v)
   in '((     access-label . / )
        (       case-label . + )
        (      innamespace . 0 )
        (      inline-open . 0 )
        (    arglist-close . 0 )
        ;; ;; Doesn't distinguish between function declarations and calls
        ;; (    arglist-intro . ++)
        (      inher-intro . ++)
        (member-init-intro . ++))
   do (c-set-offset k v))

  ;; Verbatim from http://stackoverflow.com/a/23553882
  (defadvice c-lineup-arglist (around my activate)
    "Improve indentation of continued C++11 lambda function opened as argument."
    (setq ad-return-value
          (if (and (equal major-mode 'c++-mode)
                   (ignore-errors
                     (save-excursion
                       (goto-char (c-langelem-pos langelem))
                       ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                       ;;   and with unclosed brace.
                       (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
              0                         ; no additional indent
            ad-do-it))))                ; default behavior

(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

(use-package conf-mode
  :config
  (add-hook 'conf-mode-hook #'fix-electric-indent))

(use-package csharp-mode
  :ensure t
  :mode "\\.cs$"
  :config
  (add-hook 'csharp-mode-hook
            (lambda () (local-set-key (kbd "{") #'c-electric-brace))))

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

(use-package dash
  :ensure t)

(use-package ess
  :ensure t
  :config
  (setq-default ess-default-style 'DEFAULT)
  (setq ess-indent-from-lhs nil))

(when (get-buffer "*ESS*")
  (kill-buffer "*ESS*"))

(use-package expand-region
  :ensure t
  :bind ("M-+" . er/expand-region))

(use-package fic-mode
  :ensure t
  :config
  (dolist (hook '(LaTeX-mode-hook
                   prog-mode-hook
                    js3-mode-hook))
    (add-hook hook #'turn-on-fic-mode)))

(use-package find-file
  :config
  (add-hook
   'find-file-hook
   (lambda ()
     (if (> (buffer-size) (* 1024 1024))
         ;; Strip down emacs
         (progn (setq buffer-read-only t)
                (buffer-disable-undo)
                (fundamental-mode))
       ;; Detect conflicts
       (save-excursion
         (goto-char (point-min))
         (when (re-search-forward "^<<<<<<< " nil t)
           (message "Merge conflict detected. Enabling smerge-mode.")
           (smerge-mode 1)))))))

(use-package ido
  :config
  (ido-mode)
  (setq ido-enable-flex-matching 1))

(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-column 80
        fci-rule-color "DimGrey")
  (mapc (lambda (hook) (add-hook hook #'fci-mode)) all-hooks))

(use-package flex-mode
  :no-require t
  :disabled t
  :load-path "lisp")

(use-package git-commit
  :ensure t)

(use-package gitconfig-mode
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(use-package git-rebase
  :ensure magit
  :config
  (add-hook 'git-rebase-mode-hook #'hl-line-mode))

(use-package golden-ratio-scroll-screen
  :ensure t
  :bind (("M-," . golden-ratio-scroll-screen-down)
         ("M-." . golden-ratio-scroll-screen-up)))

(use-package haskell-cabal
  :config
  (add-hook 'haskell-cabal-mode-hook #'fix-electric-indent))

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
   js3-global-externs (mapcar #'symbol-name '(console define require))
   js3-include-browser-externs      nil
   js3-include-gears-externs        nil
   js3-include-rhino-externs        nil
   js3-skip-preprocessor-directives t)
  (set-face-attribute 'js3-function-param-face    nil :foreground "white")
  (set-face-attribute 'js3-external-variable-face nil :foreground "brightred")
  (set-face-attribute 'js3-error-face             nil :foreground "brightred"))

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
  (add-hook 'lisp-mode-hook #'fix-electric-indent))

(use-package list-unicode-display
  :ensure t)

(use-package magit
  :ensure t
  :config
  (set-face-attribute 'magit-blame-heading nil
                      :background "brightblack"
                      :foreground "white"))

(use-package markdown-mode
  :ensure t
  :functions markdown-cycle
  :mode ("\\.md$" "\\.markdown$")
  :config
  (add-hook 'markdown-mode-hook
            (lambda () (local-set-key (kbd "TAB") #'markdown-cycle))))

(use-package minibuffer
  :preface
  (defconst gc-orig-thresh gc-cons-threshold
    "http://bling.github.io/blog/\
2016/01/18/why-are-you-changing-gc-cons-threshold/")
  :config
  (add-hook 'minibuffer-setup-hook
            (lambda () (setq gc-cons-threshold most-positive-fixnum)))
  (add-hook 'minibuffer-exit-hook
            (lambda () (setq gc-cons-threshold gc-orig-thresh))))

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
  :functions todoo-save-and-exit
  :mode ("TODO"  .  todoo-mode )
  :bind ("<f12>" . toggle-todoo)
  :config
  (defun toggle-todoo ()
    (interactive)
    (if (eq major-mode 'todoo-mode)
        (call-interactively #'todoo-save-and-exit)
      (call-interactively #'todoo)))
  (add-hook 'todoo-mode-hook #'fix-electric-indent)
  (setq todoo-indent-column 2))

(use-package vlf
  :ensure t)

(use-package wc-mode
  :ensure t
  :config
  (mapc (lambda (hook) (add-hook hook #'wc-mode)) all-hooks))

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
