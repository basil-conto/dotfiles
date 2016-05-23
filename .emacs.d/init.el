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
  (defvar use-package-verbose t)
  (require 'use-package))
(require 'bind-key)

;;; ===========
;;; Definitions
;;; ===========

(defun refresh-buffer ()
  "Reconcile the current buffer with what lives in the real world (the disk).
Offer to revert from the auto-save file, if it exists."
  (interactive)
  (revert-buffer nil t))

(defun open--line (forward)
  "Move forward `forward' - 1 lines before opening an empty line."
  (save-excursion
    (end-of-line forward)
    (open-line 1)))

(defun open-previous-line ()
  "Open empty line above point without affecting the current line."
  (interactive)
  (open--line 0))

(defun open-next-line ()
  "Open empty line below point without affecting the current line."
  (interactive)
  (open--line 1))

(defun what-face (pos)
  "Describe face at current point."
  (interactive "d")
  (let ((face (or (get-char-property (point) #'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun fix-trailing-enter (enter-key)
  "Advise given enter key function to first delete trailing whitespace."
  (advice-add enter-key :before #'(lambda () (delete-trailing-whitespace
                                              (line-beginning-position)
                                              (line-end-position)))))

(defun fix-electric-indent ()
  (electric-indent-local-mode 0))

(defun iwb ()
  "Indent Whole Buffer and delete trailing whitespace."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun transpose-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is split only in two. Adapted from Wilfred's
function at https://www.emacswiki.org/emacs/ToggleWindowSplit."
  (interactive)
  (unless (= (length (window-list)) 2)
    (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window)
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically))
    (switch-to-buffer nil)))

(defconst all-hooks
  '(haskell-cabal-mode-hook
        gitconfig-mode-hook
           prolog-mode-hook
           csharp-mode-hook
             text-mode-hook
             prog-mode-hook
             conf-mode-hook
              ess-mode-hook
              js3-mode-hook)
  "Individual hooks to hang from for global effect.")

;;; ========
;;; Bindings
;;; ========

(bind-keys
 ;; Line
 ("C-x C-p"     . open-previous-line)
 ("C-x C-n"     . open-next-line    )
 ;; Window / Buffer
 ("<f5>"        . refresh-buffer    )
 ("C-x 4"       . transpose-split   )
 ("S-<prior>"   . previous-buffer   )
 ("S-<next>"    .     next-buffer   )
 ;; Mutatis mutandis within tmux
 ("M-[ 5 ; 2 ~" . previous-buffer   )
 ("M-[ 6 ; 2 ~" .     next-buffer   )
 ;; Movement
 ("M-P"         . (lambda () (interactive) (scroll-down 8)))
 ("M-p"         . (lambda () (interactive) (scroll-down 4)))
 ("M-n"         . (lambda () (interactive) (scroll-up   4)))
 ("M-N"         . (lambda () (interactive) (scroll-up   8))))

;;; ========
;;; Settings
;;; ========

;; General

(setq inhibit-startup-screen t)

(when window-system
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono 8")))

(put   #'upcase-region 'disabled nil)
(put #'downcase-region 'disabled nil)

(defalias #'yes-or-no-p #'y-or-n-p)

;; Scrolling

(setq
 scroll-conservatively           most-positive-fixnum
 scroll-error-top-bottom         t
 scroll-preserve-screen-position t
 scroll-step                     1
 scroll-margin                   1)

;; Spacing

(setq-default
 tab-width        2
 indent-tabs-mode nil)

(add-hook 'text-mode-hook #'(lambda () (setq fill-column 80
                                             sentence-end-double-space nil)))

;;; ========
;;; Packages
;;; ========

(use-package 2048-game
  :ensure t)

(use-package ag
  :ensure t
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

(use-package bindings
  :bind (("M-[ 1 ; 5 C" . right-word)
         ("M-[ 1 ; 5 D" .  left-word)))

(use-package bytecomp
  :preface
  (defconst init-file-src (concat user-emacs-directory "init.el"))
  :bind (("C-c c" . byte-compile-file)
         ("C-c f" . byte-recompile-file)
         ("C-c d" . byte-recompile-directory))
  :config
  (add-hook 'kill-emacs-hook
            #'(lambda () (byte-recompile-file init-file-src nil 0))))

(use-package cc-mode
  :config
  (setq c-default-style "linux"
        c-basic-offset  2)
  (font-lock-add-keywords 'c++-mode '(("constexpr" . font-lock-keyword-face)
                                      ("nullptr"   . font-lock-keyword-face)))
  (add-hook 'c-mode-common-hook #'(lambda () (setq comment-start "//"
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

(use-package color-moccur
  :ensure t)

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
            #'(lambda () (local-set-key (kbd "{") #'c-electric-brace))))

(use-package crontab-mode
  :ensure t
  :mode ("\\.cron\\(tab\\)?\\'" "cron\\(tab\\)?\\."))

(use-package custom
  :config
  (load-theme 'tango-dark))

(use-package dafny-mode
  :ensure boogie-friends
  :config
  (add-hook 'dafny-mode-hook #'(lambda ()
                                 (fix-electric-indent)
                                 (prettify-symbols-mode 0)
                                 (flycheck-mode 0))))

(use-package dash
  :ensure t)

(use-package delsel
  :config
  (delete-selection-mode))

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
    (add-hook hook #'fic-mode)))

(use-package files
  :config
  (setq
   kept-old-versions    2
   kept-new-versions    4
   delete-old-versions  t
   version-control      t               ; Versioned backups
   backup-by-copying    t               ; Don't clobber symlinks
   backup-directory-alist               ; Backup directory
   '(("." . "~/backup/"))))

(use-package find-file
  :config
  (add-hook
   'find-file-hook
   #'(lambda ()
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

(use-package idris-mode
  :ensure t)

(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-column 80
        fci-rule-color "dim grey")
  (mapc #'(lambda (hook) (add-hook hook #'fci-mode)) all-hooks))

(use-package flex-mode
  :no-require t
  :disabled t
  :load-path "lisp")

(use-package font-lock
  :config
  (setq font-lock-maximum-decoration 2))

(use-package frame
  :config
  (blink-cursor-mode 0))

(use-package git-commit
  :ensure t)

(use-package gitconfig-mode
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(use-package git-rebase
  :ensure magit
  :bind ("C-x g" . magit-status)
  :config
  (add-hook 'git-rebase-mode-hook #'hl-line-mode))

(use-package golden-ratio-scroll-screen
  :ensure t
  :bind (("M-," . golden-ratio-scroll-screen-down)
         ("M-." . golden-ratio-scroll-screen-up)))

(use-package haskell-cabal
  :ensure haskell-mode
  :config
  (add-hook 'haskell-cabal-mode-hook #'fix-electric-indent))

(use-package haskell-mode
  :ensure t)

(use-package hayoo
  :ensure t)

(use-package helm
  :ensure t
  :defines helm-M-x-fuzzy-match
  :bind (("M-x"   . helm-M-x         )
         ("C-c b" . helm-buffers-list))
  :config
  (setq helm-M-x-fuzzy-match t))

(use-package html-mode
  :mode "\\.mustache$")

(use-package indent
  :bind ("C-c i" . indent-relative)
  :functions insert-tab
  :config
  (setq-default tab-always-indent t)
  (setq indent-line-function #'insert-tab))

(use-package jit-lock
  :config
  (setq jit-lock-stealth-time 4))

(use-package js
  :config
  (setq js-enabled-frameworks   '(javascript prototype dojo)
        js-indent-level         4
        js-switch-indent-offset 4))

(use-package js3-mode
  :ensure t
  :config
  (unbind-key "C-c C-g" js3-mode-map)   ; Why...
  (fix-trailing-enter #'js3-enter-key)

  (setq-default
   js3-auto-indent-p                         t
   js3-enter-indents-newline                 t
   js3-indent-dots                           t
   js3-indent-level                          4
   js3-indent-on-enter-key                   t
   js3-consistent-level-indent-inner-bracket t)

  (setq
   js3-include-browser-externs      nil
   js3-include-gears-externs        nil
   js3-include-rhino-externs        nil
   js3-skip-preprocessor-directives t)

  (setq js3-global-externs
        (mapcar 'symbol-name
                '(window document location console define require)))

  (set-face-attribute 'js3-function-param-face    nil :foreground "white")
  (set-face-attribute 'js3-external-variable-face nil :foreground "brightred")
  (set-face-attribute 'js3-error-face             nil :foreground "brightred"))

(use-package json-mode
  :ensure t)

(use-package lisp-mode
  :config
  (add-hook 'lisp-mode-hook #'fix-electric-indent))

(use-package list-unicode-display
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (set-face-attribute 'magit-blame-heading nil
                      :background "dim grey"
                      :foreground "white"))

(use-package magit-gh-pulls
  :ensure t
  :config
  (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(use-package markdown-mode
  :ensure t
  :functions markdown-cycle markdown-enter-key
  :mode ("\\.md$" "\\.markdown$")
  :config
  (add-hook 'markdown-mode-hook
            #'(lambda () (local-set-key (kbd "TAB") #'markdown-cycle)))
  (fix-trailing-enter #'markdown-enter-key))

(use-package minibuffer
  :preface
  (defconst gc-orig-thresh gc-cons-threshold
    "http://bling.github.io/blog/\
2016/01/18/why-are-you-changing-gc-cons-threshold/")
  :config
  (add-hook 'minibuffer-setup-hook
            #'(lambda () (setq gc-cons-threshold most-positive-fixnum)))
  (add-hook 'minibuffer-exit-hook
            #'(lambda () (setq gc-cons-threshold gc-orig-thresh))))

(use-package minimap
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

(use-package mwheel
  :config
  ;; One line at a time
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))))

(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode)
  (set-face-attribute 'linum nil :foreground "dim grey")
  (add-hook 'nlinum-mode-hook
            #'(lambda ()
                (when nlinum-mode
                  (setq nlinum--width
                        (length (number-to-string
                                 (count-lines (point-min) (point-max)))))
                  (nlinum--flush)))))

(use-package org-mode
  :preface
  (defvar org-file "~/.notes")
  (defun toggle-org ()
    (interactive)
    (if (not (eq major-mode 'org-mode))
        (find-file-other-window org-file)
      (basic-save-buffer)
      (delete-window)))
  :defines org-special-ctrl-a/e
  :bind (("C-c l" . org-store-link)
         ("<f12>" . toggle-org))
  :init
  (setq org-special-ctrl-a/e 'reversed))

(use-package paren
  :config
  (show-paren-mode))

(use-package pascal
  :no-require t
  :disabled t
  :config
  (add-hook 'pascal-mode-hook #'(lambda () (setq comment-start "//"
                                                 comment-end   ""))))

(use-package perl-mode
  :mode "\\.latexmkrc$")

(use-package prolog
  :mode ("\\.pl$" . prolog-mode)
  :config
  (setq prolog-system 'swi))

(use-package rx
  :preface
  (defun rx-to-string-bold (form)
    "Interactively wrap `rx-to-string` and remove shy groups around result."
    (interactive "sRegExp: ")
    (message "String: \"%s\"" (rx-to-string form t)))
  :bind ("C-c r" . rx-to-string-bold))

(use-package server
  :config
  (setq server-kill-new-buffers nil))

(use-package sh-script
  :config
  (setq sh-basic-offset 2
        sh-indentation  2))

(use-package simple
  :demand
  :bind (("C-x C-k" . kill-whole-line)
         ("M-\\"    .   cycle-spacing))
  :config
  (column-number-mode))

(use-package speedbar
  :config
  (setq speedbar-show-unknown-files 1
        speedbar-update-flag        nil
        speedbar-use-images         1
        speedbar-vc-do-check        nil))

(use-package sr-speedbar
  :ensure t
  :functions (sr-speedbar-window-exist-p
              sr-speedbar-remember-window-width
              sr-speedbar-window-dedicated-only-one-p
              sr-speedbar-window-p)
  :bind ("C-x t" . sr-speedbar-toggle)
  :config
  (setq sr-speedbar-auto-refresh nil))

(use-package tex
  :ensure auctex
  :config
  (setq TeX-PDF-mode t)
  (defun latexmk-pvc ()
    (interactive)
    (shell-command "latexmk -pvc &")))

(use-package tool-bar
  :config
  (when window-system
    (tool-bar-mode 0)))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package visual-regexp-steroids
  :ensure visual-regexp
  :ensure t
  :config
  (setq-default vr/match-separator-use-custom-face t))

(use-package vlf
  :ensure t)

(use-package wc-mode
  :ensure t
  :config
  (mapc #'(lambda (hook) (add-hook hook #'wc-mode)) all-hooks))

(use-package whitespace
  :config
  (global-whitespace-mode)
  (setq whitespace-style
        '(face tabs trailing empty tab-mark)))

(use-package windmove
  :demand
  :bind (("S-<up>"      . windmove-up   )
         ("S-<down>"    . windmove-down )
         ("S-<left>"    . windmove-left )
         ("S-<right>"   . windmove-right)
         ;; Mutatis mutandis within tmux
         ("M-[ 1 ; 2 A" . windmove-up   )
         ("M-[ 1 ; 2 B" . windmove-down )
         ("M-[ 1 ; 2 D" . windmove-left )
         ("M-[ 1 ; 2 C" . windmove-right)))

(use-package wrap-region
  :ensure t)

(use-package wttrin
  :ensure t
  :config
  (setq wttrin-default-cities
        '(athens-greece
          avoca-ireland
          dublin-ireland
          tel-aviv-israel
          harare-zimbabwe
          moon)))

(use-package xt-mouse
  :config
  (xterm-mouse-mode))
