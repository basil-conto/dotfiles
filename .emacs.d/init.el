;;; ====
;;; TODO
;;; ====

;; * Improve git-commit/magit/with-editor logic segregation + hooks
;; * Separate config data from logic, particularly w.r.t. sensitive data
;; * :bind local :maps once their autoloading is fixed upstream
;; * `setq' -> `setq-default'
;; * Numeric arguments -> nil/t
;; * Optimise use-package
;; * Magit over ssh
;; * Fix c++-mode memer-init-intro indentation

;; * Explore:
;;   - Org
;;   - Magit
;;   - Helm
;;   - ERC/ZNC
;;   - Company
;;   - http://www.emacswiki.org/emacs/ThreeWindows
;;   - Themes
;;     * base16-chalk-dark
;;     * base16-default-dark
;;     * zenburn

;;; =============
;;; Bootstrapping
;;; =============

;;; Profiling
(defconst emacs-start-time (current-time)
  "Time before loading user initialisation file.")

;;; MELPA
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(advice-add #'package--save-selected-packages :override #'ignore) ; Sandbox
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

(defun fix-trailing-enter (enter-key)
  "Advise given enter key function to first delete trailing whitespace."
  (advice-add
   enter-key
   :after
   #'(lambda ()
       "Delete trailing whitespace prior to newline insertion."
       (save-excursion
         (forward-line -1)
         (delete-trailing-whitespace
          (line-beginning-position)
          (line-end-position))))))

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

(defun set-face-foregrounds (foregrounds)
  "Apply `set-face-foreground' to a list of argument lists."
  (dolist (foreground foregrounds)
    (apply #'set-face-foreground foreground)))

(defconst all-hooks
  '(haskell-cabal-mode-hook
        gitconfig-mode-hook
         mustache-mode-hook
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
 ("C-c i"       . indent-relative)
 ;; Window / Buffer
 ("C-x 4"       . transpose-split)
 ("S-<prior>"   . previous-buffer)
 ("S-<next>"    .     next-buffer)
 ;; Mutatis mutandis within tmux
 ("M-[ 5 ; 2 ~" . previous-buffer)
 ("M-[ 6 ; 2 ~" .     next-buffer)
 ;; Movement
 ("M-{"         . (lambda () (interactive) (scroll-down 6)))
 ("M-}"         . (lambda () (interactive) (scroll-up   6))))

;;; ========
;;; Settings
;;; ========

;; General

(setq inhibit-startup-screen  t
      split-window-keep-point nil)

(advice-add
 #'split-window-sensibly :around
 #'(lambda (old-split &rest args)
     "Ensure the larger window dimension is sacrificed when splitting."
     (let ((split-width-threshold       (window-height))
           (split-height-threshold (lsh (window-width) -1))) ; Adjust slightly
       (apply old-split args))))

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
 tab-width         2
 tab-always-indent t
 indent-tabs-mode  nil)

(setq indent-line-function #'insert-tab)

(add-hook 'text-mode-hook #'(lambda () (setq fill-column 80
                                             sentence-end-double-space nil)))

;;; ========
;;; Packages
;;; ========

(use-package 2048-game
  :ensure t
  :defer)

(use-package ac-js2
  :ensure t
  :disabled)

(use-package ag
  :ensure t
  :defer
  :config
  (setq ag-highlight-search t)
  (add-to-list 'ag-arguments "-C 5"))

(use-package align
  :bind ("C-c p" . align-punctuation)
  :config
  (defun align-punctuation ()
    "Align punctuation as defined by the current mode in the current region."
    (interactive)
    (unless (use-region-p)
      (mark-paragraph))
    (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)\\s.")))

(use-package annoying-arrows-mode
  :disabled
  :ensure t
  :config
  (global-annoying-arrows-mode))

(use-package apt-sources
  :defer
  :config
  (add-hook 'apt-sources-mode-hook #'fix-electric-indent))

(use-package base16-theme
  :ensure t
  :disabled)

(use-package bison-mode
  :ensure t
  :defer)

(use-package bongo
  :ensure t
  :defer)

(use-package bytecomp
  :bind (("C-c c" . byte-compile-file)
         ("C-c f" . byte-recompile-file)
         ("C-c d" . byte-recompile-directory)))

(use-package cc-mode
  :defer
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

(use-package company
  :ensure t
  :bind ("M-#" . company-manual-begin)
  :config
  (setq-default company-idle-delay nil)
  (global-company-mode))

(use-package conf-mode
  :defer
  :config
  (add-hook 'conf-mode-hook #'fix-electric-indent))

(use-package crontab-mode
  :ensure t
  :mode ("\\.cron\\(tab\\)?\\'" "cron\\(tab\\)?\\."))

(use-package csharp-mode
  :ensure t
  :defer)

(use-package cssh
  :ensure t
  :defer)

(use-package csv-mode
  :ensure t
  :commands csv-align-fields
  :config
  (setq-default csv-align-style 'auto)
  (add-hook 'csv-mode-hook
            #'(lambda () (csv-align-fields nil (point-min) (point-max)))))

(use-package custom
  :config
  (load-theme 'tango-dark))

(use-package dafny-mode
  :ensure boogie-friends
  :defer
  :config
  (add-hook 'dafny-mode-hook
            #'(lambda ()
                (fix-electric-indent)
                (prettify-symbols-mode 0)
                (flycheck-mode 0))))

(use-package dash
  :ensure t
  :defer)

(use-package delsel
  :config
  (delete-selection-mode))

(use-package disaster
  :ensure t
  :defer
  :init
  (add-hook 'c-mode-common-hook
            #'(lambda ()
                (bind-key "C-c d" #'disaster c-mode-base-map)))
  :config
  (setq-default disaster-objdump "objdump -D -M att -Sl --no-show-raw-insn"))

(use-package engine-mode
  :ensure t
  :config
  (engine-mode)
  (defengine google-def
    "https://encrypted.google.com/search?ie=utf-8&oe=utf-8&q=define+%s"
    :keybinding "d")
  (defengine google-enc
    "https://encrypted.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")
  (defengine hoogle
    "https://www.haskell.org/hoogle/?hoogle=%s"
    :keybinding "h")
  (defengine mdn
    "https://developer.mozilla.org/en-US/search?q=%s"
    :keybinding "m")
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")
  (defengine wikipedia
    "https://en.wikipedia.org/w/index.php?search=%s"
    :keybinding "w"))

(use-package eshell
  :defer
  :config
  (add-hook 'eshell-mode-hook #'(lambda () (nlinum-mode 0))))

(use-package ess
  :ensure t
  :defer
  :preface
  (when (get-buffer "*ESS*")
    (kill-buffer "*ESS*"))
  :config
  (setq-default ess-default-style 'DEFAULT)
  (setq ess-indent-from-lhs nil))

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq-default exec-path-from-shell-variables
                '("PATH" "MANPATH" "SSH_AGENT_PID" "SSH_AUTH_SOCK")))

(use-package expand-region
  :ensure t
  :bind ("M-+" . er/expand-region))

(use-package eyebrowse
  :ensure t
  :defer)

(use-package faces
  :commands what-face
  :config
  (defun what-face (pos)
    "Describe face at current point."
    (interactive "d")
    (let ((face (or (get-char-property (point) #'read-face-name)
                    (get-char-property (point) 'face))))
      (if face (message "Face: %s" face) (message "No face at %d" pos)))))

(use-package fic-mode
  :ensure t
  :config
  (setq-default fic-highlighted-words '("FIXME" "TODO" "BUG" "KLUDGE" "HACK"))
  (mapc #'(lambda (hook) (add-hook hook #'fic-mode))
        '(LaTeX-mode-hook prog-mode-hook js3-mode-hook)))

(use-package files
  :bind ("<f5>" . refresh-buffer)
  :init
  (defun refresh-buffer ()
    "Reconcile the current buffer with what lives in the real world (the disk).
Offer to revert from the auto-save file, if it exists."
    (interactive)
    (revert-buffer nil t))
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
       (if (< (buffer-size) (* 1024 1024))
           ;; Snoop around for conflicts
           (save-excursion
             (goto-char (point-min))
             (when (re-search-forward "^<<<<<<< " nil t)
               (message "Merge conflict detected. Enabling `smerge-mode'.")
               (smerge-mode)))
         ;; Strip down emacs
         (setq buffer-read-only t)
         (buffer-disable-undo)
         (fundamental-mode)
         (nlinum-mode 0)))))

(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-column 80
        fci-rule-color "#696969")
  (mapc #'(lambda (hook) (add-hook hook #'fci-mode)) all-hooks))

(use-package flex-mode
  :load-path "lisp"
  :mode "\\.lex\\'")

(use-package font-lock
  :config
  (setq font-lock-maximum-decoration t))

(use-package frame
  :config
  (blink-cursor-mode 0))

(use-package git-commit
  :ensure t
  ;; Need to load package to know when to load package :(
  :mode ("/\\(\
\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\
\\|BRANCH_DESCRIPTION\\)\\'" . git-commit-mode)
  :config
  (global-git-commit-mode)

  (add-hook
   'with-editor-post-finish-hook
   #'(lambda ()
       "Ensure message buffer is killed post-git-commit."
       (and git-commit-mode
            buffer-file-name
            (string-match-p git-commit-filename-regexp buffer-file-name)
            (kill-buffer)))))

(use-package gitconfig-mode
  :ensure t
  :defer)

(use-package gitignore-mode
  :ensure t
  :defer)

(use-package git-rebase
  :defer
  :config
  (add-hook 'git-rebase-mode-hook #'hl-line-mode)
  (set-face-foreground 'git-rebase-hash "#808080"))

(use-package golden-ratio-scroll-screen
  :ensure t
  :bind (("M-," . golden-ratio-scroll-screen-down)
         ("M-." . golden-ratio-scroll-screen-up)))

(use-package haskell-cabal
  :ensure haskell-mode
  :defer
  :config
  (add-hook 'haskell-cabal-mode-hook #'fix-electric-indent))

(use-package haskell-mode
  :ensure t
  :defer)

(use-package hayoo
  :ensure t
  :defer)

(use-package helm
  :ensure t
  :bind (("C-x C-f" . helm-find-files    )
         ("M-x"     . helm-M-x           )
         ("C-c b"   . helm-mini          )
         ("M-y"     . helm-show-kill-ring))
  :config
  (setq-default helm-buffers-fuzzy-matching t
                helm-M-x-fuzzy-match        t
                helm-split-window-in-side-p t)
  (helm-mode))

(use-package helm-proc
  :ensure t
  :defer)

(use-package highlight-escape-sequences
  :ensure t
  :config
  (hes-mode))

(use-package i18next-wrap
  :load-path "lisp"
  :bind ("C-c C-i" . i18next-query-replace))

(use-package ido
  :defer
  :config
  (setq ido-enable-flex-matching 1))

(use-package idris-mode
  :ensure t
  :defer)

(use-package isearch-prop
  :ensure t
  :defer)

(use-package jade
  :disabled
  :ensure t
  :commands jade-interaction-mode
  :init
  (add-hook 'js2-mode-hook #'jade-interaction-mode))

(use-package jit-lock
  :config
  (setq jit-lock-stealth-time 4))

(use-package js
  :defer
  :config
  (setq js-enabled-frameworks   '(javascript prototype dojo)
        js-indent-level         4
        js-switch-indent-offset 4))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter ("node" "nodejs")
  :functions js2-line-break
  :init
  (setq-default js2-bounce-indent-p t)
  :config
  (bind-key "RET" #'js2-line-break js2-mode-map)

  (setq-default
   js2-allow-rhino-new-expr-initializer nil
   js2-mode-assume-strict               t
   js2-concat-multiline-strings         'eol
   js2-global-externs                   '("location" "define")
   js2-highlight-level                  3
   js2-include-node-externs             t
   js2-skip-preprocessor-directives     t)

  (add-hook 'js2-mode-hook
            #'(lambda ()
                (fix-electric-indent)
                (js2-highlight-unused-variables-mode)))

  (set-face-foregrounds
   '((js2-error             "#ff0000")
     (js2-external-variable "#ff0000")
     (js2-function-param    "#5fd7af")))

  (defun js2-moar-colour ()
    "Further customise `js2-mode' faces."
    (interactive)
    (set-face-foregrounds
     '((js2-function-call   "#fce94f")
       (js2-object-property "#fcaf3e")))))

(use-package js3-mode
  :ensure t
  :defer
  :commands js3-enter-key
  :config
  (unbind-key "C-c C-g" js3-mode-map)   ; Why...
  (fix-trailing-enter #'js3-enter-key)  ; For comments

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
        (mapcar #'symbol-name
                '(window document location console define require)))

  (set-face-foregrounds
   '((js3-function-param-face    "#ffffff")
     (js3-external-variable-face "#ff0000")
     (js3-error-face             "#ff0000"))))

(use-package json-mode
  :ensure t
  :defer)

(use-package ledger-mode
  :ensure t
  :mode "\\.ledger\\'"
  :config
  (setq-default ledger-use-iso-dates t))

(use-package lisp-mode
  :config
  (add-hook 'lisp-mode-hook #'fix-electric-indent))

(use-package list-unicode-display
  :ensure t
  :defer)

(use-package lorem-ipsum
  :ensure t
  :defer)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (global-magit-file-mode)

  (magit-wip-after-apply-mode)
  (magit-wip-after-save-mode)
  (magit-wip-before-change-mode)

  (add-hook 'magit-mode-hook #'(lambda () (nlinum-mode 0)))

  (setq-default
   magit-log-arguments    '("-n32" "--graph" "--decorate")
   magit-rebase-arguments '("--interactive"))

  ;; Align refs with wider columns
  (let* ((case-fold-search nil)
         (ctrl             '(?n ?U))
         (re               (concat "%\\(?:[+-]??[[:digit:]]*?\\)??"
                                   "\\([" (apply #'string ctrl) "]\\)"))
         (width            "-40")
         (subst            (concat "%" width "\\1")))
    (mapc #'(lambda (fmt)
              (set-default fmt (replace-regexp-in-string
                                re subst (symbol-value fmt) t)))
          '(magit-refs-local-branch-format
            magit-refs-remote-branch-format
            magit-refs-symref-format
            magit-refs-tags-format)))

  (set-face-attribute
   'magit-blame-heading nil
   :background "#696969"
   :foreground "#ffffff")
  (set-face-attribute
   'magit-header-line nil
   :inherit    'magit-section-heading
   :background (internal-get-lisp-face-attribute 'default :background))
  (set-face-foregrounds
   '((magit-dimmed "#808080")
     (magit-hash   "#808080"))))

(use-package magit-gh-pulls
  :ensure t
  :disabled                             ; gh.el doesn't speak ssh?
  :defer
  :commands turn-on-magit-gh-pulls
  :init
  (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(use-package make-mode
  :defer
  :config
  (setq-default makefile-macro-assign " := "))

(use-package markdown-mode
  :ensure t
  :commands markdown-cycle markdown-enter-key
  :mode ("\\.md\\'" "\\.markdown\\'")
  :config
  (bind-key "TAB" #'markdown-cycle markdown-mode-map)
  (fix-trailing-enter #'markdown-enter-key))

(use-package minibuffer
  :config
  (defconst gc-orig-thresh gc-cons-threshold
    "http://bling.github.io/blog/\
2016/01/18/why-are-you-changing-gc-cons-threshold/")
  (add-hook 'minibuffer-setup-hook
            #'(lambda () (setq gc-cons-threshold most-positive-fixnum)))
  (add-hook 'minibuffer-exit-hook
            #'(lambda () (setq gc-cons-threshold gc-orig-thresh))))

(use-package minimap
  :ensure t
  :defer
  :config
  (setq minimap-highlight-line  nil
        minimap-recenter-type   'relative
        minimap-width-fraction  0.05
        minimap-window-location 'right)
  (set-face-attribute 'minimap-active-region-background nil
                      :background "#696969")
  (set-face-attribute 'minimap-font-face nil
                      :height 8
                      :family "DejaVu Sans Mono"))

(use-package mustache-mode
  :ensure t
  :defer)

(use-package mwheel
  :config
  ;; One line at a time
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))))

(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode)
  (set-face-foreground 'linum "#696969")
  (add-hook 'nlinum-mode-hook
            #'(lambda ()
                (when nlinum-mode
                  (setq nlinum--width
                        (length (number-to-string
                                 (count-lines (point-min) (point-max)))))
                  (nlinum--flush)))))

(use-package nodejs-repl
  :ensure t
  :defer)

(use-package org-mode
  :bind ("C-c l" . org-store-link)
  :init
  (setq org-special-ctrl-a/e 'reversed))

(use-package palette
  :ensure t
  :defer
  :when window-system)

(use-package paren
  :config
  (show-paren-mode))

(use-package paren-face
  :ensure t
  :config
  (global-paren-face-mode))

(use-package pascal
  :no-require t
  :disabled t
  :config
  (add-hook 'pascal-mode-hook #'(lambda () (setq comment-start "//"
                                                 comment-end   ""))))

(use-package pcre2el
  :ensure t
  :defer)

(use-package perl-mode
  :mode "\\.latexmkrc\\'")

(use-package perspective
  :ensure t
  :defer)

(use-package prolog
  :mode ("\\.pl\\'" . prolog-mode)
  :config
  (setq prolog-system 'swi))

(use-package remember
  :bind (("<f7>" . remember-notes)
         ("<f8>" . remember-notes-save-and-bury-buffer))
  :config
  (setq-default remember-notes-initial-major-mode 'org-mode))

(use-package rx
  :bind ("C-c r" . rx-to-string-bold)
  :config
  (defun rx-to-string-bold (form)
    "Interactively wrap `rx-to-string` and remove shy groups around result."
    (interactive "sRegExp: ")
    (message "String: \"%s\"" (rx-to-string form t))))

(use-package sass-mode
  :ensure t
  :defer
  :config
  (add-hook 'sass-mode-hook #'(lambda () (setq comment-start "//"))))

(use-package saveplace
  :config
  (save-place-mode))

(use-package server
  :config
  (setq server-kill-new-buffers nil))

(use-package sh-script
  :defer
  :config
  (setq-default sh-basic-offset 2
                sh-indentation  2))

(use-package simple
  :demand
  :bind (("M-\\"    . cycle-spacing     )
         ("C-x C-k" . kill-whole-line   )
         ("C-x C-p" . open-previous-line)
         ("C-x C-n" . open-next-line    ))
  :config
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

  (column-number-mode))

(use-package sl
  :ensure t
  :defer)

(use-package speedbar
  :defer
  :config
  (setq-default speedbar-show-unknown-files t
                speedbar-update-flag        nil
                speedbar-use-images         t
                speedbar-vc-do-check        nil))

(use-package sr-speedbar
  :ensure t
  :bind ("C-x t" . sr-speedbar-toggle)
  :after speedbar
  :config
  (setq-default sr-speedbar-auto-refresh nil))

(use-package subword
  :config
  (global-subword-mode))

(use-package tex
  :ensure auctex
  :defer
  :config
  (setq-default TeX-PDF-mode t)
  (defun latexmk-pvc ()
    (interactive)
    (shell-command "latexmk -pvc &")))

(use-package tool-bar
  :when (display-graphic-p)
  :config
  (tool-bar-mode 0))

(use-package top-mode
  :ensure t
  :defer)

(use-package uniquify
  :config
  (setq-default uniquify-buffer-name-style 'forward))

(use-package vc-hooks
  ;; FIXME: VC needed for xref-js2 as long as projectile not used
  :disabled
  :defer
  :config
  (setq-default vc-handled-backends nil))

(use-package visual-regexp-steroids
  :ensure visual-regexp
  :ensure pcre2el
  :ensure t
  :after pcre2el
  :defer
  :config
  (setq-default vr/match-separator-use-custom-face t))

(use-package vlf
  :ensure t
  :disabled)

(use-package wc-mode
  :ensure t
  :config
  (setq-default wc-modeline-format "[%tll]")
  (mapc #'(lambda (hook) (add-hook hook #'wc-mode)) all-hooks))

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.mustache\\'"))

(use-package whitespace
  :config
  (global-whitespace-mode)
  (setq-default whitespace-style
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

(use-package winner
  :config
  (winner-mode))

(use-package wrap-region
  :ensure t
  :config
  (wrap-region-global-mode)
  (setq-default
   wrap-region-only-with-negative-prefix t
   wrap-region-tag-active-modes          '(html-mode web-mode mustache-mode))
  (wrap-region-add-wrapper "{{#i18n}}" "{{/i18n}}" "i"))

(use-package wttrin
  :ensure t
  :defer
  :config
  (setq-default wttrin-default-cities
                '(athens-greece
                  avoca-ireland
                  dublin-ireland
                  tel-aviv-israel
                  harare-zimbabwe
                  moon)))

(use-package xref-js2
  :ensure t
  :defer
  :init
  (add-hook 'js2-mode-hook
            #'(lambda ()
                (unbind-key "M-." js2-mode-map)
                (add-hook 'xref-backend-functions
                          #'xref-js2-xref-backend nil t))))

(use-package xt-mouse
  :unless window-system
  :config
  (xterm-mouse-mode))

(use-package yaml-mode
  :ensure t
  :defer)

(use-package zenburn-theme
  :ensure t
  :disabled)

(add-hook 'after-init-hook
          `(lambda ()
             "https://github.com/jwiegley/dot-emacs"
             (let ((elapsed (float-time (time-subtract (current-time)
                                                       emacs-start-time))))
               (message "Loading %s...done (%.3fs)" ,load-file-name elapsed)))
          t)
