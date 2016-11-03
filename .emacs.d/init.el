;;; ====
;;; TODO
;;; ====

;; Fix
;; * Displace colour codes with relative faces
;; * Break `defun's from existing packages out of `use-package' declarations?
;;   - Benefit: byte compilation
;;   - Alternative: force compilation?
;; * Hard-coded load paths
;; * Write setter creator macro
;; * Issue `add-hooks' errors at compile-time
;; * Order of custom/frame/theme loading
;; * Improve autoloading of pdf-tools
;; * Look into use-package :require
;; * Add current project to `ebib-bib-search-dirs'
;; * Disable nlinum in `*Messages*', `*Help*', etc. or by default?
;; * Moar `defun's, not `lambda's
;; * Tidy hooks
;; * Modify custom option lists in place instead of redefining them
;; * Window splitting - add minimum
;;   or customise `magit-display-buffer-function' use-case
;; * `with-graphical-frame' -> macro
;; * Diminish/delight
;; * Improve git-commit/magit/with-editor logic segregation + hooks
;; * Separate config data from logic, particularly w.r.t. sensitive data
;; * :bind local :maps once their autoloading is fixed upstream
;; * Make apt use of `setq'/`setq-default'/`setq-local'
;; * Numeric arguments -> `nil'/t
;; * Optimise use-package
;; * GitHub pulls over SSH
;; * Fix `c++-mode' `memer-init-intro' indentation

;; Explore
;; * Ivy
;; * Macros
;; * Org
;; * Magit
;; * Helm
;; * ERC/ZNC
;; * Company
;; * URL `http://www.emacswiki.org/emacs/ThreeWindows'
;; * Themes
;;   - `base16-chalk-dark'
;;   - `base16-default-dark'
;;   - `zenburn'

;;; =============
;;; Bootstrapping
;;; =============

;;; Profiling
(let ((start-time (current-time)))
  (eval
   `(defun report-init-time ()
      "See URL `https://github.com/jwiegley/dot-emacs'."
      (let ((elapsed (float-time (time-subtract (current-time) ',start-time))))
        (message "Loading %s...done (%.3fs)" ,load-file-name elapsed)))))

(add-hook 'after-init-hook #'report-init-time t)

;;; MELPA
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(advice-add #'package--save-selected-packages :override #'ignore) ; Sandbox
(package-initialize)

;;; `use-package'
(unless (package-installed-p 'use-package)
  (when (y-or-n-p "use-package not installed; would you like to install it?")
    (package-refresh-contents)
    (package-install 'use-package)))

(setq-default use-package-verbose 'debug)
(require 'use-package)

;; Moar subroutines
(require 'subr-x)

;;; ===========
;;; Definitions
;;; ===========

(defvar small-scroll-step 6
  "Number of lines constituting a small scroll.")

(defvar fundamental-hooks
  '(haskell-cabal-mode-hook
         mustache-mode-hook
          hledger-mode-hook
             conf-mode-hook
             prog-mode-hook
             text-mode-hook
              ess-mode-hook
              js3-mode-hook)
  "Hooks whose modes derive from `fundamental-mode' or nothing.")

(defface man-header
  '((t . (:inherit font-lock-keyword-face :weight bold)))
  "Man page heading face."
  :group 'man)

(defface man-emph
  '((t . (:inherit font-lock-string-face :underline t)))
  "Man page emphasis/underline face."
  :group 'man)

(defun debug-use-package ()
  "Enable `use-package' debugging."
  (interactive)
  (setq-default use-package-debug t))

;; TODO: define no-arg macro?
(defun no-trailing-enter--advice (&rest _)
  "Delete trailing whitespace prior to newline insertion."
  (save-excursion
    (forward-line -1)
    (delete-trailing-whitespace
     (line-beginning-position)
     (line-end-position))))

(defun no-trailing-enter (enter-key)
  "Advise enter key function to first delete trailing whitespace."
  (advice-add enter-key :after #'no-trailing-enter--advice))

(defun turn-off-electric-indent (&rest _)
  "Locally disable electric indentation."
  (electric-indent-local-mode 0))

(defun turn-off-line-numbers (&rest _)
  "Locally disable display of line numbers."
  (nlinum-mode 0))

(defun turn-off-prettify-symbols-mode (&rest _)
  "Disable `prettify-symbols-mode'."
  (prettify-symbols-mode 0))

(defun turn-off-flycheck-mode (&rest _)
  "Disable `flycheck-mode'."
  (flycheck-mode 0))

(defun large-buffer-p ()
  "Determine whether buffer classifies as being large.
Return `t' if buffer size falls under
`large-file-warning-threshold', else `nil'."
  (> (buffer-size) large-file-warning-threshold))

(defun strip-down-buffer ()
  "Try to make the current buffer as responsive as possible."
  (interactive)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (fundamental-mode)
  (turn-off-line-numbers)
  (font-lock-mode 0))

(defun strip-down-large-buffer ()
  "Call `strip-down-buffer' if current buffer is large."
  (when (large-buffer-p)
    (strip-down-buffer)))

(defun iwb ()
  "Indent Whole Buffer and delete trailing whitespace.
See URL `http://emacsblog.org/2007/01/17/indent-whole-buffer/'."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun transpose-split ()
  "Alternate between vertical and horizontal frame split.
Assumes that the frame is split only in two. Adapted from Wilfred's
function at URL `https://www.emacswiki.org/emacs/ToggleWindowSplit'."
  (interactive)
  (unless (= (length (window-list)) 2)
    (error "Can only toggle a frame split in two"))
  (let ((split (if (window-combined-p)
                   #'split-window-horizontally
                 #'split-window-vertically)))
    (delete-window)
    (funcall split)
    (switch-to-buffer nil)))

(defun use-c++-comments ()
  "Default to single-line C++-style comments."
  (setq-local comment-start "//")
  (setq-local comment-end     ""))

(defun split-larger-dimension--advice (old-split &rest args)
  "Ensure the larger window dimension is sacrificed when splitting."
  (let ((split-width-threshold       (window-height))
        (split-height-threshold (lsh (window-width) -1))) ; Adjust slightly
    (apply old-split args)))

;; TODO: rewrite as macro?
(defun with-graphical-frame (fun)
  "Run abnormal hook now, in current frame, and with every new frame."
  (let ((frame (selected-frame)))
    (when (display-graphic-p frame)
      (funcall fun frame)))
  (add-hook 'after-make-frame-functions
            `(lambda (frame)
               (when (display-graphic-p frame)
                 (funcall ,fun frame)))))

(defun unpack (fun)
  "Return a function which packs its arguments into `fun'."
  (apply-partially #'apply fun))

(defun mapc-unpack (fun &rest args)
  "Apply function `fun' to a sequence of packed arguments."
  (apply #'mapc (unpack fun) args))

(defun add-to-lists (lists)
  "Apply `add-to-list' to a list of argument lists."
  (mapc-unpack #'add-to-list lists))

;; FIXME: transform '(foo bar) -> `(,#'foo ,#'bar)
(defun add-hooks-1 (hook &rest functions)
  "Add multiple FUNCTIONS to the value of HOOK."
  (mapc (apply-partially #'add-hook hook) functions))

;; FIXME: macros?
(defun add-hooks-n (hooks)
  "Apply `add-hook-1' to a list of argument lists."
  (mapc-unpack #'add-hooks-1 hooks))

;; FIXME: thread-first/last?
(defun add-hooks-t (func &rest hooks)
  "Add FUNC to the value of HOOKS."
  (mapc #'(lambda (hook) (add-hook hook func)) hooks))

(defun set-foregrounds (foregrounds)
  "Apply `set-face-foreground' to a list of argument lists."
  (mapc-unpack #'set-face-foreground foregrounds))

(defun remap-faces (map)
  "Remap given face keys to values."
  (mapc-unpack #'face-remap-add-relative map))

(defun remap-man-faces (&rest _)
  "Customise `Man-mode' faces."
  (remap-faces '((Man-overstrike man-header)
                 (Man-underline  man-emph  ))))

(defun remap-woman-faces (&rest _)
  "Customise `woman-mode' faces."
  (remap-faces '((woman-bold   man-header)
                 (woman-italic man-emph  ))))

(defconst emacs-25+ (>= emacs-major-version 25)
  "Whether the current major version number of Emacs is 25 or higher.")
(defun small-scroll-up ()
  "Scroll up `small-scroll-step' lines."
  (interactive)
  (scroll-up small-scroll-step))

(defun small-scroll-down ()
  "Scroll down `small-scroll-step' lines."
  (interactive)
  (scroll-down small-scroll-step))

;;; ========
;;; Bindings
;;; ========

(bind-keys
 ;; Line
 ("C-c i"       .   indent-relative)
 ;; Window / Buffer
 ("C-x 7"       .   transpose-split)
 ("S-<prior>"   .   previous-buffer)
 ("S-<next>"    .       next-buffer)
 ;; Mutatis mutandis within tmux
 ("M-[ 5 ; 2 ~" .   previous-buffer)
 ("M-[ 6 ; 2 ~" .       next-buffer)
 ;; Movement
 ("M-{"         . small-scroll-down)
 ("M-}"         . small-scroll-up  ))

;;; ========
;;; Settings
;;; ========

;; General

(setq-default
 inhibit-startup-screen  t
 split-window-keep-point nil)

(advice-add #'split-window-sensibly :around #'split-larger-dimension--advice)

(with-graphical-frame
 #'(lambda (frame)
     (set-face-attribute 'default frame :family "DejaVu Sans Mono" :height 80)))

(put   #'upcase-region 'disabled nil)
(put #'downcase-region 'disabled nil)

(defalias #'yes-or-no-p #'y-or-n-p)

;; Scrolling

(setq-default
 isearch-allow-scroll            t
 scroll-conservatively           most-positive-fixnum
 scroll-error-top-bottom         t
 scroll-margin                   1
 scroll-preserve-screen-position t
 scroll-step                     1)

;; Spacing

(setq-default
 fill-column               80
 indent-line-function      #'insert-tab
 indent-tabs-mode          nil
 tab-always-indent         t
 tab-width                 2
 sentence-end-double-space nil)

;;; ========
;;; Packages
;;; ========

(use-package 2048-game
  :ensure t
  :defer)

(use-package ac-js2
  :disabled
  :ensure t)

(use-package ace-window
  :ensure t
  :bind ("C-c o" . ace-window))

(use-package ag
  :ensure t
  :defer
  :config
  (setq-default ag-highlight-search t)
  (add-to-list 'ag-arguments "-C 5"))

(use-package align
  :bind ("C-c p" . align-punctuation)
  :init
  (defun align-punctuation ()
    "Horizontally align mode-specific punctuation in region."
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
  ;; FIXME
  :load-path "/usr/share/emacs24/site-lisp/debian-el"
  :mode ("\\.sources\\'" . apt-sources-mode)
  :init
  (add-hook 'apt-sources-mode-hook #'turn-off-electric-indent))

(use-package asm-mode
  :defer
  :config
  (setq-default asm-comment-char ?#))

(use-package auctex-latexmk
  :ensure t
  :defer)

(use-package auth-source
  :defer
  :config
  (setq-default auth-source-debug t)

  ;; Add SMTPS port 465
  (when-let ((port  "465")
             (proto 'smtp)
             (names (assq proto auth-source-protocols)))
    (setf (cdr names) (add-to-list 'names port t))))

(use-package base16-theme
  :ensure t
  :disabled)

(use-package better-shell
  :ensure t
  :defer)

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
  :functions c-lineup-arglist c++-lambda-indent
  :init
  (add-hook 'c-mode-common-hook #'use-c++-comments)
  :config
  (let ((name    "blc")
        (base    "linux")
        (default (assq 'other c-default-style))
        (offsets '((     access-label . / )
                   (       case-label . + )
                   (      innamespace . 0 )
                   (      inline-open . 0 )
                   (    arglist-close . 0 )
                   ;; ;; Doesn't distinguish between
                   ;; ;; function declarations and calls
                   ;; (    arglist-intro . ++)
                   (      inher-intro . ++)
                   (member-init-intro . ++))))

    (c-add-style name `(,base
                        (c-basic-offset  . 2)
                        (c-offsets-alist . ,offsets)))

    (setf (cdr default) name))

  (defun c++-lambda-indent (langelem)
    "Return indentation offset for C++11 lambda function arguments.
Currently keeps offset unchanged by returning 0 for lambda functions
opened as arguments and `nil' for everything else.
Adapted from URL `http://stackoverflow.com/a/23553882'."
    (and (eq major-mode 'c++-mode)
         (ignore-errors
           (save-excursion
             (goto-char (c-langelem-pos langelem))
             ;; Detect "[...](" or "[...]{",
             ;; preceded by "," or "(" and with unclosed brace
             (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$")))
         0))

  (advice-add #'c-lineup-arglist :before-until #'c++-lambda-indent))

(use-package color-moccur
  :disabled
  :ensure t)

(use-package color-theme-sanityinc-solarized
  :disabled
  :ensure t)

(use-package color-theme-solarized
  :disabled
  :ensure t
  :init
  (setq-default solarized-italic     nil
                solarized-termcolors 256)
  (load-theme 'solarized t))

(use-package comint
  :defer
  :init
  (add-hook 'comint-mode-hook #'turn-off-line-numbers))

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
  :init
  (add-hook 'conf-mode-hook #'turn-off-electric-indent))

(use-package crontab-mode
  :ensure t
  :mode "\\.cron\\(?:tab\\)??\\'" "cron\\(?:tab\\)??\\.")

(use-package csharp-mode
  :ensure t
  :defer)

(use-package cssh
  :ensure t
  :defer)

(use-package csv-mode
  :ensure t
  :commands csv-align-fields
  :init
  (defun align-all-csv-fields ()
    "Align all fields in the current CSV buffer."
    (csv-align-fields nil (point-min) (point-max)))
  (add-hook 'csv-mode-hook #'align-all-csv-fields)
  :config
  (setq-default csv-align-style 'auto))

(use-package dafny-mode
  :ensure boogie-friends
  :defer
  :init
  (add-hooks-1 'dafny-mode-hook
               #'turn-off-electric-indent
               #'turn-off-flycheck-mode
               #'turn-off-prettify-symbols-mode))

(use-package dash
  :ensure t
  :defer)

(use-package delsel
  :defer
  :init
  (delete-selection-mode))

(use-package dired
  :defer
  :init
  (setq-default
   dired-listing-switches "--group-directories-first -AFhl"))

(use-package dired-x
  :defer
  :config
  (mapc #'(lambda (cell) (push cell dired-guess-shell-alist-user))
        '(("\\.pdf\\'"   "pdf"     )
          ("\\.docx?\\'" "lowriter"))))

(use-package disaster
  :ensure t
  :commands disaster
  :init
  (defun enable-disaster ()
    "Enable `disaster' in `c-mode' derivatives."
    (bind-key "C-c d" #'disaster c-mode-base-map))
  (add-hook 'c-mode-common-hook #'enable-disaster)
  :config
  (setq-default disaster-objdump "objdump -D -M att -Sl --no-show-raw-insn"))

(use-package ebib
  :ensure t
  :bind ("C-c e" . ebib)
  :config
  (setq-default
   ebib-bibtex-dialect 'biblatex
   ebib-use-timestamp  t))

(use-package engine-mode
  :ensure t
  :commands engine-mode
  :functions engine/execute-search engine/get-query
  :bind-keymap ("C-x /" . engine-mode-map)
  :config
  (defengine google-def
    "https://encrypted.google.com/search?ie=utf-8&oe=utf-8&q=define+%s"
    :keybinding "d")
  (defengine google-enc
    "https://encrypted.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")
  (defengine hoogle
    "https://www.haskell.org/hoogle/?hoogle=%s"
    :keybinding "h")
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")
  (defengine wikipedia
    "https://en.wikipedia.org/w/index.php?search=%s"
    :keybinding "w")
  (engine-mode))

(use-package eshell
  :defer
  :init
  (add-hook 'eshell-mode-hook #'turn-off-line-numbers))

(use-package ess
  :ensure t
  :defer
  :preface
  (when (get-buffer "*ESS*")
    (kill-buffer "*ESS*"))
  :config
  (setq-default ess-default-style   'DEFAULT
                ess-indent-from-lhs nil))

(use-package exec-path-from-shell
  :ensure t
  :config
  (dolist (var '("SSH_AGENT_PID" "SSH_AUTH_SOCK"))
    (add-to-list 'exec-path-from-shell-variables var)))

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
    (if-let ((face (or (get-char-property (point) #'read-face-name)
                       (get-char-property (point) 'face))))
      (message "Face: %s" face)
      (message "No face at %d" pos))))

(use-package fic-mode
  :ensure t
  :defer
  :init
  (apply #'add-hooks-t #'fic-mode fundamental-hooks)
  :config
  (dolist (word '("KLUDGE" "HACK"))
    (add-to-list 'fic-highlighted-words word)))

(use-package figlet
  :ensure t
  :defer)

(use-package files
  :defer
  :init
  (defun refresh-buffer ()
    "Reconcile current buffer with what lives on the disk.
Offer to revert from the auto-save file, if that exists."
    (interactive)
    (revert-buffer nil t))

  (bind-key "<f5>" #'refresh-buffer)

  (defun switch-to-temp-file (&optional prefix suffix)
    "Create and switch to a temporary file.
A non-empty filename PREFIX can help identify the file or its purpose,
whereas a non-empty SUFFIX will help determine the relevant major-mode."
    (interactive "sfile prefix: \nsfile extension: ")
    (let ((pre (or prefix ""))
          (suf (concat (unless (zerop (length suffix))
                         ".")           ; Prefix non-empty suffix with full-stop
                       suffix)))
      (find-file (make-temp-file pre nil suf))))

  (defconst auto-save-backup-dir "~/.backup/"
    "Directory for auto-save and backup files.")

  (setq-default
   directory-free-space-args "-hP")

  (setq
   kept-old-versions          2
   kept-new-versions          4
   delete-old-versions        t
   ;; Versioned backups
   version-control            t
   ;; Do not clobber symlinks
   backup-by-copying          t
   ;; Do not silently append EOF NL
   mode-require-final-newline nil
   ;; Backup/auto-save directory
   backup-directory-alist     `(("." . ,auto-save-backup-dir))))

(use-package find-file
  :defer
  :init
  (add-hook 'find-file-hook #'strip-down-large-buffer))

(use-package fill-column-indicator
  :ensure t
  :defer
  :init
  (apply #'add-hooks-t #'turn-on-fci-mode fundamental-hooks)
  (setq-default fci-rule-color  "#696969"
                fci-rule-column 80))

(use-package flex-mode
  :load-path "lisp"
  :mode "\\.lex\\'")

(use-package font-lock
  :defer
  :init
  (setq-default font-lock-maximum-decoration t))

(use-package frame
  :when (display-graphic-p)
  :defer
  :init
  (blink-cursor-mode 0))

(use-package git-commit
  :ensure t
  ;; Need to load package to know when to load package :(
  :mode ("/\\(?:\
\\(?:\\(?:COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\
\\|BRANCH_DESCRIPTION\\)\\'" . git-commit-mode)
  :config
  (defun kill-git-commit-buffer ()
    "Ensure message buffer is killed post-git-commit."
    (and git-commit-mode
         buffer-file-name
         (string-match-p git-commit-filename-regexp buffer-file-name)
         (kill-buffer)))

  (add-hook 'with-editor-post-finish-hook #'kill-git-commit-buffer)

  (setq-default git-commit-summary-max-length 50
                git-commit-fill-column        68)

  (global-git-commit-mode))

(use-package gitconfig-mode
  :ensure t
  :defer)

(use-package gitignore-mode
  :ensure t
  :defer)

(use-package git-rebase
  :defer
  :config
  (set-face-foreground 'git-rebase-hash "#808080"))

(use-package gnus
  :defer
  :config
  (setq-default gnus-check-new-newsgroups nil))

(use-package golden-ratio-scroll-screen
  :disabled
  :ensure t
  :bind (("M-," . golden-ratio-scroll-screen-down)
         ("M-." . golden-ratio-scroll-screen-up)))

(use-package haskell-cabal
  :ensure haskell-mode
  :defer
  :init
  (add-hook 'haskell-cabal-mode-hook #'turn-off-electric-indent))

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
  :init
  (add-hook 'helm-major-mode-hook #'turn-off-line-numbers)
  :config
  (setq-default helm-buffers-fuzzy-matching t
                helm-M-x-fuzzy-match        t
                helm-split-window-in-side-p t)

  ;; FIXME: create toggling mechanism
  (when (bound-and-true-p helm-white-selection)
    (set-face-foreground 'helm-selection "#ffffff"))

  (helm-mode))

(use-package helm-pass
  :ensure t
  :defer)

(use-package helm-proc
  :ensure t
  :defer)

(use-package helm-projectile
  :ensure t
  :after helm projectile
  :defer
  :init
  (add-hook 'projectile-mode-hook #'helm-projectile-on))

(use-package highlight-escape-sequences
  :ensure t
  :defer
  :init
  (turn-on-hes-mode))

(use-package hl-line
  :defer
  :init
  (add-hook 'git-rebase-mode-hook #'hl-line-mode))

(use-package hledger-mode
  :ensure t
  :mode "\\.journal\\'"
  :config
  (setq-default
   hledger-currency-string "€"
   hledger-jfile           "~/.hledger.journal"
   hledger-ratios-essential-expense-accounts
   "expenses:housing expenses:groceries"
   hledger-ratios-liquid-asset-accounts
   "assets:boi assets:cash"))

(use-package i18next-wrap
  :load-path "lisp"
  :bind ("C-c C-i" . i18next-query-replace))

(use-package ido
  :defer
  :init
  (setq-default ido-enable-flex-matching 1))

(use-package idris-mode
  :ensure t
  :defer)

(use-package info
  :defer
  :init
  (add-hook 'Info-mode-hook #'turn-off-line-numbers))

(use-package isearch+
  :disabled
  :ensure t)

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
  :defer
  :init
  (setq-default
   jit-lock-stealth-load    60
   jit-lock-stealth-time     4
   jit-lock-stealth-verbose  t))

(use-package js
  :defer
  :config
  (setq-default
   js-enabled-frameworks   '(javascript prototype dojo)
   js-indent-level         4
   js-switch-indent-offset 4))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node" "nodejs"
  :commands js2-line-break
  :init
  (setq-default js2-bounce-indent-p t)

  (add-hooks-1 'js2-mode-hook
               #'js2-highlight-unused-variables-mode
               #'turn-off-electric-indent)
  :config
  (setq-default
   js2-allow-rhino-new-expr-initializer nil
   js2-concat-multiline-strings         'eol
   js2-global-externs                   '("location" "define")
   js2-highlight-level                  3
   js2-include-node-externs             t
   js2-mode-assume-strict               t
   js2-skip-preprocessor-directives     t)

  (bind-key "RET" #'js2-line-break js2-mode-map)

  (set-foregrounds
   '((js2-error             "#ff0000")
     (js2-external-variable "#ff0000")
     (js2-function-param    "#5fd7af")))

  (defun js2-moar-colour ()
    "Further customise `js2-mode' faces."
    (interactive)
    (set-foregrounds
     '((js2-function-call   "#fce94f")
       (js2-object-property "#fcaf3e")))))

(use-package js2-refactor
  :ensure t
  :defer
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package js3-mode
  :ensure t
  :commands js3-enter-key
  :config
  (unbind-key "C-c C-g" js3-mode-map)   ; Why...
  (no-trailing-enter #'js3-enter-key)   ; For comments

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

  (setq-default
   js3-global-externs
   '("window" "document" "location" "console" "define" "require"))

  (set-foregrounds
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
  :defer
  :init
  (add-hook 'lisp-mode-hook #'turn-off-electric-indent))

(use-package list-processes+
  :ensure t
  :defer)

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

  (setq-default
   ;; FIXME: search/replace original value instead of redefining
   ;; Define ref alignment below as a function and reuse here
   magit-log-arguments '("-n32" "--graph" "--decorate"))

  (add-to-list 'magit-rebase-arguments "--interactive")

  (let* ((repos-base "repos")
         (repos-file (expand-file-name repos-base user-emacs-directory))
         (repos-dir  (file-name-as-directory repos-file)))
    (setq-default magit-repository-directories `((,repos-dir . 2))))

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
            magit-refs-tags-format))))

  ;; FIXME: modify tango-dark
  ;; (set-face-attribute
  ;;  'magit-blame-heading nil
  ;;  :background "#696969"
  ;;  :foreground "#ffffff")
  ;; (set-face-attribute
  ;;  'magit-header-line nil
  ;;  :inherit    'magit-section-heading
  ;;  :background (internal-get-lisp-face-attribute 'default :background))
  ;; (set-foregrounds
  ;;  '((magit-dimmed "#808080")
  ;;    (magit-hash   "#808080"))))

(use-package magit-gh-pulls
  :disabled
  :ensure t                             ; gh.el doesn't speak ssh?
  :commands turn-on-magit-gh-pulls
  :init
  (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(use-package make-mode
  :defer
  :config
  (setq-default makefile-macro-assign " := "))

(use-package man
  :defer
  :init
  (add-hook 'Man-mode-hook #'remap-man-faces))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'" "\\.markdown\\'"
  :commands markdown-cycle markdown-enter-key
  :config
  (bind-key "TAB" #'markdown-cycle markdown-mode-map)
  (no-trailing-enter #'markdown-enter-key))

(use-package minibuffer
  :defer
  :init
  ;; See URL `http://bling.github.io/blog/2016/01/18/\
  ;; why-are-you-changing-gc-cons-threshold/'
  (add-hook 'minibuffer-setup-hook
            `(lambda () (setq gc-cons-threshold most-positive-fixnum)))
  (add-hook 'minibuffer-exit-hook
            `(lambda () (setq gc-cons-threshold ,gc-cons-threshold))))

(use-package minimap
  :ensure t
  :defer
  :config
  (setq-default
   minimap-highlight-line  nil
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
  :defer
  :init
  ;; One line at a time
  (setq-default mouse-wheel-scroll-amount '(1 ((shift) . 1))))

(use-package nlinum
  :ensure t
  :defer
  :init
  (global-nlinum-mode))

(use-package nodejs-repl
  :ensure t
  :defer)

(use-package org
  :bind ("C-c l" . org-store-link)
  :init
  (setq org-special-ctrl-a/e 'reversed))

;; FIXME: require when creating graphical frame from terminal
(use-package palette
  :ensure t
  :defer)

(use-package paren
  :defer
  :init
  (show-paren-mode))

(use-package paren-face
  :disabled
  :ensure t
  :defer
  :init
  (global-paren-face-mode))

(use-package pascal
  :defer
  :init
  (add-hook 'pascal-mode-hook #'use-c++-comments))

(use-package pass
  :ensure t
  :defer)

(use-package pcre2el
  :ensure t
  :defer)

;; FIXME: require when creating graphical frame from terminal
(use-package pdf-tools
  :ensure t
  :when (display-graphic-p)
  :init
  (pdf-tools-install t t t)
  ;; (add-hook'pdf-view-mode-hook #'turn-on-auto-revert-mode)
  (setq-default pdf-view-display-size 'fit-page))

(use-package perl-mode
  :mode "\\.latexmkrc\\'")

(use-package perspective
  :ensure t
  :defer)

(use-package projectile
  :ensure t
  :defer
  :init
  (add-hook 'helm-mode-hook #'projectile-mode)
  (setq-default
   projectile-completion-system           'helm
   projectile-find-dir-includes-top-level t))

(use-package prolog
  :mode ("\\.pl\\'" . prolog-mode)
  :config
  (setq-default prolog-system 'swi))

(use-package python
  :defer
  :config
  (setq-default
   python-check-command
   (or (executable-find "epylint3")
       (executable-find "epylint" )
       (executable-find "pyflakes")
       python-check-command)
   python-shell-interpreter "ipython3"))

(use-package remember
  :bind (("<f7>" . remember-notes)
         ("<f8>" . remember-notes-save-and-bury-buffer))
  :config
  (setq-default remember-notes-initial-major-mode #'org-mode))

(use-package rx
  :bind ("C-c r" . rx-to-string-bold)
  :init
  (defun rx-to-string-bold (form)
    "Interactively wrap `rx-to-string' and remove shy groups around result."
    (interactive "sRegExp: ")
    (message "String: \"%s\"" (rx-to-string form t))))

(use-package sass-mode
  :ensure t
  :defer
  :init
  (add-hook 'sass-mode-hook #'use-c++-comments))

(if emacs-25+
    (use-package saveplace
      :defer
      :init
      (save-place-mode))
  (use-package saveplace
    :init
    (setq-default save-place t)))

(use-package server
  :defer
  :init
  (setq-default server-kill-new-buffers nil))

(use-package sh-script
  :defer
  :config
  (setq-default sh-basic-offset 2
                sh-indentation  2))

(use-package simple
  :bind (("M-\\"    . cycle-spacing     )
         ("C-x C-k" . kill-whole-line   )
         ("C-x C-p" . open-previous-line)
         ("C-x C-n" . open-next-line    ))
  :init
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

  (add-hook 'special-mode-hook #'turn-off-line-numbers)
  (with-current-buffer (messages-buffer)
    (turn-off-line-numbers))

  (column-number-mode))

(use-package sl
  :ensure t
  :defer)

(use-package smerge-mode
  :defer
  :init
  (defun sniff-smerge-session ()
    "Conditionally enable `smerge-mode'.
Enable `smerge-mode' only if the buffer is reasonably sized and
contains conflict markers."
    (when (and (not (large-buffer-p))
               (save-excursion
                 (goto-char (point-min))
                 (re-search-forward "^<<<<<<< " nil t)))
      (smerge-start-session)
      (when smerge-mode
        (message "Merge conflict detected. Enabled `smerge-mode'."))))

  (add-hook 'find-file-hook #'sniff-smerge-session t))

(use-package solarized-theme
  :disabled
  :ensure t)

(use-package speedbar
  :defer
  :config
  (setq-default
   speedbar-show-unknown-files t
   speedbar-update-flag        nil
   speedbar-use-images         t
   speedbar-vc-do-check        nil))

(use-package sr-speedbar
  :ensure t
  :after speedbar
  :bind ("C-x t" . sr-speedbar-toggle)
  :config
  (setq-default sr-speedbar-auto-refresh nil))

(use-package subword
  :defer
  :init
  (global-subword-mode))

(use-package tex
  :ensure auctex
  :defer
  :defines LaTeX-clean-intermediate-suffixes
  :functions setup-latexmk TeX-doc TeX-revert-document-buffer
  :config
  (setq-default
   LaTeX-csquotes-open-quote  "\\enquote{"
   LaTeX-csquotes-close-quote "}"
   TeX-auto-save              t
   TeX-parse-self             t
   TeX-PDF-mode               t)

  (bind-key "C-c ?" #'TeX-doc TeX-mode-map)

  (defun setup-latexmk ()
    "Define Latexmk continuous preview command and intermediate suffixes."
    (add-to-lists
     '((TeX-command-list
        ("Latexmk"                        ; Command name
         "latexmk -pvc -view=none %t"     ; Non-expanded shell command
         TeX-run-command                  ; Process handler
         t                                ; Confirm expanded shell command
         (LaTeX-mode)                     ; Applicable modes
         :help "Run Latexmk"))            ; Command description
       (LaTeX-clean-intermediate-suffixes
        "\\.fdb_latexmk")))
    (setq TeX-command-default "Latexmk"))

  (add-hooks-n
   `((LaTeX-mode-hook                          ,#'setup-latexmk
                                               ,#'turn-on-auto-fill         )
     (TeX-after-compilation-finished-functions ,#'TeX-revert-document-buffer)))

  ;; Set priority of pre-configured PDF viewers to PDF Tools, then Zathura
  (let ((program-list TeX-view-program-list-builtin))
    (push `(output-pdf ,(car (or (assoc "PDF Tools" program-list)
                                 (assoc "Zathura"   program-list))))
          TeX-view-program-selection)))

(use-package time
  :defer
  :init
  (let ((fmt "%a %d %b %R %z"))
    (setq-default
     display-time-format                 fmt
     display-time-load-average-threshold 0
     display-time-mail-string            "✉"
     display-time-world-list             '(("Europe/Dublin" "Dublin"  )
                                           ("Africa/Harare" "Harare"  )
                                           ("Europe/Athens" "Athens"  )
                                           ("Asia/Tel_Aviv" "Tel Aviv"))
     display-time-world-time-format      fmt))

  (display-time))

(use-package tool-bar
  :defer
  :preface
  (with-graphical-frame
   #'(lambda (frame)
       (tool-bar-mode 0))))

(use-package top-mode
  :ensure t
  :defer)

(use-package uniquify
  :defer
  :init
  (setq-default uniquify-buffer-name-style 'forward))

(use-package vc-hooks
  :defer
  :init
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
  :init
  (apply #'add-hooks-t #'wc-mode fundamental-hooks)
  :config
  (setq-default wc-modeline-format "[%tll]"))

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.mustache\\'"))

(use-package whitespace
  :defer
  :init
  (setq-default whitespace-style '(face tabs trailing empty tab-mark))
  (global-whitespace-mode))

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
  :defer
  :init
  (winner-mode))

(use-package woman
  :defer
  :init
  (add-hook 'woman-mode-hook #'remap-woman-faces))

(use-package wrap-region
  :ensure t
  :defer
  :init
  (wrap-region-global-mode)
  (setq-default
   wrap-region-only-with-negative-prefix t
   wrap-region-tag-active-modes          '(html-mode mustache-mode web-mode))
  (wrap-region-add-wrapper "{{#i18n}}" "{{/i18n}}" "i"))

(use-package wttrin
  :ensure t
  :defer
  :config
  (setq-default
   wttrin-default-cities
   '(athens-greece
     avoca-ireland
     dublin-ireland
     tel-aviv-israel
     harare-zimbabwe
     moon)))

(use-package xref-js2
  :if     emacs-25+
  ;; FIXME: use value of emacs-25+
  :ensure t
  :defer
  :functions turn-on-xref-js2
  :init
  (defun turn-on-xref-js2 ()
    "Add xref-js2 backend and sanitise keymap."
    (unbind-key "M-." js2-mode-map)     ; Reused by xref
    (add-hook 'xref-backend-functions
              #'xref-js2-xref-backend nil t))

  (add-hook 'js2-mode-hook #'turn-on-xref-js2))

(use-package xt-mouse
  :unless (display-graphic-p)
  :defer
  :init
  (xterm-mouse-mode))

(use-package yaml-mode
  :ensure t
  :defer)

;; FIXME: make isearch lazy highlights stand out
(use-package zenburn-theme
  :ensure t
  :defer
  :init
  (load-theme 'zenburn t))

