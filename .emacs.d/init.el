;;; ====
;;; TODO
;;; ====

;; Fix
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
  (when (y-or-n-p "use-package not installed; would you like to install it?")
    (package-refresh-contents)
    (package-install 'use-package)))

(eval-when-compile
  (defvar use-package-verbose 'debug
    "Report package compilation, loading and configuration details.")
  (require 'use-package))
(require 'bind-key)

;;; ===========
;;; Definitions
;;; ===========

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

(defun no-electric-indent (&rest _)
  "Locally disable electric indentation."
  (electric-indent-local-mode 0))

(defun no-line-numbers (&rest _)
  "Locally disable display of line numbers."
  (nlinum-mode 0))

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
  (let ((split-vertically-p (window-combined-p)))
    (delete-window)
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically))
    (switch-to-buffer nil)))

(defun use-c++-comments ()
  "Default to single-line C++-style comments."
  (setq-local comment-start "//")
  (setq-local comment-end     ""))

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
  "Individual hooks to hang from for global effect.
FIXME: This should not be necessary.")

(defface man-header
  '((t :foreground "#b4fa70" :weight bold))
  "Man page heading face."
  :group 'man)

(defface man-emph
  '((t :foreground "#e9b96e" :underline t))
  "Man page emphasis/underline face."
  :group 'man)

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

(with-graphical-frame
 #'(lambda (frame)
     (set-face-attribute 'default frame :family "DejaVu Sans Mono" :height 80)))

(put   #'upcase-region 'disabled nil)
(put #'downcase-region 'disabled nil)

(defalias #'yes-or-no-p #'y-or-n-p)

;; Scrolling

(setq
 isearch-allow-scroll            t
 scroll-conservatively           most-positive-fixnum
 scroll-error-top-bottom         t
 scroll-margin                   1
 scroll-preserve-screen-position t
 scroll-step                     1)

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
  :load-path "/usr/share/emacs24/site-lisp/debian-el"
  :mode ("\\.sources\\'" . apt-sources-mode)
  :config
  (add-hook 'apt-sources-mode-hook #'no-electric-indent))

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
  :functions (c-lineup-arglist c++-lambda-indent)
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

  (add-hook 'c-mode-common-hook #'use-c++-comments)

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
  :ensure t
  :disabled)

(use-package comint
  :defer
  :init
  (add-hook 'comint-mode-hook #'no-line-numbers))

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
  (add-hook 'conf-mode-hook #'no-electric-indent))

(use-package crontab-mode
  :ensure t
  :mode ("\\.cron\\(?:tab\\)??\\'" "cron\\(?:tab\\)??\\."))

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
                (no-electric-indent)
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

(use-package ebib
  :ensure t
  :bind ("C-c e" . ebib)
  :config
  (setq-default
   ebib-bibtex-dialect 'biblatex
   ebib-use-timestamp  t))

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
  (add-hook 'eshell-mode-hook #'no-line-numbers))

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
    (let ((face (or (get-char-property (point) #'read-face-name)
                    (get-char-property (point) 'face))))
      (if face (message "Face: %s" face) (message "No face at %d" pos)))))

(use-package fic-mode
  :ensure t
  :defer
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'fic-mode))
  :config
  (dolist (word '("KLUDGE" "HACK"))
    (add-to-list 'fic-highlighted-words word)))

(use-package files
  :bind ("<f5>" . refresh-buffer)
  :init
  (defun refresh-buffer ()
    "Reconcile current buffer with what lives on the disk.
Offer to revert from the auto-save file, if that exists."
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
  :init
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
         (no-line-numbers)))))

(use-package fill-column-indicator
  :ensure t
  :defer
  :init
  (mapc #'(lambda (hook) (add-hook hook #'fci-mode)) all-hooks)
  :config
  (setq-default fci-rule-column 80
                fci-rule-color "#696969"))

(use-package flex-mode
  :load-path "lisp"
  :mode "\\.lex\\'")

(use-package font-lock
  :defer
  :config
  (setq-default font-lock-maximum-decoration t))

(use-package frame
  :defer
  :config
  (blink-cursor-mode 0))

(use-package git-commit
  :ensure t
  ;; Need to load package to know when to load package :(
  :mode ("/\\(?:\
\\(?:\\(?:COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\
\\|BRANCH_DESCRIPTION\\)\\'" . git-commit-mode)
  :config
  (global-git-commit-mode)

  (setq-default git-commit-fill-column 68)

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
  :disabled
  :ensure t
  :bind (("M-," . golden-ratio-scroll-screen-down)
         ("M-." . golden-ratio-scroll-screen-up)))

(use-package haskell-cabal
  :ensure haskell-mode
  :defer
  :config
  (add-hook 'haskell-cabal-mode-hook #'no-electric-indent))

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

  (when (bound-and-true-p helm-white-selection)
    (set-face-foreground 'helm-selection "#ffffff"))

  (add-hook 'helm-major-mode-hook #'no-line-numbers)

  (helm-mode))

(use-package helm-pass
  :ensure t
  :defer)

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
  (setq-default ido-enable-flex-matching 1))

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
  :defer
  :config
  (setq-default jit-lock-stealth-time 4))

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
                (no-electric-indent)
                (js2-highlight-unused-variables-mode)))

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
  :defer
  :commands js3-enter-key
  :config
  (unbind-key "C-c C-g" js3-mode-map)   ; Why...
  (no-trailing-enter #'js3-enter-key)   ; For comments

  (add-hook 'js3-mode-hook #'fic-mode)

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
  :mode ("\\.ledger\\'" "\\.journal\\'")
  :config
  (setq-default ledger-use-iso-dates t))

(use-package lisp-mode
  :defer
  :config
  (add-hook 'lisp-mode-hook #'no-electric-indent))

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

  (add-hook 'magit-mode-hook #'no-line-numbers)

  (setq-default
   magit-log-arguments    '("-n32" "--graph" "--decorate")
   magit-rebase-arguments '("--interactive"))

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
            magit-refs-tags-format)))

  (set-face-attribute
   'magit-blame-heading nil
   :background "#696969"
   :foreground "#ffffff")
  (set-face-attribute
   'magit-header-line nil
   :inherit    'magit-section-heading
   :background (internal-get-lisp-face-attribute 'default :background))
  (set-foregrounds
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

(use-package man
  :defer
  :config
  (add-hook 'Man-mode-hook #'remap-man-faces))

(use-package markdown-mode
  :ensure t
  :commands markdown-cycle markdown-enter-key
  :mode ("\\.md\\'" "\\.markdown\\'")
  :config
  (bind-key "TAB" #'markdown-cycle markdown-mode-map)
  (no-trailing-enter #'markdown-enter-key))

(use-package minibuffer
  :defer
  :init
  (defconst gc-orig-thresh gc-cons-threshold
    "See URL `http://bling.github.io/blog/2016/01/18/\
why-are-you-changing-gc-cons-threshold/'")
  (add-hook 'minibuffer-setup-hook
            #'(lambda () (setq gc-cons-threshold most-positive-fixnum)))
  (add-hook 'minibuffer-exit-hook
            #'(lambda () (setq gc-cons-threshold gc-orig-thresh))))

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
  :config
  ;; One line at a time
  (setq-default mouse-wheel-scroll-amount '(1 ((shift) . 1))))

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

(use-package org
  :bind ("C-c l" . org-store-link)
  :init
  (setq org-special-ctrl-a/e 'reversed))

(use-package palette
  :ensure t
  :defer
  :when (display-graphic-p))

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
  (add-hook 'pascal-mode-hook #'use-c++-comments))

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
  (setq-default remember-notes-initial-major-mode 'org-mode))

(use-package rx
  :bind ("C-c r" . rx-to-string-bold)
  :config
  (defun rx-to-string-bold (form)
    "Interactively wrap `rx-to-string' and remove shy groups around result."
    (interactive "sRegExp: ")
    (message "String: \"%s\"" (rx-to-string form t))))

(use-package sass-mode
  :ensure t
  :defer
  :config
  (add-hook 'sass-mode-hook #'use-c++-comments))

(use-package saveplace
  :config
  (if emacs-25+
      (save-place-mode)
    (setq-default save-place t)))

(use-package server
  :defer
  :config
  (setq-default server-kill-new-buffers nil))

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
  (setq-default
   speedbar-show-unknown-files t
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
  :commands latexmk-pvc
  :config
  ;; TODO:
  ;; * `start-process' instead?
  ;; * Accept overriding file name?
  (defun latexmk-pvc ()
    "Continuously preview current LaTeX document with `latexmk'."
    (interactive)
    (unless buffer-file-name
      (error "Current buffer is not visiting a file."))
    (let ((command (combine-and-quote-strings
                    `("latexmk" "-pvc" ,(file-name-base))
                    " ")))
      (async-shell-command command)))

  (setq-default
   LaTeX-csquotes-open-quote  "\\enquote{"
   LaTeX-csquotes-close-quote "}"
   TeX-PDF-mode               t)

  (let ((view-program (assq 'output-pdf TeX-view-program-selection)))
    (setf (cadr view-program) "Zathura")))

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
  :init
  (mapc #'(lambda (hook) (add-hook hook #'wc-mode)) all-hooks)
  :config
  (setq-default wc-modeline-format "[%tll]"))

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.mustache\\'"))

(use-package whitespace
  :config
  (global-whitespace-mode)
  (setq-default whitespace-style '(face tabs trailing empty tab-mark)))

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

(use-package woman
  :defer
  :config
  (add-hook 'woman-mode-hook #'remap-woman-faces))

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
  (setq-default
   wttrin-default-cities
   '(athens-greece
     avoca-ireland
     dublin-ireland
     tel-aviv-israel
     harare-zimbabwe
     moon)))

`(use-package xref-js2
   :if     ,emacs-25+
   :ensure ,emacs-25+
   :defer
   :init
   (add-hook 'js2-mode-hook
             #'(lambda ()
                 (unbind-key "M-." js2-mode-map)
                 (add-hook 'xref-backend-functions
                           #'xref-js2-xref-backend nil t))))

(use-package xt-mouse
  :unless (display-graphic-p)
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
             "See URL `https://github.com/jwiegley/dot-emacs'."
             (let ((elapsed (float-time (time-subtract (current-time)
                                                       emacs-start-time))))
               (message "Loading %s...done (%.3fs)" ,load-file-name elapsed)))
          t)
