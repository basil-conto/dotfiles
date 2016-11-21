;;; init.el --- init file for blc -*- lexical-binding: t -*-

;;; Commentary:

;; TODO
;; * Create macro & special form mapper
;; * `electric-indent-inhibit' vs `blc-turn-off-electric-indent-local-mode'
;; * Displace colour codes with relative faces
;; * Fix hard-coded load paths
;; * Write setter creator macro
;; * Improve autoloading of pdf-tools
;; * Sniff features and explit `use-package' :requires
;; * Window splitting - add minimum
;;   or customise `magit-display-buffer-function' use-case
;; * Delight modes
;; * Separate config data from logic, particularly w.r.t. sensitive data
;; * :bind local :maps once their autoloading is fixed upstream
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

;;; Code:


;;;; BOOTSTRAPPING

;;; Performance

;; Lexically capture start time and original GC threshold
(let ((start     (current-time))
      (file      load-file-name)
      (oldthresh gc-cons-threshold)
      (maxthresh (lsh gc-cons-threshold 4)))

  (defun blc-report-init-time ()
    "Report total user initialisation time.
See URL `https://github.com/jwiegley/dot-emacs'."
    (let ((delta (float-time (time-subtract (current-time) start))))
      (message "Loading %s...done (%.3fs)" file delta)))

  (defun blc-increase-gc-thresh ()
    "Increase number of cons bytes between GCs.
See URL `http://bling.github.io/blog/2016/01/18/\
why-are-you-changing-gc-cons-threshold/'."
    (setq-default gc-cons-threshold maxthresh))

  (defun blc-restore-gc-thresh ()
    "Restore original number of cons bytes between GCs.
See URL `http://bling.github.io/blog/2016/01/18/\
why-are-you-changing-gc-cons-threshold/'."
    (setq-default gc-cons-threshold oldthresh)))

;; Increase GC threshold to reduce number of GCs during initialisation
(blc-increase-gc-thresh)

;;; Packaging

;; Built-in dependencies
(require 'package)
(require 'seq)
(require 'subr-x)

;; Package system
(setq-default
 package-enable-at-startup nil          ; Activate packages manually
 package-archives                       ; HTTPS only
 (mapcar #'(lambda (cell)
             `(,(car cell) .
               ,(replace-regexp-in-string
                 "^\\(http\\):" "https" (cdr cell) nil t 1)))
         package-archives))
(push '("melpa" . "https://melpa.org/packages/") package-archives)
(advice-add #'package--save-selected-packages :override #'ignore) ; Sandbox
(package-initialize)

;; Third-party dependencies
(when-let ((deps    '(dash dash-functional f s use-package zenburn-theme))
           (missing (seq-remove #'package-installed-p deps)))
  (when (y-or-n-p (format "Install missing packages %s?" missing))
    (package-refresh-contents)
    (mapc #'package-install missing)))

(eval-when-compile
  (setq-default use-package-verbose 'debug)
  (require 'use-package))
(require 'bind-key)
(require 'dash)
(require 'dash-functional)
(require 'f)


;;;; DEFINITIONS

;;; Malformed types

(defun blc-as-symbol (string-or-symbol)
  "Return the canonical symbol named STRING-OR-SYMBOL."
  (intern-soft string-or-symbol))

(defun blc-as-string (object)
  "Convert OBJECT to a string."
  (pcase object
    ((pred symbolp)
     (symbol-name object))
    ((pred numberp)
     (number-to-string object))
    ((pred sequencep)
     (seq-into object 'string))
    (_ "")))

(defun blc-boolean-to-natnum (boolean)
  "Return 0 if BOOLEAN is `nil' and 1 otherwise."
  (if boolean 1 0))

(defun blc-symcat (&rest objects)
  "Concatenate all OBJECTS under `blc-as-string' as a symbol."
  (intern (mapconcat #'blc-as-string objects "")))

(defun blc-tree-sed (regexp rep tree &rest args)
  "Replace all matches for REGEXP with REP in TREE.
Recursively perform `replace-regexp-in-string' on TREE. REGEXP,
REP and the optional ARGS are passed unchanged to
`replace-regexp-in-string', which see.

Note: Cons pairs are considered leaf nodes."
  (-tree-map-nodes #'stringp
                   (-cut apply #'replace-regexp-in-string regexp rep <> args)
                   tree))

(defun blc-apply-safe (fun &rest args)
  "Apply FUN to ARGS only if symbol FUN has a function definition.
Issue a warning otherwise."
  (if (fboundp fun)
      (apply fun args)
    (lwarn 'blc :error "Invalid function: %S" fun)))


;;; Byte-compiler declarations

(eval-when-compile
  (declare-function c-langelem-pos   "cc-defs")
  (declare-function csv-align-fields "csv-mode")
  (defvar c-mode-base-map)
  (defvar git-commit-filename-regexp)
  (defvar git-commit-mode)
  (defvar ivy-minibuffer-faces)
  (defvar js2-mode-map)
  (defvar recentf-list)
  (defvar smerge-mode)
  (defvar TeX-command-default)
  (defvar zenburn-default-colors-alist))

;;; Advice

(defun blc-split-larger-dimension--advice (split &rest args)
  "Sacrifice the larger window dimension when splitting."
  (let ((split-width-threshold       (window-height))
        (split-height-threshold (lsh (window-width) -1))) ; Adjust slightly
    (apply split args)))

;; TODO: Define no-arg macro?
(defun blc-trim-before-newline--advice (&rest _)
  "Delete trailing whitespace prior to newline insertion."
  (delete-trailing-whitespace (line-beginning-position) (line-end-position)))

(defun blc-c++-lambda-indent--advice (langelem)
  "Return indentation offset for C++11 lambda function arguments.
Currently keeps offset unchanged by returning 0 for lambda
functions opened as arguments and `nil' for everything else.
Adapted from URL `http://stackoverflow.com/a/23553882'."
  (and (eq major-mode 'c++-mode)
       (ignore-errors
         (save-excursion
           (goto-char (c-langelem-pos langelem))
           ;; Detect "[...](" or "[...]{",
           ;; preceded by "," or "(" and with unclosed brace
           (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$")))
       0))


;;; Modes

(defun blc-turn-off-modes (&rest modes)
  "Attempt to pass 0 to all MODES."
  (mapc (-rpartial #'blc-apply-safe 0) modes))

;; TODO:
;; * Make less verbose and more practical and general
;; * It would be nice to be able to mix & match hook lists and functions
(defmacro blc-defuse (name docstring &rest modes)
  "Define mode-disabling function `blc-turn-off-NAME'."
  (declare (doc-string 2) (indent defun))
  `(defun ,(blc-symcat "blc-turn-off-" name) (&rest _)
     ,docstring
     (interactive)
     (blc-turn-off-modes ,@modes)))

;; TODO: Disable globally?
(blc-defuse electric-indent-local-mode
  "Disable `electric-indent-local-mode'."
  #'electric-indent-local-mode)

(blc-defuse line-numbers
  "Locally disable display of line numbers."
  #'nlinum-mode
  #'linum-mode)

(blc-defuse prettify-symbols-mode
  "Disable `prettify-symbols-mode'."
  #'prettify-symbols-mode)

(blc-defuse flycheck-mode
  "Disable `flycheck-mode'."
  #'flycheck-mode)

(defun blc-use-c++-comments ()
  "Default to single-line C++-style comments."
  (setq comment-start "//"
        comment-end     ""))

(defun blc-enable-disaster ()
  "Enable `disaster' in `c-mode' derivatives."
  (bind-key "C-c d" #'disaster c-mode-base-map))

(defun blc-some-recentf (&optional count)
  "Return first COUNT or 5 items in `recentf-list'."
  (-take (or count 5) recentf-list))

(defun blc-align-all-csv-fields ()
  "Align all fields in the current CSV buffer."
  (csv-align-fields nil (point-min) (point-max)))

(defun blc-kill-git-commit-buffer ()
  "Ensure message buffer is killed post-git-commit."
  (and git-commit-mode
       buffer-file-name
       (string-match-p git-commit-filename-regexp buffer-file-name)
       (kill-buffer)))

(defun blc-account-concat (category &optional accounts acct-sep list-sep)
  "Join ACCOUNTS with their account CATEGORY.
Return a LIST-SEP-delimited (default \" \") string of account
prefixed by CATEGORY and ACCT-SEP (default \":\")."
  (let ((list-sep (or list-sep " "))
        (acct-sep (or acct-sep ":")))
    (mapconcat #'(lambda (account)
                   (string-join `(,category ,account) acct-sep))
               accounts
               list-sep)))

(defun blc-delight-isearch ()
  "Shorten lighter of `isearch-mode'."
  (setq isearch-mode " 🔍"))

(defun blc-sniff-smerge ()
  "Conditionally enable `smerge-mode'.
Enable `smerge-mode' only if buffer is reasonably sized and
contains conflict markers."
  (when (and (not (blc-large-buffer-p))
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "^<<<<<<< " nil t)))
    (smerge-start-session)
    (when smerge-mode
      (message "Merge conflict detected. Enabled `smerge-mode'."))))

(defun blc-setup-latexmk ()
  "Define Latexmk continuous preview and intermediate suffixes."
  (mapc (-applify #'add-to-list)
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

(defun blc-turn-on-xref-js2 ()
  "Register xref-js2 backend and sanitise keymap."
  (unbind-key "M-." js2-mode-map)     ; Reused by xref
  (add-hook 'xref-backend-functions
            #'xref-js2-xref-backend nil t))


;;; Editing

(defun blc-fast-line-number (&optional as-string)
  "Return line number of point within accessible portion of buffer.
If AS-STRING is non-`nil', return line number as string. See URL
`http://emacs.stackexchange.com/a/3822' for limitations."
  (let ((line-number-display-limit-width most-positive-fixnum)
        (conv (if as-string #'identity #'string-to-number)))
    (funcall conv (format-mode-line "%l"))))

(defun blc-fast-line-count ()
  "Return number of lines in buffer.
See `fast-line-number'."
  (let ((max (point-max)))
    (save-excursion
      (goto-char max)
      (- (blc-fast-line-number)
         (blc-boolean-to-natnum
          (= max (line-beginning-position)))))))

(defun blc-echo-fast-line-count ()
  "Emulate `count-lines-page' using `blc-fast-line-count'."
  (interactive)
  (let* ((total   (blc-fast-line-count))
         (current (blc-fast-line-number))
         (before  (min current total))
         (after   (- total before)))
    (message "Buffer has %d lines (%d + %d)" total before after)))

(defun blc-trim-before-newline (newline-function)
  "Advise NEWLINE-FUNCTION to first delete trailing whitespace."
  (advice-add newline-function :before #'blc-trim-before-newline--advice))

(defun blc-large-buffer-p ()
  "Determine whether buffer classifies as being large.
Return `t' if buffer size falls under
`large-file-warning-threshold', else `nil'."
  (> (buffer-size) large-file-warning-threshold))

(defun blc-strip-buffer ()
  "Try to make the current buffer as responsive as possible."
  (interactive)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (fundamental-mode)
  (blc-turn-off-line-numbers)
  (blc-turn-off-modes #'font-lock-mode))

(defun blc-strip-large-buffer ()
  "Call `strip-down-buffer' if current buffer is large."
  (when (blc-large-buffer-p)
    (blc-strip-buffer)))

(defun blc-revert-buffer ()
  "Reconcile current buffer with what lives on the disk.
Offer to revert from the auto-save file, if that exists."
  (interactive)
  (revert-buffer nil t))

(defun blc-switch-to-temp-file (&optional prefix suffix)
  "Create and switch to a temporary file.
A non-empty filename PREFIX can help identify the file or its
purpose, whereas a non-empty SUFFIX will help determine the
relevant major-mode."
  (interactive "sfile prefix: \nsfile extension: ")
  (let ((pre (or prefix ""))
        (suf (unless (string-blank-p suffix) (f-swap-ext "" suffix))))
    (find-file (make-temp-file pre nil suf))))

;; TODO: Operate on region as well?
(defun blc-iwb ()
  "Indent Whole Buffer and delete trailing whitespace.
See URL `http://emacsblog.org/2007/01/17/indent-whole-buffer/'."
  (interactive)
  (let ((min (point-min-marker))
        (max (point-max-marker)))
    (delete-trailing-whitespace)
    (indent-region min max)
    (untabify      min max)))

(defun blc-transpose-split ()
  "Alternate between vertical and horizontal frame split.
Assumes frame is split exactly in two. Adapted from Wilfred's
function at URL
`https://www.emacswiki.org/emacs/ToggleWindowSplit'."
  (interactive)
  (unless (= (length (window-list)) 2)
    (error "Can only toggle a frame split in twain"))
  (let ((split (if (window-combined-p)
                   #'split-window-horizontally
                 #'split-window-vertically)))
    (delete-window)
    (funcall split)
    (switch-to-buffer nil)))

;; ;; FIXME
;; ;; * See URL `https://www.emacswiki.org/emacs/ApplesAndOranges'
;; (defun blc-with-graphical-frame (fun)
;;   "Run abnormal hook now, in current frame, and with every new frame."
;;   (let ((frame (selected-frame)))
;;     (when (display-graphic-p frame)
;;       (funcall fun frame)))
;;   (add-hook 'after-make-frame-functions
;;             `(lambda (frame)
;;                (when (display-graphic-p frame)
;;                  (funcall ,fun frame)))))

(defun blc-open-line (forward)
  "Open empty line (FORWARD - 1) lines in front of current line."
  (save-excursion
    (end-of-line forward)
    (open-line 1)))

(defun blc-open-previous-line ()
  "Open empty line before current line."
  (interactive)
  (blc-open-line 0))

(defun blc-open-next-line ()
  "Open empty line after current line."
  (interactive)
  (blc-open-line 1))

(defun blc-small-scroll-up ()
  "Scroll up `blc-small-scroll-step' lines."
  (interactive)
  (scroll-up blc-small-scroll-step))

(defun blc-small-scroll-down ()
  "Scroll down `blc-small-scroll-step' lines."
  (interactive)
  (scroll-down blc-small-scroll-step))

(defun blc-align-punctuation ()
  "Horizontally align mode-specific punctuation in region."
  (interactive)
  (unless (use-region-p)
    (mark-paragraph))
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)\\s."))


;;; Themes

(defun blc-man-fontify (&rest _)
  "Customise `Man-mode' faces."
  (mapc (-applify #'face-remap-add-relative)
        '((Man-overstrike font-lock-keyword-face)
          (Man-underline  font-lock-string-face ))))

(defun blc-woman-fontify (&rest _)
  "Customise `woman-mode' faces."
  (mapc (-applify #'face-remap-add-relative)
        '((woman-bold   font-lock-keyword-face)
          (woman-italic font-lock-string-face ))))

(defun blc-zenburn-assoc (colour)
  "Return the `zenburn-theme' values associated with COLOURS.
For each colour name in COLOURS return its corresponding CDR slot
in `zenburn-default-colors-alist'."
  (cdr (assoc-string colour zenburn-default-colors-alist)))

(defun blc-zenburn-brighten-fci ()
  "Distinguish FCI rule from background under 256 colours."
  (setq-default fci-rule-color (blc-zenburn-assoc 'zenburn-bg+1)))

(defun blc-zenburn-darken-ivy ()
  "Darken background of `ivy' matches under `zenburn-theme'."
  (-zip-with
   #'set-face-background
   (cdr ivy-minibuffer-faces)
   (-map #'blc-zenburn-assoc '(zenburn-red-4 zenburn-blue-4 zenburn-green-1))))

(defun blc-zenburn-darken-linum ()
  "Darken foreground of face `linum' under `zenburn-theme'."
  (set-face-foreground 'linum (blc-zenburn-assoc 'zenburn-bg+3)))

(defun blc-setup-theme-zenburn ()
  "Customise `zenburn-theme' to taste."
  (set-face-background 'highlight (blc-zenburn-assoc 'zenburn-bg-1))

  (mapc (-applify #'add-hook)
   `((   fci-mode-hook ,#'blc-zenburn-brighten-fci)
     (   ivy-mode-hook ,#'blc-zenburn-darken-ivy  )
     (nlinum-mode-hook ,#'blc-zenburn-darken-linum))))

;;; Variables

(defvar blc-repos-dir (f-join user-emacs-directory "repos")
  "Directory containing symlinks to user Git repositories.")

(defvar blc-small-scroll-step 6
  "Number of lines constituting a small scroll.")

(defvar blc-fundamental-hooks
  (mapcar (-rpartial #'blc-symcat "-mode-hook")
          '(conf ess haskell-cabal hledger mustache prog text))
  "Hooks whose modes derive from `fundamental-mode' or nothing.")


;;;; MISCELLANEA

;; FIXME: Make isearch lazy highlights stand out
(let ((theme 'zenburn))
  (and (load-theme theme t)
       (blc-apply-safe (blc-symcat "blc-setup-theme-" theme))))

(when (display-graphic-p)
  (set-face-attribute 'default nil :font "DejaVu Sans Mono 8"))

(defalias #'yes-or-no-p #'y-or-n-p)

(setq-default
 source-directory                (f-join blc-repos-dir "localsrc" "emacs")
 ;; Movement/drawing
 recenter-redisplay              nil
 scroll-conservatively           most-positive-fixnum
 scroll-margin                   1
 scroll-preserve-screen-position t
 scroll-step                     1
 ;; Spacing
 fill-column                     80
 indent-tabs-mode                nil
 tab-width                       2)

;;; Bindings

(bind-keys
 ;; Alignment
 ("C-c P"       .    blc-align-punctuation)
 ;; Line
 ("C-c i"       .          indent-relative)
 ("C-x l"       . blc-echo-fast-line-count)
 ("C-x C-p"     .   blc-open-previous-line)
 ("C-x C-n"     .   blc-open-next-line    )
 ;; Window / buffer
 ("C-x 7"       .      blc-transpose-split)
 ("S-<prior>"   .          previous-buffer)
 ("S-<next>"    .              next-buffer)
 ("<f5>"        .        blc-revert-buffer)
 ;; Mutatis mutandis within tmux
 ("M-[ 5 ; 2 ~" .          previous-buffer)
 ("M-[ 6 ; 2 ~" .              next-buffer)
 ;; Movement / drawing
 ("M-R"         .           redraw-display)
 ("M-{"         .    blc-small-scroll-down)
 ("M-}"         .    blc-small-scroll-up  ))


;;;; PACKAGES

(use-package 2048-game
  :ensure t
  :defer)

(use-package ac-js2
  :disabled
  :ensure t)

(use-package ace-window
  :ensure t
  :bind ("M-]" . ace-window))

(use-package ag
  :ensure t
  :defer
  :config
  (setq-default ag-highlight-search t)
  (add-to-list 'ag-arguments "-C 5"))

(use-package apt-sources
  ;; FIXME
  :load-path "/usr/share/emacs/site-lisp/debian-el"
  :mode ("\\.sources\\'" . apt-sources-mode)
  :init
  (add-hook 'apt-sources-mode-hook #'blc-turn-off-electric-indent-local-mode))

(use-package ascii
  :ensure t
  :defer)

(use-package ascii-art-to-unicode
  :ensure t
  :defer)

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
  (setq-default
   auth-source-debug t
   ;; Add SMTPS port 465
   auth-source-protocols
   (if-let ((protos  auth-source-protocols)
            (proto   'smtp)
            (secport "465")
            (stdport  "25")
            (defined (memq proto protos)))
       ;; No lexical `add-to-list' ;_;
       (-map-first (-lambda ((key)) (eq key proto))
                   (-rpartial #'-union `(,secport))
                   protos)
     (-snoc protos `(,proto ,(blc-as-string proto) ,stdport ,secport)))))

(use-package avy
  :ensure t
  :bind (("C-c #" . avy-goto-char-timer)
         ("M-g f" . avy-goto-line      )))

(use-package base16-theme
  :disabled
  :ensure t)

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
  :functions c-lineup-arglist
  :init
  (add-hook 'c-mode-common-hook #'blc-use-c++-comments)
  :config
  (let ((name    "blc")
        (base    "linux")
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

    (setf (alist-get 'other c-default-style) name))

  (advice-add #'c-lineup-arglist :before-until #'blc-c++-lambda-indent--advice))

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
  (add-hook 'comint-mode-hook #'blc-turn-off-line-numbers))

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
  (add-hook 'conf-mode-hook #'blc-turn-off-electric-indent-local-mode))

(use-package counsel
  :ensure t
  :defer
  :bind
  (("M-x"     . counsel-M-x)
   ("M-y"     . counsel-yank-pop)
   ("C-s"     . counsel-grep-or-swiper)
   ("C-c g"   . counsel-ag)
   ("C-c t"   . counsel-git)
   ("C-c u"   . counsel-unicode-char)
   ("C-h f"   . counsel-describe-function)
   ("C-h v"   . counsel-describe-variable)
   ("C-h S"   . counsel-info-lookup-symbol)
   ("C-h C-j" . counsel-describe-face)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-l" . counsel-locate)
   ("C-c j d" . counsel-dired-jump)
   ("C-c j f" . counsel-file-jump))
  :config
  (setq-default
   ;; Search with smart case and shell expansion
   counsel-grep-base-command "ag --nocolor \"%s\" %s"
   ;; Do not match start of input for counsel commands
   ivy-initial-inputs-alist
   (-remove #'(lambda (cell)
                (string-prefix-p "counsel-" (blc-as-string (car cell))))
            ivy-initial-inputs-alist))

  (ivy-set-sources
   'counsel-locate
   '((blc-some-recentf)
     (original-source))))

(use-package counsel-projectile
  :ensure t
  :defer)

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
  :defer
  :init
  (add-hook 'csv-mode-hook #'blc-align-all-csv-fields)
  :config
  (setq-default csv-align-style 'auto))

(use-package dafny-mode
  :ensure boogie-friends
  :defer
  :init
  (mapc (-partial #'add-hook 'dafny-mode-hook)
        `(,#'blc-turn-off-electric-indent-local-mode
          ,#'blc-turn-off-flycheck-mode
          ,#'blc-turn-off-prettify-symbols-mode)))

(use-package dash
  :ensure t
  :defer
  :config
  (dash-enable-font-lock))

  :defer)

(use-package define-word
  :ensure t
  :bind ("C-c /" . define-word-at-point))

(use-package delight
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
   dired-listing-switches
   (string-join '("--almost-all"
                  "--classify"
                  "--group-directories-first"
                  "--human-readable"
                  "-l")
                " ")))

(use-package dired-x
  :after dired
  :bind (("C-x C-j"   . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  :config
  (mapc (-partial #'add-to-list 'dired-guess-shell-alist-user)
        '(("\\.pdf\\'"   "pdf"     )
          ("\\.docx?\\'" "lowriter"))))

(use-package disaster
  :ensure t
  :commands disaster
  :init
  (add-hook 'c-mode-common-hook #'blc-enable-disaster)
  :config
  (setq-default disaster-objdump "objdump -D -M att -Sl --no-show-raw-insn"))

;; FIXME: Add current project to `ebib-bib-search-dirs'
(use-package ebib
  :ensure t
  :bind ("C-c e" . ebib)
  :config
  (setq-default
   ebib-bibtex-dialect 'biblatex
   ebib-use-timestamp  t))

(use-package elisp-mode
  :defer
  :init
  (delight '((      emacs-lisp-mode "ελ" :major)
             (lisp-interaction-mode "λι" :major))))

(use-package engine-mode
  :ensure t
  :commands engine-mode engine/execute-search engine/get-query
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
  (add-hook 'eshell-mode-hook #'blc-turn-off-line-numbers))

(use-package ess
  :ensure t
  :defer
  :config
  (setq-default ess-default-style   'DEFAULT
                ess-indent-from-lhs nil))

(use-package exec-path-from-shell
  :ensure t
  :defer
  :config
  (mapc (-partial #'add-to-list 'exec-path-from-shell-variables)
        '("SSH_AGENT_PID" "SSH_AUTH_SOCK")))

(use-package expand-region
  :ensure t
  :bind ("M-+" . er/expand-region))

(use-package eyebrowse
  :ensure t
  :defer)

(use-package fic-mode
  :ensure t
  :defer
  :init
  (mapc (-rpartial #'add-hook #'fic-mode) blc-fundamental-hooks)
  :config
  (mapc (-partial #'add-to-list 'fic-highlighted-words)
        '("HACK" "KLUDGE" "NOTE" "WARN")))

(use-package figlet
  :ensure t
  :defer)

(use-package files
  :defer
  :init
  (add-hook 'after-save-hook
            #'executable-make-buffer-file-executable-if-script-p))

(use-package files
  :defer
  :init
  (setq-default
   backup-by-copying          t         ; Do not clobber symlinks
   backup-directory-alist               ; Backup/auto-save directory
   `(("." . "~/.backup/"))
   delete-old-versions        t
   directory-free-space-args  "-hP"
   kept-new-versions          4
   kept-old-versions          2
   mode-require-final-newline nil       ; Do not silently append EOF NL
   version-control            t))       ; Versioned backups

(use-package fill-column-indicator
  :ensure t
  :defer
  :init
  (mapc (-rpartial #'add-hook #'turn-on-fci-mode) blc-fundamental-hooks)
  (setq-default fci-rule-color  "#696969"
                fci-rule-column 80))

(use-package find-file
  :defer
  :init
  (add-hook 'find-file-hook #'blc-strip-large-buffer))

(use-package find-func
  :bind
  (("C-x F"   . find-function)
   ("C-x 4 F" . find-function-other-window)
   ("C-x 5 F" . find-function-other-frame)
   ("C-x K"   . find-function-on-key)
   ("C-x 4 K" . find-function-on-key-other-window)
   ("C-x 5 K" . find-function-on-key-other-frame)
   ("C-x V"   . find-variable)
   ("C-x 4 V" . find-variable-other-window)
   ("C-x 5 V" . find-variable-other-frame)))

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
  (blc-turn-off-modes #'blink-cursor-mode))

(use-package git-commit
  :ensure magit
  ;; Need to load package to know when to load package :(
  :mode ("/\\(?:\
\\(?:\\(?:COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\
\\|BRANCH_DESCRIPTION\\)\\'" . git-commit-mode)
  :config
  (setq-default git-commit-summary-max-length 50
                git-commit-fill-column        68)

  (add-hook 'with-editor-post-finish-hook #'blc-kill-git-commit-buffer)

  (global-git-commit-mode))

(use-package gitconfig-mode
  :ensure t
  :defer)

(use-package gitignore-mode
  :ensure t
  :defer)

(use-package gnus
  :defer
  :config
  (setq-default
   gnus-check-new-newsgroups nil
   gnus-select-method
   '(nnimap "")))

(use-package golden-ratio-scroll-screen
  :disabled
  :ensure t
  :bind (("M-," . golden-ratio-scroll-screen-down)
         ("M-." . golden-ratio-scroll-screen-up)))

(use-package haskell-cabal
  :ensure haskell-mode
  :defer
  :init
  (add-hook 'haskell-cabal-mode-hook #'blc-turn-off-electric-indent-local-mode))

(use-package haskell-mode
  :ensure t
  :defer)

(use-package hayoo
  :ensure t
  :defer)

;; TODO: Delight
(use-package helm
  :disabled
  :ensure t
  :bind ("C-c b" . helm-mini)
  :init
  (add-hook 'helm-major-mode-hook #'blc-turn-off-line-numbers)
  :config
  (setq-default
   helm-buffers-fuzzy-matching t
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
  :disabled
  :ensure t
  :after helm projectile
  :commands helm-projectile-on
  :config
  (helm-projectile-on))

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
   (blc-account-concat "expenses" '("housing" "groceries"))
   hledger-ratios-liquid-asset-accounts
   (blc-account-concat "assets"   '("boi" "cash"))))

(use-package i18next-wrap
  :load-path "lisp"
  :bind ("C-c C-i" . i18next-query-replace))

(use-package ido
  :defer
  :init
  (setq-default ido-enable-flex-matching t))

(use-package idris-mode
  :ensure t
  :defer)

(use-package "indent"
  :defer
  :init
  (setq-default indent-line-function #'insert-tab))

(use-package info
  :defer
  :init
  (add-hook 'Info-mode-hook #'blc-turn-off-line-numbers))

(use-package "isearch"
  :defer
  :init
  (setq-default isearch-allow-scroll t)
  (add-hook 'isearch-mode-hook #'blc-delight-isearch))

(use-package isearch+
  :disabled
  :ensure t)

(use-package isearch-prop
  :ensure t
  :defer)

(use-package ivy
  :ensure t
  :delight ivy-mode
  :commands ivy-set-sources
  :bind (("C-x b"   . ivy-switch-buffer)
         ("C-x 4 b" . ivy-switch-buffer-other-window)
         ("C-c C-r" . ivy-resume))
  :config
  (setq-default
   ivy-format-function     'ivy-format-function-arrow
   ivy-use-virtual-buffers t)

  (ivy-mode))

(use-package ivy-hydra
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
   js-enabled-frameworks
   (-intersection '(dojo javascript prototype) js-enabled-frameworks)
   js-indent-level         4
   js-switch-indent-offset 4))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node" "nodejs"
  :commands js2-line-break
  :init
  (setq-default js2-bounce-indent-p t)

  (mapc (-partial #'add-hook 'js2-mode-hook)
        `(,#'js2-highlight-unused-variables-mode
          ,#'blc-turn-off-electric-indent-local-mode))

  :config
  (setq-default
   js2-allow-rhino-new-expr-initializer nil
   js2-concat-multiline-strings         'eol
   js2-global-externs                   '("define" "location")
   js2-highlight-level                  3
   js2-include-node-externs             t
   js2-mode-assume-strict               t
   js2-skip-preprocessor-directives     t)

  (bind-key "RET" #'js2-line-break js2-mode-map))

  ;; ;; FIXME
  ;; (mapc (-applify #'set-face-foreground)
  ;;       '((js2-error             "#ff0000")
  ;;         (js2-external-variable "#ff0000")
  ;;         (js2-function-param    "#5fd7af")))

  ;; (defun blc-js2-moar-colour ()
  ;;   "Further customise `js2-mode' faces."
  ;;   (interactive)
  ;;   (mapc (-applify #'set-face-foreground)
  ;;         '((js2-function-call   "#fce94f")
  ;;           (js2-object-property "#fcaf3e")))))

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
  (unbind-key "C-c C-g" js3-mode-map)        ; Why...
  (blc-trim-before-newline #'js3-enter-key)  ; For comments

  (setq-default
   js3-auto-indent-p                         t
   js3-consistent-level-indent-inner-bracket t
   js3-enter-indents-newline                 t
   js3-global-externs
   '("console" "define" "document" "location" "require" "window")
   js3-include-browser-externs               nil
   js3-include-gears-externs                 nil
   js3-include-rhino-externs                 nil
   js3-indent-dots                           t
   js3-indent-level                          4
   js3-indent-on-enter-key                   t
   js3-skip-preprocessor-directives          t)

  ;; FIXME
  (mapc (-applify #'set-face-foreground)
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
  (add-hook 'lisp-mode-hook #'blc-turn-off-electric-indent-local-mode))

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
  (delight                              ; Tidy?
   '((magit-blame-mode-lighter  "± Bl"  magit-blame)
     (magit-cherry-mode         "± Ch"       :major)
     (magit-diff-mode           "± Df"       :major)
     (magit-log-mode            "± Lg"       :major)
     (magit-log-select-mode     "± Ls"       :major)
     (magit-merge-preview-mode  "± Mg"       :major)
     (magit-mode                "±"          :major)
     (magit-process-mode        "± Pr"       :major)
     (magit-rebase-mode         "± Rb"       :major)
     (magit-reflog-mode         "± Rfl"      :major)
     (magit-refs-mode           "± Rf"       :major)
     (magit-repolist-mode       "± Rp"       :major)
     (magit-revision-mode       "± Rv"       :major)
     (magit-stash-mode          "± St"       :major)
     (magit-stashes-mode        "± Sts"      :major)
     (magit-status-mode         "±"          :major)
     (magit-submodule-list-mode "± Md"       :major)))

  (global-magit-file-mode)
  (magit-wip-after-apply-mode)
  (magit-wip-after-save-mode)
  (magit-wip-before-change-mode)

  (add-to-list 'magit-rebase-arguments "--interactive")

  (setq-default magit-repository-directories `((,blc-repos-dir . 2)))

  (let* (;; Limit number of commits in log
         (logcommits       "32")
         (logre            "-n\\([[:digit:]]+\\)")
         (logargs          'magit-log-arguments)
         ;; Align refs with wider columns
         (fmtwidth         "-40")
         (fmtflags         '(?n ?U))
         (fmtre            (format "%%\\([+-]??[[:digit:]]*?\\)[%s]"
                                   (apply #'string fmtflags)))
         (case-fold-search nil))

    (set-default logargs
                 (blc-tree-sed logre logcommits (symbol-value logargs) t t 1))

    (mapc #'(lambda (fmt)
              (set-default fmt (replace-regexp-in-string
                                fmtre fmtwidth (symbol-value fmt) t t 1)))
          '(magit-refs-local-branch-format
            magit-refs-remote-branch-format
            magit-refs-symref-format
            magit-refs-tags-format))))

  ;; ;; FIXME: modify tango-dark
  ;; (set-face-attribute
  ;;  'magit-blame-heading nil
  ;;  :background "#696969"
  ;;  :foreground "#ffffff")
  ;; (set-face-attribute
  ;;  'magit-header-line nil
  ;;  :inherit    'magit-section-heading
  ;;  :background (internal-get-lisp-face-attribute 'default :background))
  ;; (mapc (-applify #'set-face-foregrounds)
  ;;       '((magit-dimmed "#808080")
  ;;         (magit-hash   "#808080"))))

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
  (add-hook 'Man-mode-hook #'blc-man-fontify))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'" "\\.markdown\\'"
  :commands markdown-cycle markdown-enter-key
  :config
  (bind-key "TAB" #'markdown-cycle markdown-mode-map)
  (blc-trim-before-newline #'markdown-enter-key))

(use-package minibuffer
  :defer
  :init
  (mapc (-applify #'add-hook)
        `((minibuffer-setup-hook ,#'blc-increase-gc-thresh)
          (minibuffer-exit-hook  ,#'blc-restore-gc-thresh ))))

(use-package minimap
  :ensure t
  :defer
  :config
  (setq-default
   minimap-highlight-line  nil
   minimap-recenter-type   'relative
   minimap-width-fraction  0.05
   minimap-window-location 'right)
  (set-face-background 'minimap-active-region-background "#696969")
  (set-face-attribute  'minimap-font-face nil :font "DejaVu Sans Mono 1"))

(use-package mustache-mode
  :ensure t
  :defer)

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

;; FIXME: Load with first graphical frame
(use-package palette
  :ensure t
  :defer)

(use-package "paragraphs"
  :defer
  :init
  (setq-default sentence-end-double-space nil))

(use-package paren
  :defer
  :init
  (show-paren-mode))

(use-package paren-face
  :ensure t
  :defer
  :init
  (global-paren-face-mode))

(use-package pascal
  :defer
  :init
  (add-hook 'pascal-mode-hook #'blc-use-c++-comments))

(use-package pass
  :ensure t
  :defer)

(use-package pcre2el
  :ensure t
  :defer)

;; FIXME: Load with first graphical display
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

;; TODO: Delight
(use-package projectile
  :ensure t
  :defer
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  :config
  (setq-default
   projectile-completion-system           'ivy
   projectile-find-dir-includes-top-level t)

  ;; Delight mode but not project name
  (let ((var 'projectile-mode-line)
        (nom "Projectile")
        (dim ""))
    (set-default var (blc-tree-sed nom dim (symbol-value var))))

  (projectile-mode))

(use-package prolog
  :mode ("\\.pl\\'" . prolog-mode)
  :config
  (setq-default prolog-system 'swi))

(use-package python
  :defer
  :config
  (when-let ((cmds '("epylint3" "epylint" "pyflakes"))
             (cmd  (-some #'executable-find cmds)))
    (setq-default python-check-command cmd))

  (setq-default python-shell-interpreter "ipython3"))

(use-package recentf
  :defer
  :init
  (add-hook 'ivy-mode-hook #'recentf-mode))

(use-package remember
  :bind (("<f7>" . remember-notes)
         ("<f8>" . remember-notes-save-and-bury-buffer))
  :config
  (setq-default remember-notes-initial-major-mode #'org-mode))

(use-package sass-mode
  :ensure t
  :defer
  :init
  (add-hook 'sass-mode-hook #'blc-use-c++-comments))

(use-package saveplace
  :defer
  :init
  (save-place-mode))

;; FIXME: Disable with every graphical frame
(use-package scroll-bar
  :defer
  :init
  (blc-turn-off-modes #'toggle-scroll-bar))

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
  :bind
  (("M-c"     . capitalize-dwim)
   ("M-l"     .   downcase-dwim)
   ("M-u"     .     upcase-dwim)
   ("M-\\"    .   cycle-spacing)
   ("C-x C-k" . kill-whole-line))

  :init
  (add-hook 'special-mode-hook #'blc-turn-off-line-numbers)
  (with-current-buffer (messages-buffer)
    (blc-turn-off-line-numbers))

  (column-number-mode))

(use-package sl
  :ensure t
  :defer)

(use-package smerge-mode
  :defer
  :init
  (add-hook 'find-file-hook #'blc-sniff-smerge t))


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

(use-package "startup"
  :defer
  :init
  (setq-default inhibit-startup-screen t)
  (add-hook 'after-init-hook #'blc-report-init-time t))

(use-package subword
  :defer
  :delight subword-mode
  :init
  (global-subword-mode))

(use-package swiper
  :ensure t
  :defer)

(use-package tex
  :ensure auctex
  :commands TeX-doc TeX-revert-document-buffer
  :defines LaTeX-clean-intermediate-suffixes
  :config
  (setq-default
   LaTeX-csquotes-open-quote  "\\enquote{"
   LaTeX-csquotes-close-quote "}"
   TeX-auto-save              t
   TeX-parse-self             t
   TeX-PDF-mode               t)

  (bind-key "C-c ?" #'TeX-doc TeX-mode-map)

  ;; Set priority of pre-configured PDF viewers
  (when-let ((priority '("PDF Tools" "Zathura"))
             (viewers  TeX-view-program-list-builtin)
             (viewer   (-first (-rpartial #'assoc-string viewers) priority)))
    (push `(output-pdf ,viewer) TeX-view-program-selection))

  (mapc
   (-applify #'add-hook)
   `((LaTeX-mode-hook                          ,#'blc-setup-latexmk         )
     (LaTeX-mode-hook                          ,#'turn-on-auto-fill         )
     (TeX-after-compilation-finished-functions ,#'TeX-revert-document-buffer))))

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

;; FIXME: Disable with first graphical frame
(use-package tool-bar
  :when (display-graphic-p)
  :defer
  :init
  (blc-turn-off-modes #'tool-bar-mode))

(use-package top-mode
  :ensure t
  :defer)

(use-package uniquify
  :defer
  :init
  (setq-default uniquify-buffer-name-style 'forward))

(use-package use-package
  :commands blc-debug-use-package use-package-autoload-keymap
  :config
  (defun blc-debug-use-package ()
    "Enable `use-package' debugging."
    (interactive)
    (setq-default use-package-debug t)))

(use-package vc-hooks
  :defer
  :init
  ;; Magit-only
  (setq-default vc-handled-backends nil))

(use-package visual-regexp-steroids
  :ensure t
  :after pcre2el
  :defer
  :config
  (setq-default vr/match-separator-use-custom-face t))

(use-package vlf
  :ensure t
  :defer)

(use-package w3m
  :ensure t
  :defer)

(use-package wc-mode
  :ensure t
  :commands wc-mode
  :config
  (setq-default wc-modeline-format "[%tll]"))

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.mustache\\'"))

(use-package wgrep
  :ensure t
  :defer)

(use-package whitespace
  :defer
  :delight global-whitespace-mode
  :init
  (setq-default whitespace-style '(face tabs trailing empty tab-mark))
  (global-whitespace-mode))

(use-package windmove
  :bind
  (("S-<up>"      . windmove-up   )
   ("S-<down>"    . windmove-down )
   ("S-<left>"    . windmove-left )
   ("S-<right>"   . windmove-right)
   ;; Mutatis mutandis within tmux
   ("M-[ 1 ; 2 A" . windmove-up   )
   ("M-[ 1 ; 2 B" . windmove-down )
   ("M-[ 1 ; 2 D" . windmove-left )
   ("M-[ 1 ; 2 C" . windmove-right))
  :config
  (setq-default
   windmove-window-distance-delta 2
   windmove-wrap-around           t))

(use-package "window"
  :defer
  :init
  (setq-default
   scroll-error-top-bottom t
   split-window-keep-point nil)
  (advice-add #'split-window-sensibly
              :around #'blc-split-larger-dimension--advice))

(use-package winner
  :defer
  :init
  (winner-mode))

(use-package woman
  :defer
  :init
  (add-hook 'woman-mode-hook #'blc-woman-fontify))

(use-package wrap-region
  :ensure t
  :delight wrap-region-mode
  :commands wrap-region-add-wrapper
  :init
  (wrap-region-global-mode)
  :config
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
   (-map (-rpartial #'string-join ", ")
         '(("Athens"     "Greece"  )
           ("Avoca"      "Ireland" )
           ("Dublin"     "Ireland" )
           ("Kfar Qasim" "Israel"  )
           ("Harare"     "Zimbabwe")
           (             "Moon"    )))))

(use-package xref-js2
  :ensure t
  :defer
  :init
  (add-hook 'js2-mode-hook #'blc-turn-on-xref-js2))

;; FIXME: Enable with first terminal frame
(use-package xt-mouse
  :unless (display-graphic-p)
  :defer
  :init
  (xterm-mouse-mode))

(use-package yaml-mode
  :ensure t
  :defer)

;;; init.el ends here
