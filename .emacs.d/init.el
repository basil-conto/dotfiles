;;; init.el --- init file for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://github.com/basil-conto/dotfiles

;;; Code:


;;;; BOOTSTRAPPING

;;; Performance & dependencies

(defalias 'blc-gc-thresh-restore
  (let ((thresh gc-cons-threshold))
    (lambda () (setq gc-cons-threshold thresh)))
  "Restore default `gc-cons-threshold' value.
See URL `http://bling.github.io/blog/2016/01/18/\
why-are-you-changing-gc-cons-threshold/'.")

(eval-and-compile
  (defun blc-gc-thresh-maximise ()
    "Increase GC threshold to at least 64MiB"
    (setq gc-cons-threshold (max (ash 1 26) gc-cons-threshold)))

  (blc-gc-thresh-maximise)

  ;; Include user lisp libraries
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

;; User
(require 'blc-lib)
(eval-when-compile
  (require 'blc-macs))

;; Built-in
(require 'map)
(require 'package)
(require 'seq)
(eval-when-compile
  (require 'subr-x))

;;; Profiling

(defalias 'blc-report-init-time
  (let ((file load-file-name))
    (lambda ()
      (thread-last (float-time (time-subtract after-init-time before-init-time))
        (message "Loading %s...done (%.3fs)" file))))
  "`use-package'-style `emacs-init-time' to 3 decimal places.")

;;; Packaging

(eval-and-compile
  ;; Sandbox this nuisance
  (advice-add #'package--save-selected-packages :override #'ignore)

  (setq-default package-menu-hide-low-priority t)

  ;; Archives
  (seq-do-indexed (pcase-lambda (`(,id . ,url) i)
                    (blc-aput 'package-archives           id url)
                    (blc-aput 'package-archive-priorities id (1+ i)))
                  '(("melpa" . "https://melpa.org/packages/")
                    ("org"   . "http://orgmode.org/elpa/")))

  ;; Locate and activate packages
  (package-initialize)

  ;; Third-party dependencies
  (when-let (missing (seq-remove #'package-installed-p
                                 '(bind-key
                                   dash-functional
                                   delight
                                   use-package
                                   zenburn-theme)))
    (when (y-or-n-p (format "Install missing packages %s?" missing))
      (mapc #'package-install missing))))

(require 'bind-key)
(eval-when-compile
  (setq-default use-package-always-defer t
                use-package-verbose      'debug)
  (require 'dash-functional)
  (require 'use-package))


;;;; DEFINITIONS

;;; Byte-compiler declarations

(blc-declare-vars
  Info-standalone
  LaTeX-clean-intermediate-suffixes
  bbdb-mua-summary-unify-format-letter
  c-mode-base-map
  doc-view-resolution
  ffap-alist
  ffap-file-finder
  ghc-doc-hackage-format
  ivy-format-function
  ivy-height
  js2-mode-map
  org-default-notes-file
  org-directory
  recentf-list
  tile-cycler)

(eval-when-compile
  (add-to-list (defvar eieio--known-slot-names ()) 'current-strategy))

(blc-declare-fns
  (cc-cmds     c-toggle-comment-style)
  (cc-defs     c-langelem-pos)
  (csv-mode    csv-align-fields)
  (doc-view    doc-view-start-process)
  (esh-mode    eshell-truncate-buffer)
  (eww         eww-copy-page-url
               eww-html-p)
  (hi-lock     hi-lock-set-pattern)
  (ibuf-ext    ibuffer-switch-to-saved-filter-groups)
  (ibuffer     ibuffer-current-buffer)
  (man         Man-goto-section)
  (mailcap     mailcap-extension-to-mime)
  (message     message-field-value
               message-make-from
               message-replace-header
               message-user-mail-address)
  (org         org-goto)
  (term        term-char-mode
               term-in-char-mode
               term-line-mode)
  (tile        tile-get-name))

(blc-autoloads
  (gnus     gnus-find-subscribed-addresses)
  (ibuf-ext ibuffer-pop-filter
            ibuffer-push-filter)
  (ibuffer  ibuffer-buf-matches-predicates
            ibuffer-update)
  (ielm     inferior-emacs-lisp-mode))

;;; Variables

(defvar blc-bib-file "~/.bib.bib"
  "Default user BibTeX file.")

(defvar blc-fundamental-hooks
  (mapcar (-cut blc-symcat <> "-mode-hook")
          '(conf ess haskell-cabal hledger mustache prog text))
  "Hooks whose modes derive from `fundamental-mode' or nothing.")

;;; Advice

(define-advice bibtex-completion-format-entry
    (:around (fmt entry width) blc-narrow)
  "Decrease `ivy-bibtex' entry width due to other formatting.
`ivy-bibtex-default-action' only considers `frame-width', which
does not, for example, take the effect of `ivy-format-function'
into account."
  (funcall fmt entry
           (blc-but-fringes
            width (string-width (funcall ivy-format-function '(""))))))

(define-advice c-lineup-arglist (:before-until (langelem) blc-c++-lambda-indent)
  "Return indentation offset for C++11 lambda arguments.
Currently keeps offset unchanged by returning 0 for lambdas
opened as arguments and nil for everything else.
Adapted from URL `http://stackoverflow.com/a/23553882'."
  (and (derived-mode-p #'c++-mode)
       (ignore-errors
         (save-excursion
           (goto-char (c-langelem-pos langelem))
           ;; Detect "[...](" or "[...]{",
           ;; preceded by "," or "(" and with unclosed brace
           (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$")))
       0))

(define-advice eshell-pcomplete (:override (&rest _) blc-completion-at-point)
  "Use default inline completion."
  (completion-at-point))

(define-advice ledger-pcomplete (:override (&rest _) blc-completion-at-point)
  "Use default inline completion."
  (completion-at-point))

(define-advice makefile-insert-gmake-function
    (:after (&rest _) blc-delete-trailing-space)
  "Delete trailing whitespace after function call insertion."
  (delete-horizontal-space t))

(define-advice package-install (:before (&rest _) blc-async-bytecomp)
  "Install `async' and enable `async-bytecomp-package-mode'."
  (use-package async
    :ensure
    :init
    (setq-default async-bytecomp-allowed-packages '(all))
    (async-bytecomp-package-mode)))

(define-advice recentf-save-list (:around (save &rest args) blc-save-safely)
  "Save silently only if sole Emacs instance."
  (when (> 2 (seq-count (lambda (pid)
                          (string-match-p
                           "emacs" (alist-get 'comm (process-attributes pid))))
                        (list-system-processes)))
    (let ((save-silently t))
      (apply save args))))

(define-advice turn-on-hi-lock-if-enabled (:before () blc-exclude-derived-modes)
  "Exempt derived modes from hi-lock highlighting.
Include every major mode derived from the current
`hi-lock-exclude-modes' in that blacklist."
  (let ((modes 'hi-lock-exclude-modes))
    (when (apply #'derived-mode-p (symbol-value modes))
      (add-to-list modes major-mode))))


;;; Package utilities

(defun blc-bbdb-set-gnus-summary-line-format ()
  "Prepare `gnus-summary-line-format' for `bbdb' unification."
  (setq-default
   gnus-summary-line-format
   (blc-gnus-summary-line-format "u" bbdb-mua-summary-unify-format-letter)))

(defun blc-print-url--lpr (url &rest _)
  "Asynchronously print URL using `lpr-command'.
This function is written with a print command like `hp-print' in
mind, which is passed `lpr-switches' and URL as arguments."
  (interactive "fPrint file: ")
  (make-process :name            "LPR Print"
                :command         `(,lpr-command ,@lpr-switches ,url)
                :connection-type 'pipe))

(defun blc-print-url--webkit (url &rest _)
  "Print URL using `wkhtmltopdf'.
The contents of URL are converted to a temporary PDF file by
`wkhtmltopdf' before printing the result with
`blc-print-url--lpr'. "
  (let ((name "WebKit Print")
        (temp (make-temp-file "blc-" nil ".pdf")))
    (make-process
     :name            name
     :command         `("wkhtmltopdf" ,url ,temp)
     :connection-type 'pipe
     :sentinel
     (lambda (proc event)
       (if (blc-process-success-p proc)
           (blc-print-url--lpr temp)
         (lwarn 'blc :error "%s: %s" name event))))))

(defun blc-print-url--selector (url)
  "Return cons cell with appropriate printer and filter for URL."
  (require 'eww)
  (require 'mailcap)
  (if-let ((mimetype (mailcap-extension-to-mime (url-file-extension url)))
           (remote   (url-handler-file-remote-p url)))
      (if (string-match-p
           (regexp-opt '("application/pdf" "application/postscript")) mimetype)
          `(,#'blc-print-url--lpr . ,#'url-file-local-copy)
        `(,#'blc-print-url--webkit . ,#'identity))
    (if (eww-html-p mimetype)
        `(,#'blc-print-url--webkit . ,#'identity)
      `(,#'blc-print-url--lpr
        . ,(lambda (url)
             (url-filename (url-generic-parse-url url)))))))

(defun blc-print-url (url &rest args)
  "Print contents of URL.
See `browse-url' for an explanation of the arguments."
  (pcase-let ((`(,browser . ,filter) (blc-print-url--selector url)))
    (apply browser (funcall filter url) args)))

(function-put #'blc-print-url 'interactive-form (interactive-form #'browse-url))

(defvar blc-hackage-url-format
  "https://hackage.haskell.org/package/%s/docs/%s.html"
  "URL format string for Hackage packages.
The two format specifiers correspond to the package and module
names, respectively.")

(defun blc-browse-url-ghc-doc (url &rest args)
  "Pass latest version of Hackage package URL to `browse-url'."
  (let ((matches (thread-first
                     (blc-sed "%s" "\\(.+\\)" ghc-doc-hackage-format t t)
                   (blc-matches url 1 3))))
    (apply #'browse-url (apply #'format blc-hackage-url-format matches) args)))

(defun blc-browse-url-irfc (url &rest _)
  "Visit RFC URL via `irfc-visit'.
URL is parsed using the regular expressions found in
`auto-mode-alist' and `ffap-alist' for `irfc-mode' and
`ffap-rfc', respectively."
  (require 'ffap)
  (if-let (res (blc-keep (lambda (cell)
                           (when-let (re (car cell))
                             `(regexp ,re)))
                         (map-apply #'rassq `((irfc-mode . ,auto-mode-alist)
                                              (ffap-rfc  . ,ffap-alist)))))
      (pcase (blc-matches (blc-rx `(| ,@res)) (url-file-nondirectory url) 1)
        (`(,num) (irfc-visit (string-to-number num)))
        (_       (user-error "Invalid RFC URL: %s" url)))
    (user-error "Regexp not found for RFC URL: %s" url)))

(defvar blc-browser-alist
  `(("EWW"                . ,#'eww-browse-url       )
    ("Firefox"            . ,#'browse-url-firefox   )
    ("Print"              . ,#'blc-print-url        )
    ("Emacs IRFC"         . ,#'blc-browse-url-irfc  )
    ("XDG"                . ,#'browse-url-xdg-open  )
    ("Chromium"           . ,#'browse-url-chromium  )
    ("Elinks"             . ,#'browse-url-elinks    )
    ("Xterm text browser" . ,#'browse-url-text-xterm)
    ("Emacs text browser" . ,#'browse-url-text-emacs))
  "Map preferred browsers to their calling function.")

(defun blc-browse-url (url &rest args)
  "Read WWW browser name to open URL with completion.
See `blc-browser-alist' for known browsers and `browse-url' for a
description of the arguments to this function."
  (let* ((prompt-fmt (if (string-blank-p url)
                         "Open browser: "
                       "Open URL `%s' in: "))
         (prompt-url (url-truncate-url-for-viewing url (ash (frame-width) -1)))
         (prompt     (blc-sed "%" "%%" (format prompt-fmt prompt-url) t t)))
    (when-let (browser
               (blc-aget blc-browser-alist
                         (completing-read prompt blc-browser-alist nil t)))
      (apply browser url args))))

(function-put
 #'blc-browse-url 'interactive-form (interactive-form #'browse-url))

(defun blc-system-tz ()
  "Return contents of `/etc/timezone' or nil."
  (blc-with-contents "/etc/timezone"
    (and (blc-search-forward (rx (group (+ nonl)) (? ?\n) eos))
         (match-string-no-properties 1))))

(defun blc-system-location ()
  "Return location of `blc-system-tz' or nil."
  (when-let (tz (blc-system-tz))
    (cadr (split-string tz "/" t))))

(defun blc-solar-set-location (&optional location)
  "Reconcile solar calendar with LOCATION from `blc-locations'."
  (interactive `(,(completing-read "Location: " blc-locations nil t nil ()
                                   (blc-system-location))))
  (pcase (blc-aget blc-locations location)
    ((plist :country country :lat lat :long long)
     (setq-default calendar-latitude      lat
                   calendar-longitude     long
                   calendar-location-name (format "%s, %s" location country)))))

(defun blc-turn-on-c++-comments ()
  "Default to C++-style line comments."
  (if (bound-and-true-p c-buffer-is-cc-mode)
      (c-toggle-comment-style -1)
    (setq comment-start "//"
          comment-end   "")))

(defun blc-csv-align-all-fields ()
  "Align all fields in the current CSV buffer."
  (csv-align-fields nil (point-min) (point-max)))

(defun blc-doc-view-pdf-to-png (pdf png page callback)
  "MuPDF-backed PDF to PNG converter function for DocView."
  (doc-view-start-process
   "pdf->png"
   "mutool"
   `("draw"
     "-o" ,png
     "-r" ,(number-to-string (round doc-view-resolution))
     ,pdf
     ,@(and page `(,(number-to-string page))))
   callback))

(defun blc-eww-increase-readability ()
  "Adjust font for increased readability."
  (setq line-spacing (window-font-height))
  (text-scale-set 1))

(defun blc-eww-bookmark-save ()
  "Copy the URL of the current bookmark into the kill ring."
  (interactive)
  (if-let (eww-data (get-text-property (line-beginning-position) 'eww-bookmark))
      (eww-copy-page-url)
    (user-error "No bookmark on the current line")))

(defun blc-turn-off-flycheck (&rest _)
  "Disable `flycheck-mode'."
  (interactive)
  (blc-turn-off #'flycheck-mode))

(defun blc-kill-git-buffer ()
  "Kill current git commit message or rebase todo list buffer."
  (when-let (re (cond ((bound-and-true-p git-commit-mode)
                       git-commit-filename-regexp)
                      ((derived-mode-p #'git-rebase-mode)
                       git-rebase-filename-regexp)))
    (and buffer-file-name
         (string-match-p re buffer-file-name)
         (kill-buffer))))

(defun blc-git-commit-set-fill-column ()
  "Set local `fill-column' for `git-commit-mode' buffers."
  ;; Benefit over setq: displays debugging message
  (set-fill-column 68))

(defun blc-hi-lock-no-eof-nl ()
  "Highlight missing trailing EOF newlines."
  (hi-lock-set-pattern "^.+\\'" 'hi-red-b))

(defun blc-hledger-accounts (category &optional accounts acct-sep list-sep)
  "Join ACCOUNTS with their account CATEGORY.
Return a LIST-SEP-delimited (default \" \") string of account
prefixed by CATEGORY and ACCT-SEP (default \":\")."
  (let ((list-sep (or list-sep " "))
        (acct-sep (or acct-sep ":")))
    (thread-first (lambda (account)
                    (string-join `(,category ,account) acct-sep))
      (mapconcat accounts list-sep))))

(defun blc-ibuffer-ffap ()
  "Like `ibuffer-find-file', but backed by `ffap-file-finder'."
  (interactive)
  (require 'ffap)
  (let* ((buffer            (ibuffer-current-buffer))
         (buffer            (if (buffer-live-p buffer) buffer (current-buffer)))
         (default-directory (buffer-local-value 'default-directory buffer)))
    (call-interactively ffap-file-finder)))

(defvar blc-ibuffer-default-group "default"
  "Name of default saved ibuffer filter group.")

(defun blc-turn-on-ibuffer-filter-groups ()
  "Enable default ibuffer filter groups.
See `blc-ibuffer-default-group'."
  (ibuffer-switch-to-saved-filter-groups blc-ibuffer-default-group))

(defun blc-ielm-other-window ()
  "Call `ielm' in another window."
  (interactive)
  (let ((display-buffer-alist `(("" () (inhibit-same-window . t)))))
    (call-interactively #'ielm)))

(defun blc-info-read-buffer ()
  "Read the name, file and node of an Info buffer.
Return the name of the buffer as a string or `nil'."
  (let ((bufs (blc-keep
               (lambda (buf)
                 (with-current-buffer buf
                   (when (derived-mode-p #'Info-mode)
                     (let ((name (buffer-name)))
                       `(,(concat name (substring-no-properties
                                        (cadr mode-line-buffer-identification)))
                         . ,name)))))
               (buffer-list))))
    (if (cdr bufs)
        (blc-aget bufs (completing-read "Info buffer: " bufs))
      (cdar bufs))))

(defun blc-info (&optional buffer)
  "Call `info' on interactively completed BUFFER."
  (interactive `(,(blc-info-read-buffer)))
  (info nil buffer))

(defun blc-info-other-window (&optional buffer)
  "Call `info-other-window' on interactively completed BUFFER."
  (interactive `(,(blc-info-read-buffer)))
  (info-other-window nil buffer))

(defun blc-info-kill ()
  "Quit Info and kill its buffer."
  (interactive)
  (if Info-standalone
      (save-buffers-kill-emacs)
    (quit-window t)))

(defun blc-isearch-delight ()
  "Shorten lighter of `isearch-mode'."
  (setq isearch-mode "🔍"))

(defun blc-ivy-recentf (&optional count)
  "Return first COUNT or `ivy-height'/2 items in `recentf-list'."
  (seq-take recentf-list (or count (ash ivy-height -1))))

(defun blc-ledger-frame-width ()
  "Return available `frame-width' as a string."
  (number-to-string (blc-but-fringes (frame-width))))

(defun blc-msmtp-addresses ()
  "Return list of unique addresses in ~/.msmtprc."
  (blc-with-contents "~/.msmtprc"
    (let (addresses)
      (while (blc-search-forward
              (rx bol "account" (+ space) (group (+ (not space))) eol))
        (when (blc-search-forward
               (rx bol "from" (+ space)
                   (group (+ (not space)) ?@ (+ (not space))
                          ?. (+ (not space))) eol))
          (push (match-string-no-properties 1) addresses)))
      (nreverse addresses))))

(defun blc-message-set-msmtp-from ()
  "Replace From header with address read from ~/.msmtprc."
  (interactive)
  (thread-last (completing-read "From address: " (blc-msmtp-addresses)
                                nil t nil nil user-mail-address)
    (message-make-from nil)
    (message-replace-header "From")))

(defun blc-org-cycle ()
  "Call a prefixed `org-cycle'.
Without the prefix, visibility cycling in `outline-minor-mode'
and `orgstruct-mode' never seems to enter the SUBTREE state."
  (interactive)
  (org-cycle t))

(defun blc-org-read-file ()
  "Read `org' filename.
Defaults to `org-directory' and `org-default-notes-file'."
  ;; DEFAULT-FILENAME argument doesn't work with ivy
  (read-file-name "Org file: " org-directory nil nil
                  (file-name-nondirectory org-default-notes-file)))

(defun blc-org-find-file (&optional file)
  "Like `find-file', but defaults to `org-directory' files."
  (interactive `(,(blc-org-read-file)))
  (find-file file))

(defun blc-org-find-file-other-window (&optional file)
  "Like `blc-org-find-file', but opens another window."
  (interactive `(,(blc-org-read-file)))
  (find-file-other-window file))

(defun blc-toggle-subterm-mode ()
  "Toggle between `term-char-mode' and `term-line-mode'."
  (interactive)
  (require 'term)
  (if (term-in-char-mode)
      (term-line-mode)
    (term-char-mode)))

(defun blc-configure-beamer ()
  "Configure LaTeX Beamer intermediate suffixes."
  (add-to-list 'LaTeX-clean-intermediate-suffixes "\\.vrb"))

(defun blc-configure-latexmk ()
  "Configure Latexmk commands and intermediate suffixes."
  (add-to-list 'LaTeX-clean-intermediate-suffixes "\\.fdb_latexmk")

  (let* ((exe "latexmk")
         (nom (setq-default TeX-command-default (capitalize exe))))

    (dolist (pvc '(nil t))
      (let* ((nom (format "%s%s"     nom (if pvc " PVC" "")))
             (cmd (format "%s%s %%t" exe (if pvc "-pvc -view=none" "")))
             (dsc (format "Run %s"   nom)))
        (blc-aput 'TeX-command-list nom     ; Command name
                  `(,cmd                    ; Non-expanded shell command
                    TeX-run-command         ; Process handler
                    nil                     ; Confirm expanded shell command
                    (latex-mode LaTeX-mode) ; Applicable modes
                    :help ,dsc))))))        ; Command

(defun blc-tile (&optional select)
  "Tile windows with `tile' and report new strategy.
With prefix argument SELECT, call `tile-select' instead."
  (interactive "P")
  (funcall (if select #'tile-select #'tile))
  (message "%s" (tile-get-name (eieio-oref tile-cycler 'current-strategy))))


;;; Theme utilities

(defun blc-google-contacts-fontify ()
  "Customise `google-contacts-mode' faces."
  (map-do #'face-remap-add-relative
          '((google-contacts-familyname . font-lock-keyword-face)
            (google-contacts-givenname  . font-lock-keyword-face)
            (google-contacts-header     . font-lock-string-face ))))

(defun blc-message-header-fontify ()
  "Customise `message-mode' header faces."
  (face-remap-add-relative 'message-header-name 'font-lock-builtin-face))

(defun blc-sx-question-list-fontify ()
  "Customise `sx-question-list-mode' title faces."
  (map-do (-cut set-face-attribute <> nil :inherit <> :underline nil)
          '((sx-question-list-read-question   . link-visited)
            (sx-question-list-unread-question . link        ))))


;;;; MISCELLANEA

;;; Custom theme

(eval-and-compile
  (when (load-theme 'zenburn t)

    (defun blc-zenburn-assoc (colour)
      "Return the `zenburn' value associated with COLOUR."
      (cdr (assoc-string colour zenburn-default-colors-alist)))

    (defun blc-zenburn-brighten-fci ()
      "Make FCI rule lighter than background under `zenburn'."
      (setq-default fci-rule-color (blc-zenburn-assoc 'zenburn-bg+1)))

    (defun blc-zenburn-darken-ivy ()
      "Darken background of `ivy' matches under `zenburn'."
      (seq-mapn #'set-face-background
                ()
                ;; (cdr (bound-and-true-p ivy-minibuffer-faces))
                (mapcar #'blc-zenburn-assoc
                        '(zenburn-red-4 zenburn-blue-4 zenburn-green-1))))

    (defun blc-zenburn-fontify-org-todo ()
      "Customise `org-todo-keyword-faces' under `zenburn'."
      (setq-default org-todo-keyword-faces
                    (map-apply (lambda (kw colour)
                                 `(,kw . ,(blc-zenburn-assoc colour)))
                               '(("NEXT" . zenburn-magenta)
                                 ("EXEC" . zenburn-orange )
                                 ("MEET" . zenburn-yellow )
                                 ("WAIT" . zenburn-cyan   )
                                 ("BALK" . zenburn-fg     )
                                 ("VOID" . zenburn-blue-2 )))))

    (set-face-background 'highlight   (blc-zenburn-assoc 'zenburn-bg-1))
    (set-face-foreground 'line-number (blc-zenburn-assoc 'zenburn-bg+3))

    (map-do #'add-hook
            `((fci-mode-hook . ,#'blc-zenburn-brighten-fci    )
              (ivy-mode-hook . ,#'blc-zenburn-darken-ivy      )
              (org-load-hook . ,#'blc-zenburn-fontify-org-todo)))))

;; Maximise initial frame
(map-put initial-frame-alist 'fullscreen 'maximized)

;; Disable menu and tool bars
(mapc (-cut map-put default-frame-alist <> 0)
      '(menu-bar-lines tool-bar-lines))

;; Set default font under X
(map-put (map-elt window-system-default-frame-alist 'x)
         'font "DejaVu Sans Mono-8")

;; Ask short questions
(advice-add #'yes-or-no-p :override #'y-or-n-p)

;; Like a free bird
(function-put #'narrow-to-region 'disabled nil)

(setq-default
 auto-hscroll-mode               'current-line
 fill-column                     blc-chars-per-line
 frame-resize-pixelwise          t
 indent-tabs-mode                nil
 indicate-buffer-boundaries      t
 indicate-unused-lines           t
 line-number-display-limit-width (ash blc-chars-per-line 3)
 mode-line-format                (blc-sed-tree " +" " " mode-line-format)
 recenter-redisplay              nil
 scroll-conservatively           most-positive-fixnum
 scroll-margin                   1
 scroll-preserve-screen-position t
 scroll-step                     1
 shell-file-name                 "/bin/sh"
 tab-width                       2
 window-combination-resize       t
 x-gtk-use-system-tooltips       nil)

;;; Bindings

(map-do
 (lambda (map bindings)
   (map-do (-cut define-key map <> <>) bindings))
 `((,(current-global-map)
    ([S-next]                  . ,#'blc-small-scroll-up)
    ([S-prior]                 . ,#'blc-small-scroll-down)
    ([f5]                      . ,#'blc-revert-buffer)
    ([remap info]              . ,#'blc-info)
    ([remap info-other-window] . ,#'blc-info-other-window))
   (,mode-specific-map
    ("\C-r"                    . ,#'blc-rename-buffer)
    ("b"                       . ,#'blc-org-find-file)
    ("4b"                      . ,#'blc-org-find-file-other-window)
    ("i"                       . ,#'blc-indent-relative)
    ("P"                       . ,#'blc-align-punctuation))
   (,ctl-x-map
    ("\C-n"                    . ,#'blc-open-next-line)
    ("\C-p"                    . ,#'blc-open-previous-line)
    ("7"                       . ,#'blc-transpose-split)
    ("B"                       . ,#'blc-bury-buffer)
    ("l"                       . ,#'blc-echo-fast-line-count))
   (,ctl-x-5-map
    ("3"                       . ,#'blc-make-graphic-display))
   (,esc-map
    ("R"                       . ,#'redraw-display))))


;;;; PACKAGES

(use-package 2048-game
  :ensure)

(use-package abbrev
  :delight abbrev-mode)

(use-package ace-window
  :ensure
  :bind* ("M-o" . ace-window)
  :config
  (ace-window-display-mode))

(use-package ag
  :ensure
  :config
  (setq-default ag-highlight-search t)
  (add-to-list 'ag-arguments "--context=5"))

(use-package alert
  :ensure
  :init
  (setq-default alert-default-style 'notifications))

(use-package apt-sources
  :disabled
  :mode (("\\.sources\\'"                   . apt-sources-mode)
         ("sources\\.list\\'"               . apt-sources-mode)
         ("sources\\.list\\.d/.+\\.list\\'" . apt-sources-mode))
  :init
  (add-hook 'apt-sources-mode-hook #'blc-turn-off-electric-indent-local))

(use-package apt-utils
  :disabled
  :commands apt-utils-search apt-utils-show-package)

(use-package ascii
  :ensure)

(use-package ascii-art-to-unicode
  :ensure)

(use-package asm-mode
  :config
  (setq-default asm-comment-char ?#))

(use-package atomic-chrome
  :ensure
  :config
  (setq-default
   atomic-chrome-extension-type-list
   (seq-intersection '(ghost-text) atomic-chrome-extension-type-list))
  (blc-aput 'atomic-chrome-url-major-mode-alist "github\\.com" #'gfm-mode))

(use-package auctex-latexmk
  :ensure)

(use-package auth-password-store
  :ensure)

(use-package auth-source
  :config
  ;; Add SMTPS
  (map-put auth-source-protocols 'smtp '("smtp" "smtps" "25" "465" "587"))

  (setq-default auth-source-cache-expiry (blc-mins-to-secs 15)
                auth-source-debug        'trivia
                auth-sources             '(password-store)))

(use-package auth-source-pass
  :commands auth-source-pass-backend-parse
  :init
  (add-hook 'auth-source-backend-parser-functions
            #'auth-source-pass-backend-parse))

(use-package autorevert
  :delight auto-revert-mode "↻")

(use-package avy
  :ensure
  :bind (:map
         esc-map
         ("]"   . avy-goto-word-or-subword-1)
         :map
         goto-map
         ("f" . avy-goto-line)
         :map
         isearch-mode-map
         ("C-'" . avy-isearch))
  :config
  (setq-default avy-all-windows 'all-frames
                avy-background  t))

(use-package babel
  :ensure)

(use-package battery
  :init
  (setq-default battery-mode-line-format "🔋%b%p%% ")
  (display-battery-mode))

(use-package bbdb
  :ensure
  :init
  (map-do #'add-hook
          `((gnus-started-hook . ,#'blc-bbdb-set-gnus-summary-line-format)
            (gnus-startup-hook . ,#'bbdb-insinuate-gnus)))

  (setq-default bbdb-default-country    nil
                bbdb-name-format        'last-first
                bbdb-phone-style        nil
                bbdb-pop-up-window-size t)

  :config
  (map-do #'add-to-list
          `(;; Support Eircode
            (bbdb-legal-postcodes
             . ,(let ((char '(in "A-N" "P-Z" digit)))
                  (blc-rx `(: bos (= 3 ,char) (? ?\s) (= 4 ,char) eos))))
            ;; Display `mail-name' xfield in gnus summary buffer
            (bbdb-mua-summary-unification-list . ,bbdb-mail-name)))

  (setq-default
   bbdb-phone-label-list
   (blc-sed-tree "cell" "mobile" bbdb-phone-label-list nil t)
   bbdb-user-mail-address-re
   (regexp-opt (mapcar (lambda (addr)
                         (car (split-string addr "@")))
                       (blc-msmtp-addresses))
               'words)))

(use-package better-shell
  :ensure)

(use-package "bindings"
  :init
  (setq-default mode-line-percent-position '(-3 "%o")))

(use-package bison-mode
  :ensure)

(use-package bongo
  :ensure)

(use-package bookmark
  :bind (:map
         ctl-x-r-map
         ("4 b" . bookmark-jump-other-window))
  :init
  (setq-default bookmark-save-flag       1
                bookmark-search-delay    0
                bookmark-use-annotations t))

(use-package browse-url
  :init
  (setq-default browse-url-browser-function #'blc-browse-url))

(use-package bug-reference
  :init
  (map-do #'add-hook
          `((prog-mode-hook . ,#'bug-reference-prog-mode)
            (text-mode-hook . ,#'bug-reference-mode     ))))

(use-package calendar
  :init
  (setq-default
   calendar-date-style                  'iso
   calendar-christian-all-holidays-flag t
   calendar-islamic-all-holidays-flag   t))

(use-package calfw-cal
  :ensure
  :bind ("<f9>" . cfw:open-diary-calendar))

(use-package cc-mode
  :init
  (mapc (-cut add-hook 'c-mode-common-hook <>)
        `(,#'blc-turn-on-c++-comments
          ,#'hs-minor-mode))
  :config
  (let ((name    "blc")
        (base    "linux")
        (offsets '((     access-label . / )
                   (       case-label . + )
                   (      innamespace . 0 )
                   (      inline-open . 0 )
                   (    arglist-close . 0 )
                   (      inher-intro . ++)
                   (member-init-intro . ++))))

    (c-add-style name `(,base
                        (c-basic-offset  . 2)
                        (c-offsets-alist . ,offsets)))

    (map-put c-default-style 'other name)))

(use-package chess
  :ensure
  :config
  (let ((dir (package-desc-dir (cadr (assq 'chess package-alist)))))
    (setq-default
     chess-images-default-size blc-chars-per-line
     chess-images-directory    (blc-dir dir "pieces" "xboard"))))

(use-package cmake-mode
  :ensure)

(use-package comint
  :config
  (unbind-key "C-c C-r" comint-mode-map))

(use-package comment-dwim-2
  :ensure
  :bind ([remap comment-dwim] . comment-dwim-2))

(use-package compile
  :init
  (setq-default compilation-message-face  'default
                compilation-scroll-output 'first-error
                compile-command           "make"))

(use-package conf-mode
  :mode ("\\.dirs\\'" . conf-unix-mode)
  :init
  (add-hook 'conf-mode-hook #'blc-turn-off-electric-indent-local)
  :config
  (delight `((    ,#'conf-colon-mode "🔧[:]"  :major)
             (    ,#'conf-space-mode "🔧[ ]"  :major)
             (     ,#'conf-unix-mode "🔧[🐧]" :major)
             (,#'conf-xdefaults-mode "🔧[X]"  :major))))

(use-package copyright
  :init
  (setq-default copyright-names-regexp (regexp-quote user-full-name)))

(use-package counsel
  :ensure
  :delight counsel-mode

  :bind
  (([remap bookmark-jump           ] . counsel-bookmark)
   ([remap describe-bindings       ] . counsel-descbinds)
   ([remap describe-function       ] . counsel-describe-function)
   ([remap describe-variable       ] . counsel-describe-variable)
   ([remap execute-extended-command] . counsel-M-x)
   ([remap find-library            ] . counsel-find-library)
   ([remap imenu                   ] . counsel-imenu)
   ([remap info-lookup-symbol      ] . counsel-info-lookup-symbol)
   ([remap isearch-forward         ] . counsel-grep-or-swiper)
   ([remap load-library            ] . counsel-load-library)
   ([remap load-theme              ] . counsel-load-theme)
   ([remap menu-bar-open           ] . counsel-tmm)
   ([remap org-goto                ] . counsel-org-goto)
   ([remap org-set-tags-command    ] . counsel-org-tag)
   ([remap pop-mark                ] . counsel-mark-ring)
   ([remap yank-pop                ] . counsel-yank-pop)
   :map
   ctl-x-map
   ("C-l" . counsel-locate)
   :map
   help-map
   ("C-j" . counsel-faces)
   :map
   mode-specific-map
   ("g"   . counsel-ag)
   ("t"   . counsel-git)
   ("u"   . counsel-unicode-char)
   ("j d" . counsel-dired-jump)
   ("j f" . counsel-file-jump))

  :init
  ;; Do not remap keys above with `counsel-mode'
  (setq-default counsel-mode-map ()
                ffap-file-finder #'counsel-find-file)

  (advice-add #'org-goto :override #'counsel-org-goto)

  :config
  (setq-default
   counsel-find-file-at-point           t
   counsel-git-grep-skip-counting-lines t
   ;; Search with smart case and shell expansion
   counsel-grep-base-command            "ag --nocolor \"%s\" %s"
   counsel-org-goto-display-tags        t
   counsel-org-goto-display-todo        t
   counsel-org-goto-face-style          'verbatim)

  (counsel-mode))

(use-package counsel-gtags
  :ensure)

(use-package counsel-projectile
  :ensure
  :init
  (add-hook 'projectile-mode-hook #'counsel-projectile-on))

(use-package crontab-mode
  :ensure
  :mode "\\.cron\\(?:tab\\)??\\'" "cron\\(?:tab\\)??\\.")

(use-package csharp-mode
  :ensure)

(use-package cssh
  :ensure)

(use-package cus-edit
  :init
  (when (file-exists-p
         (setq-default custom-file
                       (expand-file-name "custom.el" user-emacs-directory)))
    (lwarn 'blc :warning "Custom file %s exists but not loaded." custom-file)))

(use-package csv-mode
  :ensure
  :init
  (add-hook 'csv-mode-hook #'blc-csv-align-all-fields)
  :config
  (setq-default csv-align-style 'auto))

(use-package dafny-mode
  :ensure boogie-friends
  :init
  (mapc (-cut add-hook 'dafny-mode-hook <>)
        `(,#'blc-turn-off-electric-indent-local
          ,#'blc-turn-off-flycheck
          ,#'blc-turn-off-prettify-symbols)))

(use-package dash
  :functions dash-enable-font-lock
  :config
  (dash-enable-font-lock))

(use-package deb-view
  :disabled)

(use-package debbugs
  :ensure)

(use-package debian-changelog-mode
  :ensure)

(use-package debpaste
  :ensure)

(use-package define-word
  :ensure
  :bind (:map
         mode-specific-map
         ("/" . define-word-at-point)))

(use-package delsel
  :init
  (delete-selection-mode))

(use-package diary-lib
  :init
  (setq-default
   diary-comment-start     ";"
   diary-number-of-entries 3))

(use-package dictionary
  :ensure)

(use-package dired
  :defines dired-omit-files
  :bind (:map
         ctl-x-map
         ("C-j" . dired-jump)
         :map
         ctl-x-4-map
         ("C-j" . dired-jump-other-window))

  :init
  (setq-default
   dired-auto-revert-buffer t
   dired-dwim-target        t
   dired-listing-switches   (string-join '("--almost-all"
                                           "--classify"
                                           "--group-directories-first"
                                           "--human-readable"
                                           "-l")
                                         " ")
   dired-recursive-copies   'always)

  :config
  (require 'dired-x)

  (setq-default
   dired-omit-files
   (blc-rx `(| (: bos ?. (not (in ?.))) (regexp ,dired-omit-files))))

  (map-do (lambda (cmd suffs)
            (thread-first 'dired-guess-shell-alist-user
              (blc-aput (blc-rx `(: ?. (| ,@suffs) eos)) `(,cmd))))
          '(("localc"   . ("ods" "xls" "xlsx"))
            ("lowriter" . ("doc" "docx" "odt"))
            ("mpv"      . ("mkv" "mp4" "webm"))
            ("pdf"      . ("pdf")))))

(use-package dired-aux
  :config
  (mapc (-cut add-to-list 'dired-compress-files-alist <>)
        '(("\\.tar\\.7z\\'" . "tar -c %i | 7zr a -si %o")
          ("\\.7z\\'"       . "7zr a %o %i"))))

(use-package disaster
  :ensure
  :init
  (with-eval-after-load 'cc-mode
    (bind-key "C-c d" #'disaster c-mode-base-map))
  :config
  (setq-default disaster-objdump "objdump -D -M att -Sl --no-show-raw-insn"))

(use-package discover-my-major
  :ensure
  :bind ("C-h C-m" . discover-my-major))

(use-package doc-view
  :init
  (setq-default doc-view-conversion-refresh-interval nil
                doc-view-pdf->png-converter-function #'blc-doc-view-pdf-to-png))

(use-package dropbox
  :ensure
  :init
  (setq-default dropbox-locale  "en_IE"
                dropbox-verbose t))

(use-package ducpel
  :ensure)

(use-package ebib
  :ensure
  :bind (:map
         mode-specific-map
         ("e" . ebib))
  :config
  (setq-default
   ebib-bibtex-dialect 'biblatex
   ebib-use-timestamp  t)

  (add-to-list 'ebib-preload-bib-files blc-bib-file))

(use-package eldoc
  :delight eldoc-mode)

(use-package elisp-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'blc-rainbow-mode)
  :config
  (delight `((      ,#'emacs-lisp-mode "(ε)" :major)
             (,#'lisp-interaction-mode "(ι)" :major))))

(use-package embrace
  :ensure)

(use-package emms
  :ensure
  :init
  (setq-default emms-volume-change-function #'emms-volume-pulse-change))

(use-package engine-mode
  :ensure
  :commands engine/execute-search engine/get-query
  :init
  (autoload 'engine-mode-prefixed-map "engine-mode" nil nil 'keymap)
  (define-key ctl-x-map "/" 'engine-mode-prefixed-map)
  :config
  (defengine book-depository
    "https://bookdepository.com/search?searchTerm=%s"
    :keybinding "b")
  (defengine google-def
    "https://encrypted.google.com/search?ie=utf-8&oe=utf-8&q=define+%s"
    :keybinding "d")
  (defengine google-enc
    "https://encrypted.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")
  (defengine google-sch
    "https://scholar.google.com/scholar?q=%s"
    :keybinding "s")
  (defengine hoogle
    "https://haskell.org/hoogle/?hoogle=%s"
    :keybinding "h")
  (defengine imdb
    "http://imdb.com/find?q=%s"
    :keybinding "i")
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "v")
  (defengine wikipedia
    "https://en.wikipedia.org/w/index.php?search=%s"
    :keybinding "w")
  (engine-mode))

(use-package eshell
  :init
  (add-hook 'eshell-output-filter-functions #'eshell-truncate-buffer))

(use-package ess
  :ensure
  :config
  (setq-default ess-default-style   'DEFAULT
                ess-indent-from-lhs nil))

(use-package eudc
  :after message
  :init
  (add-hook 'gnus-load-hook #'eudc-load-eudc)
  (setq-default eudc-protocol            'bbdb
                eudc-inline-query-format '((email)
                                           (name)
                                           (firstname)
                                           (firstname name))))

(use-package ewmctrl
  :ensure)

(use-package eww
  :bind (:map
         eww-bookmark-mode-map
         ("n" .             next-line)
         ("p" .         previous-line)
         ("w" . blc-eww-bookmark-save))
  :init
  (add-hook 'eww-mode-hook #'blc-eww-increase-readability)
  (setq-default
   eww-search-prefix
   "https://encrypted.google.com/search?ie=utf-8&oe=utf-8&q=")
  :config
  (delight #'eww-mode "🕸" :major))

(use-package exec-path-from-shell
  :ensure
  :config
  (mapc (-cut add-to-list 'exec-path-from-shell-variables <>)
        '("SSH_AGENT_PID" "SSH_AUTH_SOCK")))

(use-package executable
  :init
  (add-hook 'after-save-hook
            #'executable-make-buffer-file-executable-if-script-p))

(use-package expand-region
  :ensure
  :bind ("M-+" . er/expand-region))

(use-package eyebrowse
  :ensure)

(use-package ffap
  :commands ffap-gnus-hook
  ;; For some reason remaps do not work
  :bind (:map
         ctl-x-map
         ("d"   .     dired-at-point)
         ("C-f" . find-file-at-point)
         :map
         ctl-x-4-map
         ("d"   . ffap-dired-other-window)
         ("C-f" .       ffap-other-window)
         :map
         ctl-x-5-map
         ("d"   . ffap-dired-other-frame)
         ("C-f" .       ffap-other-frame))

  :init
  (mapc (-cut add-hook <> #'ffap-gnus-hook)
        '(gnus-summary-mode-hook gnus-article-mode-hook))

  (setq-default dired-at-point-require-prefix t
                ffap-require-prefix           t
                ffap-rfc-path                 "https://ietf.org/rfc/rfc%s.txt")

  :config
  (add-to-list 'ffap-rfc-directories
               (blc-dir (blc-user-dir "DOCUMENTS") "rfc")))

(use-package fic-mode
  :ensure
  :init
  (mapc (-cut add-hook <> #'fic-mode) blc-fundamental-hooks)
  :config
  (mapc (-cut add-to-list 'fic-highlighted-words <>)
        '("HACK" "KLUDGE" "NOTE" "WARN")))

(use-package figlet
  :ensure)

(use-package files
  :init
  (setq-default
   auto-save-visited-interval auto-save-timeout
   backup-by-copying          t         ; Do not clobber symlinks
   backup-directory-alist               ; Backup/auto-save directory
   '(("." . "~/.backup/"))
   delete-old-versions        t
   directory-free-space-args  "-hP"
   find-file-visit-truename   t
   kept-new-versions          4
   kept-old-versions          2
   mode-require-final-newline nil       ; Do not silently append EOF NL
   version-control            t)        ; Versioned backups

  (add-to-list 'safe-local-variable-values
               '(eval . (when buffer-file-name (view-mode))))

  (auto-save-visited-mode))

(use-package fill-column-indicator
  :ensure
  :commands turn-off-fci-mode
  :init
  (setq-default fci-rule-column blc-chars-per-line)

  (map-do (lambda (fn hooks)
            (mapc (-cut add-hook <> fn) hooks))
          `((,#'turn-on-fci-mode  . ,blc-fundamental-hooks)
            (,#'turn-off-fci-mode . (lisp-interaction-mode-hook
                                     org-mode-hook
                                     visual-line-mode-hook)))))

(use-package find-file
  :init
  (add-hook 'find-file-hook #'blc-strip-large-buffer))

(use-package find-func
  :bind
  (("C-h C-f"   . find-function)
   ("C-h 4 C-f" . find-function-other-window)
   ("C-h 5 C-f" . find-function-other-frame)
   ("C-h C-k"   . find-function-on-key)
   ("C-h 4 C-k" . find-function-on-key-other-window)
   ("C-h 5 C-k" . find-function-on-key-other-frame)
   ("C-h C-v"   . find-variable)
   ("C-h 4 C-v" . find-variable-other-window)
   ("C-h 5 C-v" . find-variable-other-frame)))

(use-package fireplace
  :ensure)

(use-package flex-mode
  :mode "\\.lex\\'")

(use-package flx
  :ensure)

(use-package font-lock
  :init
  (setq-default font-lock-maximum-decoration t))

(use-package frame
  :init
  (blc-with-every-frame #'blc-turn-off-cursor-blink))

(use-package free-keys
  :ensure)

(use-package ggtags
  :ensure
  :bind (:map
         ggtags-mode-map
         ("M-F" . ggtags-find-reference))
  :commands ggtags-find-reference
  :config
  (unbind-key "M-]" ggtags-mode-map)    ; `ggtags-find-reference'
  (setq-default ggtags-enable-navigation-keys nil))

(use-package ghc
  :ensure
  :init
  (add-hook 'haskell-mode-hook #'ghc-init)
  (setq-default ghc-doc-browser-function #'blc-browse-url-ghc-doc))

(use-package git-annex
  :ensure
  :after dired
  :init
  (setq-default git-annex-commit nil))

(use-package gitattributes-mode
  :ensure)

(use-package git-commit
  :init
  (add-to-list 'auto-mode-alist
               `(,git-commit-filename-regexp . ,#'git-commit-setup))

  (mapc (-cut add-hook 'git-commit-setup-hook <>)
        `(,#'blc-git-commit-set-fill-column
          ,#'bug-reference-mode))

  (setq-default git-commit-summary-max-length 50
                global-git-commit-mode        nil)

  :config
  (add-to-list 'git-commit-style-convention-checks 'overlong-summary-line))

(use-package gitconfig-mode
  :ensure
  :init
  (add-hook 'gitconfig-mode-hook #'blc-turn-off-indent-tabs))

(use-package gitignore-mode
  :ensure)

(use-package gnus
  :init
  ;; Shave a few startup seconds
  (map-do #'add-hook
          `((gnus-load-hook    . ,#'blc-gc-thresh-maximise)
            (gnus-started-hook . ,#'blc-gc-thresh-restore )))

  (setq-default
   gnus-home-directory user-emacs-directory
   gnus-init-file      (expand-file-name "gnus" gnus-home-directory)))

(use-package gnus-desktop-notify
  :ensure
  :init
  (add-hook 'gnus-startup-hook #'gnus-desktop-notify-mode)
  :config
  (setq-default gnus-desktop-notify-groups   'gnus-desktop-notify-explicit
                gnus-desktop-notify-format   "%3n: %G"
                gnus-desktop-notify-function 'gnus-desktop-notify-send))

(use-package gnutls
  :init
  (setq-default gnutls-min-prime-bits nil))

(use-package google-contacts
  :ensure
  :init
  (add-hook 'google-contacts-mode-hook #'blc-google-contacts-fontify))

(use-package google-maps
  :ensure)

(use-package google-this
  :ensure)

(use-package gscholar-bibtex
  :ensure
  :config
  (setq-default
   gscholar-bibtex-database-file  blc-bib-file
   gscholar-bibtex-default-source
   (map-contains-key gscholar-bibtex-available-sources "Google Scholar")))

(use-package hacker-typer
  :ensure
  :config
  (require 'mm-util)
  (setq-default
   hacker-typer-files
   (mapcar (-cut concat "file://" <>)
           (directory-files (blc-dir source-directory "src") t "\\.c\\'" t))
   hacker-typer-random-range   (mapcar (-cut * 2 <>) hacker-typer-random-range)
   hacker-typer-show-hackerman t))

(use-package haskell-mode
  :ensure
  :bind (:map
         haskell-mode-map
         ([remap haskell-hoogle] . haskell-hayoo))

  :init
  (setq-default
   haskell-completing-read-function            #'completing-read
   haskell-indent-offset                       2
   haskell-notify-p                            t
   haskell-process-log                         t
   haskell-process-suggest-hoogle-imports      t
   haskell-process-suggest-remove-import-lines t)

  (map-do
   #'add-hook
   `((haskell-cabal-mode-hook . ,#'blc-turn-off-electric-indent-local)
     ,@(mapcar (-cut cons 'haskell-mode-hook <>)
               `(,#'blc-turn-off-electric-indent-local
                 ,#'haskell-indent-mode
                 ,#'interactive-haskell-mode))))

  :config
  (delight `((,#'haskell-indent-mode)
             (,#'haskell-mode
              (:eval (if (interactive-haskell-mode)
                         "λ>"
                       "λ"))
              :major)
             (,#'interactive-haskell-mode))))

(use-package hayoo
  :ensure)

(use-package helm-make
  :ensure
  :init
  (setq-default
   helm-make-cache-targets      t
   helm-make-completion-method  'ivy
   helm-make-list-target-method 'qp
   helm-make-require-match      nil))

(use-package hi-lock
  :commands turn-on-hi-lock-if-enabled
  :init
  (add-hook 'hi-lock-mode-hook #'blc-hi-lock-no-eof-nl)
  (global-hi-lock-mode)

  :config
  (mapc (-cut add-to-list 'hi-lock-exclude-modes <>)
        '(comint-mode
          completion-list-mode
          display-time-world-mode
          erc-mode
          eshell-mode
          term-mode))

  (let* ((mode    'hi-lock-mode)
         (lighter (map-elt minor-mode-alist mode)))
    (map-put minor-mode-alist mode (blc-sed-tree " .+" "⛯" lighter))))

(use-package highlight-escape-sequences
  :ensure
  :init
  (turn-on-hes-mode))

(use-package hl-line
  :init
  (mapc (-cut add-hook <> #'hl-line-mode)
        '(dired-mode-hook
          git-rebase-mode-hook
          ibuffer-mode-hook
          ivy-occur-mode-hook)))

(use-package hideshow
  :bind (:map
         hs-minor-mode-map
         ("C-c C-i" . hs-toggle-hiding)))

(use-package hledger-mode
  :ensure
  :mode "\\.journal\\'"
  :config
  (setq-default
   hledger-currency-string "€"
   hledger-jfile           "~/.hledger.journal"
   hledger-ratios-essential-expense-accounts
   (blc-hledger-accounts "expenses" '("housing" "groceries"))
   hledger-ratios-liquid-asset-accounts
   (blc-hledger-accounts "assets"   '("boi" "cash"))))

(use-package holidays
  :commands calendar-holiday-list
  :init
  (setq-default
   holiday-bahai-holidays    ()
   holiday-oriental-holidays ())
  :config
  ;; Remove redundant full-stops
  (mapc (lambda (sym)
          (set-default
           sym (blc-sed-tree "St\\(\\.\\)" "" (symbol-value sym) t t 1)))
        '(calendar-holidays
          holiday-general-holidays))

  ;; Maximise Unicode usage
  (let ((lut '(("Christmas"        . "🎄")
               ("Good Friday"      . "🞤" )
               ("Halloween"        . "👻")
               ("Hanukkah"         . "🕎")
               ("Palm Sunday"      . "🌴")
               ("St Patrick's Day" . "☘" )
               ("Valentine's Day"  . "❤" ))))
    (setq-default calendar-holidays (blc-sed-tree (regexp-opt (map-keys lut))
                                                  (-cut blc-aget lut <> nil t)
                                                  calendar-holidays))))

(use-package ibuf-ext
  :commands ibuffer-auto-mode
  :bind (([remap list-buffers] . ibuffer)
         :map
         ibuffer-mode-map
         ([remap ibuffer-find-file] . blc-ibuffer-ffap)
         :map
         mode-specific-map
         ("j b" . ibuffer-jump))
  :init
  (mapc (-cut add-hook 'ibuffer-mode-hook <>)
        `(,#'ibuffer-auto-mode
          ,#'blc-turn-on-ibuffer-filter-groups))

  :config
  (define-ibuffer-filter modes
      "Filter by multiple parent major mode QUALIFIERs."
    (:description "derived major modes")
    (with-current-buffer buf
      (apply #'derived-mode-p (blc-as-list qualifier))))

  (define-ibuffer-filter names
      "Filter by multiple string or variable QUALIFIERs.
Filter `starred-name' is implied unless symbol `nostar' present."
    (:description "buffer names")
    (let ((names (blc-keep (lambda (name)
                             (funcall (if (and (symbolp name) (boundp name))
                                          #'symbol-value
                                        #'identity)
                                      name))
                           (blc-as-list qualifier)
                           t)))
      (and (or (memq 'nostar names)
               (funcall (caddr (assq 'starred-name ibuffer-filtering-alist))
                        buf qualifier))
           (let ((re (regexp-opt
                      (blc-keep (lambda (name)
                                  (funcall (pcase name
                                             ((pred stringp) #'identity)
                                             ((pred bufferp) #'buffer-name)
                                             (_              #'ignore))
                                           name))
                                (delq 'nostar names)))))
             (unless (string-empty-p re)
               (ibuffer-buf-matches-predicates buf `(,re)))))))

  ;; Define before use
  (mapc (-cut add-to-list 'ibuffer-saved-filters <>)
        `(("package" (directory . ,(regexp-opt
                                    (mapcar #'expand-file-name
                                            `(,(blc-parent-dir data-directory)
                                              ,source-directory
                                              ,package-user-dir)))))
          ("REPL"    (modes . (eshell-mode
                               inferior-emacs-lisp-mode
                               lisp-interaction-mode)))))

  (setq-default
   ibuffer-always-compile-formats          t
   ibuffer-default-sorting-mode            'alphabetic
   ibuffer-jump-offer-only-visible-buffers t
   ibuffer-old-time                        12
   ibuffer-saved-filter-groups
   `((,blc-ibuffer-default-group
      ("Book" (or (modes . (bookmark-bmenu-mode
                            bookmark-edit-annotation-mode))
                  (names . "Bookmark Annotation")))
      ("Code" (and (modes . (conf-mode
                             prog-mode))
                   (not (saved . "package"))
                   (not (saved . "REPL"))))
      ("Cus"  (modes . Custom-mode))
      ("Dir"  (modes . dired-mode))
      ("Doc"  (or (modes . (apropos-mode
                            help-mode
                            Info-mode
                            Man-mode
                            woman-mode))
                  (names . "Ivy Help")))
      ("Gnus" (or (saved . "gnus")
                  (names . (nostar
                            gnus-dribble-buffer))))
      ("Git"  (modes . (magit-mode
                        magit-repolist-mode)))
      ("Img"  (modes . image-mode))
      ("Log"  (or (modes . (TeX-output-mode
                            compilation-mode
                            ivy-occur-mode
                            messages-buffer-mode
                            tags-table-mode))
                  (names . ("Backtrace"
                            "Warnings"
                            "WoMan-Log"
                            blc-gnus-log-buffers
                            dired-log-buffer
                            doc-view-conversion-buffer))))
      ("PDF"  (modes . pdf-view-mode))
      ("Pkg"  (and (saved . "package")
                   (not (saved . "REPL"))))
      ("Proc" (names . "Async Shell Command"))
      ("REPL" (saved . "REPL"))
      ("SX"   (or (modes . (sx-compose-mode
                            sx-question-list-mode
                            sx-question-mode))
                  (names . "sx temp buffer")))
      ("TeX"  (saved . "TeX"))
      ("Text" (saved . "text document"))
      ("Web"  (saved . "web"))))
   ibuffer-show-empty-filter-groups        nil
   ibuffer-use-other-window                t))

(use-package ido
  :init
  (setq-default ido-enable-flex-matching t))

(use-package idris-mode
  :ensure)

(use-package ielm
  :init
  (add-hook 'ielm-mode-hook #'blc-turn-on-lexical-binding)
  :config
  (let ((lighter "(>)"))
    (delight #'inferior-emacs-lisp-mode lighter :major)
    (setq-default ielm-noisy  nil
                  ielm-prompt (format "%s " lighter))))

(use-package "indent"
  :init
  (setq-default indent-line-function #'insert-tab))

(use-package info
  :bind (:map
         Info-mode-map
         ("k" . blc-info-kill))
  :config
  (delight #'Info-mode "📘" :major))

(use-package interleave
  :ensure)

(use-package irfc
  :ensure
  :mode ("[rR][fF][cC]\\([[:digit:]]+?\\)\\.txt\\'" . irfc-mode)
  :bind (:map
         irfc-mode-map
         ("DEL" . scroll-down))
  :config
  (when (require 'ffap nil t)
    (setq-default
     irfc-directory         (seq-find #'identity ffap-rfc-directories)
     irfc-download-base-url (url-file-directory  ffap-rfc-path))))

(use-package irfc-x
  :commands irfc-x-list)

(use-package irony
  :ensure
  :delight irony-mode "🜜"
  :init
  (map-do #'add-hook
          `((c-mode-common-hook . ,#'irony-mode)
            (   irony-mode-hook . ,#'irony-cdb-autosetup-compile-options)))
  :config
  (setq-default irony-server-build-dir
                (blc-dir irony-server-source-dir "build")))

(use-package irony-eldoc
  :ensure
  :init
  (add-hook 'irony-mode-hook #'irony-eldoc)
  (setq-default irony-eldoc-use-unicode t))

(use-package "isearch"
  :init
  (setq-default isearch-allow-scroll t)
  (add-hook 'isearch-mode-hook #'blc-isearch-delight))

(use-package isearch-prop
  :ensure)

(use-package ivy
  :ensure
  :delight ivy-mode
  :commands ivy--regex-ignore-order ivy-format-function-arrow ivy-set-sources
  :bind (([remap switch-to-buffer] . ivy-switch-buffer)
         ([remap switch-to-buffer-other-window]
          . ivy-switch-buffer-other-window)
         :map
         mode-specific-map
         ("r"   . ivy-resume)
         :map
         ivy-minibuffer-map
         ("M-D" . ivy-dispatching-done)
         :map
         ivy-occur-mode-map
         ("n"   . ivy-occur-next-line)
         ("p"   . ivy-occur-previous-line))

  :init
  ;; Autoloading
  (setq-default completing-read-function #'ivy-completing-read)
  (add-hook 'eval-expression-minibuffer-setup-hook #'ivy-mode)

  :config
  ;; Default matching behaviour
  (map-put ivy-re-builders-alist t #'ivy--regex-ignore-order)

  ;; Reverse parsed order
  (mapc (-cut map-put ivy-sort-functions-alist <> #'blc-sort-reverse)
        `(,#'Info-complete-menu-item
          ,#'Man-goto-section))

  ;; Faces
  (map-do (-cut set-face-attribute <> nil :inherit <>)
          '((ivy-action          . font-lock-keyword-face)
            (ivy-modified-buffer . font-lock-variable-name-face)
            (ivy-virtual         . shadow)))

  ;; Location suggestions
  (ivy-set-sources 'counsel-locate
                   '((blc-ivy-recentf)
                     (original-source)))

  (setq-default
   ivy-count-format            "(%d/%d) "
   ivy-extra-directories       ()
   ivy-fixed-height-minibuffer t
   ivy-format-function         #'ivy-format-function-arrow
   ;; Do not match start of input for counsel, man or org commands
   ivy-initial-inputs-alist
   (map-remove (lambda (cmd _)
                 (or (memq cmd '(man woman))
                     (string-match-p (rx bos (| "org" "counsel") ?-)
                                     (symbol-name cmd))))
               ivy-initial-inputs-alist)
   ivy-on-del-error-function   #'ignore
   ivy-use-virtual-buffers     t)

  (ivy-mode))

(use-package ivy-bibtex
  :ensure
  :commands bibtex-completion-format-entry
  :config
  (map-do #'add-to-list
          `((bibtex-completion-additional-search-fields . "date")
            (bibtex-completion-bibliography             . ,blc-bib-file)))

  (setq-default
   bibtex-completion-display-formats
   `((t . ,(string-join
            '("${author:30}"
              "${date:4}"
              "${title:*}"
              "${=has-pdf=:1}${=has-note=:1}"
              "${=type=:14}")
            " ")))))

(use-package ivy-hydra
  :ensure)

(use-package ivy-pages
  :ensure)

(use-package ivy-rich
  :disabled
  :ensure
  :after ivy
  :functions ivy-set-display-transformer
  :commands ivy-rich-switch-buffer-transformer
  :config
  (ivy-set-display-transformer #'ivy-switch-buffer
                               #'ivy-rich-switch-buffer-transformer))

(use-package jade
  :disabled
  :ensure
  :commands jade-interaction-mode
  :init
  (add-hook 'js2-mode-hook #'jade-interaction-mode))

(use-package jit-lock
  :init
  (setq-default
   jit-lock-stealth-load 60
   jit-lock-stealth-time  4))

(use-package js
  :config
  (setq-default
   js-enabled-frameworks
   (seq-intersection '(dojo javascript prototype) js-enabled-frameworks)
   js-indent-level         4
   js-switch-indent-offset 4))

(use-package js2-mode
  :ensure
  :mode "\\.js\\'"
  :interpreter "node" "nodejs"
  :functions js2-line-break
  :init
  (setq-default js2-bounce-indent-p t)

  (mapc (-cut add-hook 'js2-mode-hook <>)
        `(,#'js2-highlight-unused-variables-mode
          ,#'blc-turn-off-electric-indent-local))

  :config
  (delight #'js2-mode "jsⅡ" :major)

  (define-key js2-mode-map "\r" #'js2-line-break)

  (setq-default
   js2-allow-rhino-new-expr-initializer nil
   js2-global-externs                   '("define" "location")
   js2-highlight-level                  3
   js2-include-node-externs             t
   js2-mode-assume-strict               t
   js2-skip-preprocessor-directives     t))

(use-package js2-refactor
  :ensure
  :delight js2-refactor-mode
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package json-mode
  :ensure)

(use-package ledger-mode
  :ensure
  :functions ledger-reports-add
  :mode "\\.ledger\\'"
  :config
  (setq-default
   ledger-default-date-format               ledger-iso-date-format
   ledger-post-amount-alignment-at          :decimal
   ledger-report-auto-refresh-sticky-cursor t
   ledger-report-use-header-line            t
   ledger-use-iso-dates                     t)

  (blc-aput 'ledger-report-format-specifiers
            "frame-width"
            #'blc-ledger-frame-width)

  (ledger-reports-add "blc-account"
                      (string-join '("%(binary)"
                                     "--columns %(frame-width)"
                                     "--dc"
                                     "--file %(ledger-file)"
                                     "register %(account)")
                                   " ")))

(use-package lisp-mode
  :init
  (add-hook 'lisp-mode-hook #'blc-turn-off-electric-indent-local))

(use-package list-processes+
  :ensure)

(use-package list-unicode-display
  :ensure)

(use-package lorem-ipsum
  :ensure)

(use-package lpr
  :init
  (setq-default lpr-add-switches nil
                lpr-command      "hp-print"))

(use-package lunar
  :config
  (setq-default
   lunar-phase-names
   (mapcar (lambda (name)
             (char-to-string (char-from-name (concat name " symbol") t)))
           lunar-phase-names)))

(use-package know-your-http-well
  :ensure)

(use-package macrostep
  :ensure)

(use-package magit
  :ensure
  :functions
  magit-add-section-hook
  magit-display-buffer-same-window-except-diff-v1
  magit-repolist-column-dirty
  :bind (:map
         ctl-x-map
         ("g" . magit-status))

  :init
  (setq-default magit-repository-directories `((,blc-repos-dir . 2)))

  :config
  (delight
   '((git-rebase-mode                 "±𝄢"  :major     )
     (magit-blame-mode                "🖜"  magit-blame)
     (magit-cherry-mode               "±🍒" :major     )
     (magit-diff-mode                 "±±"  :major     )
     (magit-log-mode                  "±㏒" :major     )
     (magit-log-select-mode           "±㏒" :major     )
     (magit-merge-preview-mode        "±⛙"  :major     )
     (magit-mode                      "±"   :major     )
     (magit-process-mode              "±👷" :major     )
     (magit-reflog-mode               "±🚑" :major     )
     (magit-refs-mode                 "±⚖"  :major     )
     (magit-repolist-mode             "±🖧" :major     )
     (magit-revision-mode             "±¶"  :major     )
     (magit-stash-mode                "±︷" :major     )
     (magit-stashes-mode              "±︷" :major     )
     (magit-status-mode               "±"   :major     )
     (magit-submodule-list-mode       "±%"  :major     )
     (magit-wip-after-apply-mode      ""    magit-wip  )
     (magit-wip-after-save-local-mode ""    magit-wip  )
     (magit-wip-before-change-mode    ""    magit-wip  )))

  ;; Misc.
  (setq-default
   magit-branch-popup-show-variables       t
   magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1
   magit-list-refs-sortby                  "-creatordate"
   magit-prefer-remote-upstream            t
   magit-remote-add-set-remote.pushDefault 'ask)

  ;; Always highlight tabs
  (blc-aput 'magit-diff-highlight-indentation "" 'tabs)

  ;; Status buffer
  (mapc (-cut magit-add-section-hook
              'magit-status-headers-hook
              <>
              'magit-insert-head-branch-header)
        '(magit-insert-remote-header
          magit-insert-repo-header))

  ;; Repo list: insert dirty column in third position
  (blc-insert-at
   'magit-repolist-columns `("D" 1 ,#'magit-repolist-column-dirty ()) 2)

  ;; Arguments
  (map-do #'add-to-list
          '((magit-merge-arguments  . "--ff-only"    )
            (magit-rebase-arguments . "--interactive")))

  ;; Inline format reformatting
  (let ((fmtre (rx ?% (group (? (in ?+ ?-)) (* digit)) (in ?U ?n)))
        case-fold-search)

    ;; Align refs with wider columns
    (mapc (lambda (fmt)
            (set-default fmt (blc-sed fmtre "-40" (symbol-value fmt) t t 1)))
          '(magit-refs-local-branch-format
            magit-refs-remote-branch-format
            magit-refs-symref-format
            magit-refs-tags-format))

    ;; Limit number of commits in log
    (setq-default
     magit-log-arguments
     (blc-sed-tree (rx "-n" (group (+ digit))) "32" magit-log-arguments t t 1)))

  ;; Modes
  (global-magit-file-mode)
  (magit-wip-after-apply-mode)
  (magit-wip-after-save-mode)
  (magit-wip-before-change-mode))

(use-package magit-annex
  :ensure)

(use-package magit-gh-pulls
  :disabled
  :ensure                               ; gh.el doesn't speak ssh?
  :commands turn-on-magit-gh-pulls
  :init
  (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(use-package magithub
  :disabled
  :ensure
  :after magit
  :commands magithub-feature-autoinject
  :init
  (setq-default magithub-api-timeout    4
                magithub-debug-mode     t
                ;; KLUDGE: Enable magithub in a sandboxed state
                magithub-hub-executable "")
  :config
  ;; KLUDGE: Allow magithub to be enabled on next
  ;;         `magithub-toggle-pull-requests' or
  ;;         `magithub-toggle-issues'.
  (custom-reevaluate-setting 'magithub-hub-executable)
  (magithub-feature-autoinject t))

(use-package make-mode
  :bind (:map
         makefile-mode-map
         ("C-c $" . makefile-insert-macro-ref))
  :config
  (delight `((,#'makefile-automake-mode "⛏.am" :major)
             (,#'makefile-mode          "⛏"    :major)
             (,#'makefile-gmake-mode    "⛏GNU" :major)))

  (setq-default makefile-macro-assign                           " := "
                makefile-pickup-everything-picks-up-filenames-p t
                makefile-tab-after-target-colon                 nil)

  ;; Expand GNU functions
  (let ((fns 'makefile-gnumake-functions-alist))
    (map-do (lambda (fn args)
              (unless (assoc-string fn (symbol-value fns))
                (blc-aput fns fn args)))
            '(("abspath"  "Names")
              ("error"    "Text")
              ("flavor"   "Variable")
              ("info"     "Text")
              ("lastword" "Text")
              ("realpath" "Names")
              ("value"    "Variable")
              ("warning"  "Text")
              ("wordlist" "Start index" "End index" "Text")))

    (set-default fns (seq-sort-by #'car #'string-lessp (symbol-value fns))))

  ;; Expand special targets
  (let ((targets 'makefile-special-targets-list))
    ;; Remove old-fashioned suffix rules
    (set targets (seq-remove (-cut string-match-p "\\." <>)
                             (symbol-value targets)))

    (mapc (-cut add-to-list targets <>) '("DEFAULT_GOAL"
                                          "DELETE_ON_ERROR"
                                          "EXPORT_ALL_VARIABLES"
                                          "INTERMEDIATE"
                                          "LOW_RESOLUTION_TIME"
                                          "ONESHELL"
                                          "NOTPARALLEL"
                                          "POSIX"
                                          "SECONDARY"
                                          "SECONDEXPANSION"))

    (set-default targets (sort (symbol-value targets) #'string-lessp))))

(use-package markdown-mode
  :ensure
  :init
  (setq-default markdown-fontify-code-blocks-natively t
                markdown-header-scaling               t)
  :config
  (delight `((     ,#'gfm-mode "🐙" :major)
             (,#'markdown-mode "🡇"  :major))))

(use-package matlab
  :ensure matlab-mode)

(use-package mbsync
  :ensure)

(use-package meme
  :commands meme meme-file)

(use-package message
  :functions message-forward-subject-fwd message-send-mail-with-sendmail
  :bind (:map
         message-mode-map
         ("C-c C-f f" . blc-message-set-msmtp-from))
  :init
  (map-do #'add-hook
          `((message-mode-hook  . ,#'blc-message-header-fontify)
            (message-setup-hook . ,#'footnote-mode)
            (message-subscribed-address-functions
             . ,#'gnus-find-subscribed-addresses)))
  :config
  (delight #'message-mode "🖹" :major)

  (add-to-list 'message-required-mail-headers 'To)

  (when-let (addresses (blc-msmtp-addresses))
    (setq-default message-alternative-emails (regexp-opt addresses)
                  user-mail-address          (car addresses)))

  (setq-default
   message-cite-style
   (map-merge 'list message-cite-style-thunderbird
              '((message-citation-line-format
                 "On %a, %b %d %Y, at %R, %f wrote:\n")))
   message-confirm-send                  t
   message-fill-column                   60
   message-from-style                    'angles
   message-forward-before-signature      nil
   message-make-forward-subject-function #'message-forward-subject-fwd
   message-send-mail-function            #'message-send-mail-with-sendmail
   message-sendmail-envelope-from        'header
   message-signature                     user-full-name
   message-wide-reply-confirm-recipients t))

(use-package minimap
  :ensure
  :config
  (setq-default
   minimap-highlight-line  nil
   minimap-recenter-type   'relative
   minimap-width-fraction  0.05
   minimap-window-location 'right)
  (set-face-background 'minimap-active-region-background "#696969")
  (set-face-attribute  'minimap-font-face nil :height 10))

(use-package mm-decode
  :functions mm-file-name-replace-whitespace
  :config
  (add-hook 'mm-file-name-rewrite-functions #'mm-file-name-replace-whitespace)

  (setq-default
   mm-decrypt-option            'ask
   mm-default-directory         (blc-user-dir "DOWNLOAD")
   mm-external-terminal-program "x-terminal-emulator"
   mm-html-blocked-images       nil
   mm-inline-large-images       'resize
   mm-sign-option               'guided
   mm-text-html-renderer        'gnus-w3m
   mm-verify-option             'always))

(use-package mml-sec
  :init
  (setq-default mml-secure-verbose t))

(use-package "mule-cmds"
  :init
  (setq-default default-input-method "greek"))

(use-package mustache-mode
  :ensure)

(use-package mwheel
  :init
  (setq-default mwheel-tilt-scroll-p t))

(use-package nodejs-repl
  :ensure)

(use-package org
  :ensure org-plus-contrib

  :bind (:map
         mode-specific-map
         ("a" . org-agenda)
         ("c" . org-capture)
         ("l" . org-store-link))

  :init
  (add-hook 'outline-minor-mode-hook #'orgstruct-mode)

  (setq-default
   org-directory          (blc-dir user-emacs-directory "org")
   org-default-notes-file (blc-file org-directory "notes.org")
   org-modules            '(org-bibtex
                            org-bookmark
                            org-docview
                            org-eshell
                            org-eww
                            org-gnus
                            org-id
                            org-info
                            org-man))

  :config
  (mapc (-cut map-put org-babel-load-languages <> t)
        '(C
          haskell
          java
          js
          latex
          ledger
          lisp
          makefile
          ocaml
          org
          perl
          python
          scheme
          shell))

  (setq-default
   org-agenda-files                                  `(,org-default-notes-file)
   org-archive-location
   (format "%s::" (blc-file org-directory "archive.org"))
   org-babel-python-command                          "python3"
   org-catch-invisible-edits                         'smart
   org-checkbox-hierarchical-statistics              nil
   org-ctrl-k-protect-subtree                        t
   org-export-coding-system                          'utf-8
   org-footnote-section                              nil
   org-goto-interface                                'outline-path-completion
   org-goto-max-level                                10
   org-hierarchical-todo-statistics                  nil
   org-list-demote-modify-bullet                     '(("+" . "-") ("-" . "+"))
   org-list-use-circular-motion                      t
   org-log-done                                      'note
   org-log-into-drawer                               t
   org-log-redeadline                                'note
   org-log-reschedule                                'note
   org-lowest-priority                               (+ org-highest-priority 3)
   org-M-RET-may-split-line                          nil
   org-outline-path-complete-in-steps                nil
   org-refile-allow-creating-parent-nodes            'confirm
   org-refile-targets
   `((org-agenda-files . (:maxlevel . ,org-goto-max-level)))
   org-refile-use-outline-path                       'file
   org-special-ctrl-a/e                              t
   org-startup-indented                              t
   org-todo-keywords
   '((type "NEXT(n)" "TODO(t)" "EXEC(e)" "MEET(m)" "WAIT(w)" "BALK(b)" "|"
           "DONE(d!)" "VOID(v@)"))
   org-treat-S-cursor-todo-selection-as-state-change nil
   org-use-speed-commands                            t))

(use-package org-mime
  :ensure)

(use-package org-pdfview
  :ensure
  :after org
  :after pdf-view)

(use-package org-pomodoro
  :ensure
  :config
  (setq-default
   org-pomodoro-format
   (blc-sed-tree "pomodoro" "🍅" org-pomodoro-format nil t)))

(use-package org-ref
  :ensure)

(use-package outline
  :config
  (define-key outline-minor-mode-map "\C-c\t" #'blc-org-cycle))

(use-package pacmacs
  :ensure)

(use-package palette
  :ensure)

(use-package paradox
  :ensure
  :init
  (setq-default paradox-execute-asynchronously t
                paradox-github-token           t))

(use-package "paragraphs"
  :init
  (setq-default sentence-end-double-space nil))

(use-package paren
  :init
  (show-paren-mode))

(use-package paren-face
  :ensure
  :init
  (add-hook 'prog-mode-hook #'global-paren-face-mode))

(use-package pascal
  :init
  (add-hook 'pascal-mode-hook #'blc-turn-on-c++-comments))

(use-package pass
  :ensure)

(use-package passmm
  :ensure)

(use-package password-cache
  :init
  (setq-default password-cache nil))

(use-package pcomplete
  :init
  (setq-default pcomplete-ignore-case t))

(use-package pcre2el
  :ensure)

(use-package pdf-tools
  :ensure
  :bind (:map
         pdf-view-mode-map
         ("C-s" . isearch-forward))
  :init
  (setq-default pdf-view-display-size 'fit-page)
  (add-hook 'doc-view-mode-hook #'pdf-tools-install))

(use-package perl-mode
  :mode "\\.latexmkrc\\'")

(use-package perspective
  :disabled ; (void-function make-variable-frame-local)
  :ensure)

(use-package playerctl
  :ensure)

(use-package pomidor
  :ensure)

(use-package projectile
  :ensure
  :functions projectile-add-known-project projectile-save-known-projects

  :init
  (autoload 'projectile-command-map "projectile" nil t 'keymap)
  (define-key mode-specific-map "p" 'projectile-command-map)

  :config
  (when (and (require 'magit-repos nil t) (fboundp 'magit-list-repos))
    (mapc #'projectile-add-known-project (mapcar #'blc-dir (magit-list-repos)))
    (projectile-save-known-projects))

  (setq-default
   projectile-completion-system           'ivy
   projectile-find-dir-includes-top-level t
   ;; Delight mode but not project name
   projectile-mode-line
   '(:eval (format "[%s]" (if (file-remote-p default-directory)
                              "📡"
                            (projectile-project-name)))))

  (projectile-mode))

(use-package prolog
  :mode ("\\.pl\\'" . prolog-mode)
  :config
  (setq-default prolog-system 'swi))

(use-package python
  :config
  (map-do
   (lambda (var cmds)
     (when-let (cmd (seq-some #'executable-find cmds))
       (set-default var cmd)))
   '((python-check-command     . ("epylint3" "epylint" "pyflakes"))
     (python-shell-interpreter . ("ipython3" "python3" "ipython" "python")))))

(use-package rainbow-mode
  :ensure
  :delight rainbow-mode "🌈")

(use-package recentf
  :init
  ;; Do not attempt to `abbreviate-file-name' of Tramp files requiring root
  (setq-default recentf-initialize-file-name-history nil)
  (add-hook 'ivy-mode-hook #'recentf-mode)
  :config
  (run-at-time t (blc-mins-to-secs 10) 'recentf-save-list))

(use-package redtick
  :ensure
  :config
  (setq-default
   ;; Do not distract with colours - inherit `mode-line' foreground
   redtick--bars
   (mapcar (pcase-lambda (`(,interval ,bar _))
             `(,interval ,bar nil))
           redtick--bars)
   redtick-history-file
   (expand-file-name "redtick-history.txt" user-emacs-directory)))

(use-package reftex
  :init
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  :config
  (add-to-list 'reftex-default-bibliography blc-bib-file)

  (setq-default
   reftex-cite-format       'biblatex
   reftex-comment-citations t
   reftex-plug-into-AUCTeX  t
   reftex-revisit-to-follow t))

(use-package regex-tool
  :ensure)

(use-package remember
  :bind (("<f7>" . remember-notes)
         ("<f8>" . remember-notes-save-and-bury-buffer))
  :config
  (setq-default remember-notes-initial-major-mode #'org-mode))

(use-package sass-mode
  :ensure
  :init
  (add-hook 'sass-mode-hook #'blc-turn-on-c++-comments))

(use-package saveplace
  :init
  (save-place-mode))

(use-package scroll-bar
  :init
  (blc-with-every-frame #'blc-turn-off-scroll-bar))

(use-package sendmail
  :init
  (setq-default sendmail-program "msmtp"))

(use-package server
  :init
  (setq-default server-kill-new-buffers nil))

(use-package sh-script
  :config
  (setq-default sh-basic-offset 2
                sh-indentation  2))

(use-package shell
  :init
  (setq-default explicit-shell-file-name (or (getenv "ESHELL")
                                             (getenv "SHELL")
                                             "/bin/bash")))

(use-package shr
  :init
  (setq-default shr-bullet  "• "
                shr-hr-line ?─
                shr-width   blc-chars-per-line))

(use-package simple
  :commands turn-on-auto-fill
  :bind (([remap delete-horizontal-space] .   cycle-spacing)
         ([remap         capitalize-word] . capitalize-dwim)
         ([remap           downcase-word] .   downcase-dwim)
         ([remap             upcase-word] .     upcase-dwim)
         :map
         goto-map
         ("e" . first-error))

  :init
  (setq-default kill-whole-line     t
                mail-user-agent     'gnus-user-agent
                next-error-recenter '(4)
                read-mail-command   'gnus)

  (mapc (-cut add-hook <> #'turn-on-auto-fill)
        '(bookmark-edit-annotation-mode-hook LaTeX-mode-hook org-mode-hook))

  (column-number-mode))

(use-package skype
  :ensure)

(use-package sl
  :ensure)

(use-package slime-volleyball
  :ensure)

(use-package smart-mode-line
  :disabled
  :ensure)

(use-package solar
  :init
  (setq-default
   calendar-time-display-form
   '(24-hours ":" minutes (and time-zone (concat " (" time-zone ")"))))
  :config
  (when-let (loc (blc-system-location))
    (blc-solar-set-location loc)))

(use-package speedbar
  :config
  (setq-default
   speedbar-show-unknown-files t
   speedbar-update-flag        nil
   speedbar-use-images         t
   speedbar-vc-do-check        nil))

(use-package sr-speedbar
  :ensure
  :after speedbar
  :bind (:map
         ctl-x-map
         ("t" . sr-speedbar-toggle))
  :config
  (setq-default sr-speedbar-auto-refresh nil))

(use-package "startup"
  :init
  (setq inhibit-default-init    t
        inhibit-startup-screen  t)

  (when-let ((fortune
              (blc-with-contents (or (getenv "COWTUNE_FILE") "~/.cowtune")
                (let ((comment-start ";;")
                      (comment-empty-lines t)
                      delete-trailing-lines)
                  (caddr (blc-funcalls `(,#'comment-region
                                         ,#'delete-trailing-whitespace
                                         ,#'buffer-substring-no-properties)
                                       (point-min-marker)
                                       (point-max-marker)))))))
    (setq initial-scratch-message fortune))

  (mapc (-cut add-hook 'window-setup-hook <> t)
        `(,#'blc-report-init-time
          ,#'blc-gc-thresh-restore)))

(use-package subword
  :delight subword-mode
  :init
  (global-subword-mode))

(use-package sudoku
  :ensure
  :defines sudoku-builtin-puzzles)

(use-package swiper
  :ensure
  :init
  (setq-default swiper-goto-start-of-match t))

(use-package sx
  :ensure
  :bind (:map
         mode-specific-map
         ("s a" . sx-tab-all-questions  )
         ("s i" . sx-inbox              )
         ("s m" . sx-tab-meta-or-main   )
         ("s n" . sx-inbox-notifications)
         ("s s" . sx-search             ))
  :init
  (add-hook 'sx-question-list-mode-hook #'blc-sx-question-list-fontify)
  (setq-default sx-question-mode-comments-format "%s:\n   %s\n"))

(use-package systemd
  :ensure)

(use-package term
  :bind (:map
         term-mode-map
         ([remap term-char-mode] . blc-toggle-subterm-mode)
         ([remap term-line-mode] . blc-toggle-subterm-mode)
         :map
         term-raw-map
         ([remap term-char-mode] . blc-toggle-subterm-mode)
         ([remap term-line-mode] . blc-toggle-subterm-mode)))

(use-package tex
  :ensure auctex
  :bind (:map
         TeX-mode-map
         ("C-c ?" . TeX-doc))
  :commands TeX-revert-document-buffer
  :init
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  (mapc (-cut add-hook 'LaTeX-mode-hook <>)
        `(,#'blc-configure-beamer
          ,#'blc-configure-latexmk))

  :config
  (setq-default
   LaTeX-csquotes-open-quote  "\\enquote{"
   LaTeX-csquotes-close-quote "}"
   TeX-auto-save              t
   TeX-engine                 'xetex
   TeX-parse-self             t
   TeX-PDF-mode               t)

  ;; Set priority of pre-configured PDF viewers
  (mapc (lambda (nom)
          (push `(output-pdf ,nom) TeX-view-program-selection))
        (seq-intersection '("Zathura" "PDF Tools")
                          (map-keys TeX-view-program-list-builtin))))

(use-package text-mode
  :init
  (mapc (-cut add-hook 'text-mode-hook <>)
        `(,#'blc-indent-relative-first-indent-point
          ,#'blc-turn-off-electric-indent-local)))

(use-package threes
  :ensure)

(use-package tile
  :ensure
  :bind (("<f2>" . blc-tile)))

(use-package time
  :init
  (let ((fmt "%a %d %b %R %z"))
    (setq-default
     display-time-format                 fmt
     display-time-load-average-threshold 0
     display-time-mail-string            "✉"
     display-time-world-list
     (map-apply (lambda (loc props)
                  `(,(apply #'blc--location-to-tz loc props) ,loc))
                blc-locations)
     display-time-world-time-format      fmt))
  (display-time))

(use-package top-mode
  :ensure)

(use-package tramp
  :init
  (setq-default tramp-default-method "rsync"))

(use-package tuareg
  :ensure)

(use-package typit
  :ensure)

(use-package unfill
  :ensure
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package uniquify
  :init
  (setq-default uniquify-after-kill-buffer-p  nil
                uniquify-buffer-name-style    'forward
                uniquify-min-dir-content      1
                uniquify-trailing-separator-p t))

(use-package url
  :config
  (add-to-list 'url-cookie-untrusted-urls "economist\\.com"))

(use-package use-package
  :commands use-package-autoload-keymap)

(use-package vc-hooks
  :config
  ;; Git or Magit only
  (setq-default vc-handled-backends
                (when-let (git (blc-assoc vc-handled-backends 'git t))
                  `(,git))))

(use-package view
  :delight view-mode "👓")

(use-package visual-fill-column
  :ensure
  :init
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package visual-regexp-steroids
  :ensure
  :after pcre2el
  :config
  (setq-default vr/match-separator-use-custom-face t))

(use-package vlf
  :disabled
  :ensure)

(use-package w3m
  :ensure)

(use-package warnings
  :init
  (setq-default warning-minimum-log-level :debug))

(use-package wc-mode
  :ensure
  :commands wc-mode
  :config
  (setq-default wc-modeline-format "[%tll]"))

(use-package wdired
  :init
  (setq-default wdired-allow-to-change-permissions t))

(use-package web-mode
  :ensure
  :mode ("\\.html\\'" "\\.mustache\\'"))

(use-package webjump
  :bind (:map
         mode-specific-map
         ("j w" . webjump))
  :config
  (setq-default
   webjump-sites
   '(("Book Depository"
      . [simple-query "https://bookdepository.com/"
                      "https://bookdepository.com/search?searchTerm=hi"
                      ""])
     ("Emacs Wiki"
      . [simple-query "https://emacswiki.org/"
                      "https://emacswiki.org/cgi-bin/wiki/"
                      ""])
     ("GitHub"
      . [mirrors      "https://github.com/"
                      "https://github.com/basil-conto"
                      "https://github.com/issues"
                      "https://github.com/notifications"
                      "https://github.com/pulls"
                      "https://github.com/search"])
     ("Google Definition"
      . [simple-query "https://encrypted.google.com/"
                      "https://encrypted.google.com/search?q=define+"
                      "&ie=utf-8&oe=utf-8"])
     ("Google Encrypted"
      . [simple-query "https://encrypted.google.com/"
                      "https://encrypted.google.com/search?q="
                      "&ie=utf-8&oe=utf-8"])
     ("Google Scholar"
      . [simple-query "https://scholar.google.com/"
                      "https://scholar.google.com/scholar?q="
                      ""])
     ("Hoogle"
      . [simple-query "https://haskell.org/hoogle/"
                      "https://haskell.org/hoogle/?hoogle="
                      ""])
     ("IMDB"
      . [simple-query "http://imdb.com/"
                      "http://imdb.com/find?q="
                      ""])
     ("Imgur"
      .               "http://imgur.com/")
     ("Stack Overflow"
      . [simple-query "https://stackoverflow.com/"
                      "https://stackoverflow.com/search?q="
                      ""])
     ("Wikipedia"
      . [simple-query "https://en.wikipedia.org/"
                      "https://en.wikipedia.org/w/index.php?search="
                      ""]))))

(use-package wgrep
  :ensure)

(use-package whitespace
  :delight global-whitespace-mode
  :init
  (setq-default whitespace-style '(face tab-mark trailing))
  (global-whitespace-mode))

(use-package wiki-summary
  :ensure)

(use-package windmove
  :bind
  (("S-<up>"    . windmove-up   )
   ("S-<down>"  . windmove-down )
   ("S-<left>"  . windmove-left )
   ("S-<right>" . windmove-right))
  :config
  (setq-default
   windmove-window-distance-delta 2
   windmove-wrap-around           t))

(use-package "window"
  :init
  (setq-default
   display-buffer-reuse-frames     t
   scroll-error-top-bottom         t
   split-height-threshold          0
   split-window-keep-point         nil
   split-window-preferred-function #'blc-split-window
   ;; Limit automatic `display-buffer' vertical window splitting
   window-min-height               20))

(use-package winner
  :init
  (winner-mode))

(use-package with-editor
  :ensure
  :init
  ;; Clean up git buffers whether action executed or cancelled
  (mapc (-cut add-hook <> #'blc-kill-git-buffer)
        '(with-editor-post-cancel-hook
          with-editor-post-finish-hook)))

(use-package wrap-region
  :ensure
  :delight wrap-region-mode
  :commands wrap-region-add-wrapper
  :config
  (setq-default
   wrap-region-only-with-negative-prefix t
   wrap-region-tag-active-modes          '(html-mode mustache-mode web-mode))
  (wrap-region-add-wrapper "{{#i18n}}" "{{/i18n}}" "i"))

(use-package writeroom-mode
  :ensure
  :bind (:map
         writeroom-mode-map
         ("C-M-<" . writeroom-decrease-width)
         ("C-M->" . writeroom-increase-width)
         ("C-M-=" . writeroom-adjust-width  ))
  :config
  ;; Less jumpy with `auto-fill-mode'
  (setq visual-fill-column-width (+ fill-column 20)))

(use-package wttrin
  :ensure
  :config
  (setq-default
   wttrin-default-accept-language
   `(,(car wttrin-default-accept-language) . "el,en,*")
   wttrin-default-cities
   `(,"Moon" ,@(map-apply
                (pcase-lambda (loc (app (apply #'blc--country-xref) country))
                  (format "%s, %s" loc (plist-get country :name)))
                blc-locations))))

(use-package xref-js2
  :ensure
  :after js2-mode
  :init
  (with-eval-after-load 'js2-mode
    (unbind-key "M-." js2-mode-map)     ; Reused by xref
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(use-package xt-mouse
  :init
  (blc-with-every-frame #'blc-turn-on-xterm-mouse))

(use-package yaml-mode
  :ensure)

;;; init.el ends here
