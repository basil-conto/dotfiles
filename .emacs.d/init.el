;;; init.el --- init file for blc -*- lexical-binding: t -*-

;;; Commentary:

;; TODO
;; * Define personal keymap to reduce conflicts?
;; * Add syntax highlighting for netrc files
;; * Order `use-package' keywords by their definition
;; * Use ivy with ID + title from RFC index
;; * Control cmus
;; * Create macro & special form mapper
;; * `electric-indent-inhibit' vs `blc-turn-off-local-electric-indent'
;; * Displace colour codes with relative faces
;; * Fix hard-coded load paths
;; * Write setter creator macro
;; * Improve autoloading of pdf-tools
;; * Window splitting - add minimum
;;   or customise `magit-display-buffer-function' use-case
;; * Delight modes
;; * Separate config data from logic, particularly w.r.t. sensitive data
;;   * Sniff features and explit `use-package' :requires
;; * Fix `c++-mode' `memer-init-intro' indentation

;; Explore
;; * IRC log viewing
;; * HighlightChars
;; * Outshine/outline-magic
;; * Emms
;; * OrgMobile
;; * OrgRef
;; * OfflineIMAP
;; * Quelpa
;; * Ivy
;; * Macros
;; * Org
;; * Magit
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
      (maxthresh (lsh gc-cons-threshold 6)))

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

;; Silence byte-compiler ;_;
(eval-when-compile
  (declare-function blc-report-init-time   "init")
  (declare-function blc-increase-gc-thresh "init")
  (declare-function blc-restore-gc-thresh  "init"))

;; Increase GC threshold to reduce number of GCs during initialisation
(blc-increase-gc-thresh)

;;; Packaging

;; Built-in dependencies
(require 'map)
(require 'package)
(require 'seq)
(require 'subr-x)

(eval-and-compile
  ;; Sandbox this nuisance
  (advice-add #'package--save-selected-packages :override #'ignore)

  ;; Archives
  (setq-default
   package-archive-priorities
   '(("gnu"   . 1)
     ("melpa" . 2)
     ("org"   . 3))
   package-archives
   '(("gnu"   . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org"   . "http://orgmode.org/elpa/")))

  ;; Locate and activate packages
  (package-initialize)

  ;; Third-party dependencies
  (let* ((deps    '(dash dash-functional f use-package zenburn-theme))
         (missing (seq-remove #'package-installed-p deps)))
    (when (or noninteractive            ; Byte-compiling
              (and missing
                   (y-or-n-p (format "Install missing packages %s?" missing))))
      (package-refresh-contents)
      (mapc #'package-install missing))))

(require 'bind-key)
(require 'dash)
(require 'dash-functional)
(require 'f)
(eval-when-compile
  (setq-default use-package-verbose 'debug)
  (require 'use-package))


;;;; DEFINITIONS

;;; Byte-compiler declarations

(eval-and-compile
  (map-do (lambda (file funcs)
            (mapc (-cut autoload <> file) funcs))
          '(("browse-url" . (browse-url-chrome
                             browse-url-interactive-arg))
            ("cc-defs"    . (c-langelem-pos))
            ("comint"     . (comint-mode))
            ("csv-mode"   . (csv-align-fields))
            ("dired-x"    . (dired-omit-mode))
            ("erc"        . (erc-mode))
            ("eww"        . (eww-copy-page-url))
            ("hi-lock"    . (hi-lock-set-pattern))
            ("ibuf-ext"   . (ibuffer-auto-mode
                             ibuffer-switch-to-saved-filter-groups))
            ("mailcap"    . (mailcap-extension-to-mime))
            ("message"    . (message-fetch-field
                             message-narrow-to-headers))
            ("shr"        . (shr-copy-url))
            ("smtpmail"   . (smtpmail-user-mail-address))
            ("url-util"   . (url-get-url-at-point)))))

(eval-when-compile
  (defvar c-mode-base-map)
  (defvar git-commit-filename-regexp)
  (defvar git-commit-mode)
  (defvar ivy-height)
  (defvar ivy-minibuffer-faces)
  (defvar js2-mode-map)
  (defvar LaTeX-clean-intermediate-suffixes)
  (defvar recentf-list)
  (defvar smerge-mode)
  (defvar smtpmail-smtp-user)
  (defvar TeX-command-default)
  (defvar TeX-command-list)
  (defvar whitespace-style)
  (defvar zenburn-default-colors-alist))

;;; Malformed types

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

(defun blc-symcat (&rest objects)
  "Concatenate all OBJECTS under `blc-as-string' as a symbol."
  (intern (mapconcat #'blc-as-string objects "")))

(defun blc-quote-format-string (format-string)
  "Escape format specifiers in FORMAT-STRING.
Replace %-sequences with literal percentage characters `%%' for
passing FORMAT-STRING to functions that expect a format control
string."
  (replace-regexp-in-string "%" "%%" format-string t t))

(defun blc-regexp-opt (&rest strings)
  "Remove any shy groups after applying `regexp-opt' to STRINGS."
  (string-remove-suffix
   "\\)" (string-remove-prefix
          "\\(?:" (regexp-opt strings))))

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
  (when (fboundp fun)
    (apply fun args)))

;;; Advice

;; FIXME: Use `display-buffer-alist'
(defun blc-split-larger-dimension--advice (split &rest args)
  "Sacrifice the larger window dimension when splitting."
  (let ((split-width-threshold       (window-height))
        (split-height-threshold (lsh (window-width) -1))) ; Adjust slightly
    (apply split args)))

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

(defun blc-hi-lock-exclude-derived-modes--advice (&rest _)
  "Exempt derived modes from hi-lock highlighting.
Include every major mode derived from the current
`hi-lock-exclude-modes' in that blacklist."
  (when-let* ((modes   'hi-lock-exclude-modes)
              (derived (apply #'derived-mode-p (symbol-value modes))))
    (add-to-list modes major-mode)))

(defvar blc-holiday-list-lut
  '(("Christmas"         . "🎄")
    ("Halloween"         . "👻")
    ("Hanukkah"          . "🕎")
    ("St. Patrick's Day" . "☘")
    ("Valentine's Day"   . "❤"))
  "Holiday string replacements.")

(defun blc-holiday-list--advice (haystack)
  "Replace holiday strings according to `blc-holiday-list-lut'."
  (let ((lut blc-holiday-list-lut))
    (blc-tree-sed (regexp-opt (map-keys lut))
                  (-compose  #'cdr (-cut assoc-string <> lut t))
                  haystack)))

(defun blc-narrow-candidate--advice (args)
  "Decrease `ivy-bibtex' entry width due to other formatting.
`ivy-bibtex-default-action' only considers `frame-width', which
does not, for example, take the effect of `ivy-format-function'
into account."
  (seq-let (entry width) args
    `(,entry ,(- width 3))))

(defun blc-set-sender--advice (send &rest args)
  "Change the sender's email address before sending mail."
  (-if-let* ((user-mail-address (mail-strip-quoted-names
                                 (save-restriction
                                   (message-narrow-to-headers)
                                   (message-fetch-field "From" t))))
             (user (-some->> (smtpmail-user-mail-address)
                             (setq user-mail-address)
                             (setq smtpmail-smtp-user))))
      (apply send args)
    (lwarn 'blc :error "Invalid `From' header")))

(defun blc-pcomplete--advice (&rest _)
  "Replace pcomplete with default completion UI."
  (completion-at-point))


;;; Modes

(defun blc-with-every-frame (&rest funs)
  "Run abnormal hooks in current frame and with every new one."
  ;; `run-hook-with-args' is not available during initialisation
  (mapc (-juxt (-cut funcall <> (selected-frame))
               (-cut add-hook 'after-make-frame-functions <>))
        funs))

(defun blc-turn-off-modes (&rest modes)
  "Attempt to pass 0 to all MODES."
  (mapc (-cut blc-apply-safe <> 0) modes))

;; TODO: Move to password-store
(defun blc-mail-ids (&optional max)
  "Return alist of IMAPS identities.
Each identity has the form (ALIAS . ADDRESS), where both key and
value are strings. The returned identities are retrieved from
`auth-sources'."
  (mapcar (-lambda ((&plist :host alias :user addr))
            `(,alias . ,addr))
          (auth-source-search
           :type    'netrc
           :require '(:port)
           :port    "imaps"
           :max     (or max 8))))

(defun blc-async-print-url--lpr (url &rest _)
  "Asynchronously print URL using `lpr-command'.
A slave Emacs process is used to start `lpr-command' with
`lpr-switches' and URL as arguments. This function is written
with a print command like `hp-print' in mind."
  (interactive "fPrint file: ")
  (apply #'async-start-process
         "LPR Print" lpr-command nil `(,@lpr-switches ,url)))

(defun blc-async-print-url--webkit (url &rest _)
  "Asynchronously print URL using `wkhtmltopdf'.
The contents of URL are converted to a temporary PDF file by
calling `wkhtmltopdf' from a slave Emacs process before the
generated file is printed with `blc-async-print-url--lpr'."
  (let ((tmp (make-temp-file "blc-" nil ".pdf")))
    (async-start-process "WebKit Print"
                         "wkhtmltopdf"
                         (-cut blc-async-print-url--lpr tmp)
                         url
                         tmp)))

(defun blc-print-url--selector (url)
  "Return appropriate printer and filter for URL.
The two values are returned as the CAR and CDR of a cons cell,
respectively."
  (if-let ((mimetype (mailcap-extension-to-mime (url-file-extension url)))
           (remote   (url-handler-file-remote-p url)))
      (if (string-match-p
           (regexp-opt '("application/pdf" "application/postscript")) mimetype)
          `(,#'blc-async-print-url--lpr . ,#'url-file-local-copy)
        `(,#'blc-async-print-url--webkit . ,#'identity))
    (if (string= mimetype "text/html")
        `(,#'blc-async-print-url--webkit . ,#'identity)
      `(,#'blc-async-print-url--lpr
        . ,(-compose #'url-filename #'url-generic-parse-url)))))

;; TODO: Smarter MIME-type handling?
(defun blc-print-url (url &rest args)
  "Print contents of URL.
See `browse-url' for an explanation of the arguments."
  (interactive (browse-url-interactive-arg "URL: "))
  (pcase-let ((`(,browser . ,filter) (blc-print-url--selector url)))
    (apply browser (funcall filter url) args)))

;; TODO: Add downloader?
(defvar blc-browser-alist
  `(("EWW"                . ,#'eww-browse-url       )
    ("Firefox"            . ,#'browse-url-firefox   )
    ("XDG"                . ,#'browse-url-xdg-open  )
    ("Print"              . ,#'blc-print-url        )
    ("Chromium"           . ,#'browse-url-chromium  )
    ("Google Chrome"      . ,#'browse-url-chrome    )
    ("Elinks"             . ,#'browse-url-elinks    )
    ("Xterm text browser" . ,#'browse-url-text-xterm)
    ("Emacs text browser" . ,#'browse-url-text-emacs))
  "Map preferred browsers to their calling function.")

(defun blc-counsel-browse-url (url &rest args)
  "Read WWW browser name to open URL with completion.
See `blc-browser-alist' for known browsers and `browse-url' for a
description of the arguments to this function."
  (interactive (browse-url-interactive-arg "URL: "))
  (let* ((prompt-fmt (if (string-blank-p url)
                         "Open browser: "
                       "Open URL `%s' in: "))
         (prompt-url (url-truncate-url-for-viewing url (lsh (frame-width) -1)))
         (prompt     (blc-quote-format-string (format prompt-fmt prompt-url)))
         (browser    (ivy-read prompt
                               blc-browser-alist
                               :require-match t
                               :preselect     0
                               :caller        'blc-counsel-browse-url)))
    (apply (cdr (assoc-string browser blc-browser-alist)) url args)))

(defun blc-turn-off-dired-omit ()
  "Disable `dired-omit-mode'."
  (blc-turn-off-modes #'dired-omit-mode))

;; TODO: Disable globally?
(defun blc-turn-off-local-electric-indent (&rest _)
  "Disable `electric-indent-local-mode'."
  (interactive)
  (blc-turn-off-modes #'electric-indent-local-mode))

(defun blc-rainbow-font-lock-faces ()
  "Highlight font lock face variable names."
  (interactive)
  (font-lock-add-keywords
   nil
   (mapcar
    (lambda (face)
      `(,(regexp-opt `(,(symbol-name face)) 'symbols) . ,face))
    '(font-lock-builtin-face
      font-lock-comment-delimiter-face
      font-lock-comment-face
      font-lock-constant-face
      font-lock-doc-face
      font-lock-function-name-face
      font-lock-keyword-face
      font-lock-preprocessor-face
      font-lock-string-face
      font-lock-type-face
      font-lock-variable-name-face
      font-lock-warning-face
      font-lock-negation-char-face)))
  (font-lock-flush))

(defun blc-increase-readability ()
  "Adjust font for increased readability."
  (setq line-spacing (window-font-height))
  (text-scale-set 1))

(defun blc-eww-bookmark-save ()
  "Copy the URL of the current bookmark into the kill ring."
  (interactive)
  (if-let ((eww-data
            (get-text-property (line-beginning-position) 'eww-bookmark)))
      (eww-copy-page-url)
    (user-error "No bookmark on the current line")))

(defun blc-hi-lock-no-eof-nl ()
  "Highlight missing trailing EOF newlines."
  (hi-lock-set-pattern "^.+\\'" 'hi-red-b))

(defvar blc-ibuffer-default-group "default"
  "Name of default saved ibuffer filter group.")

(defun blc-turn-on-ibuffer-filter-groups ()
  "Enable default ibuffer filter groups.
See `blc-ibuffer-default-group'."
  (ibuffer-switch-to-saved-filter-groups blc-ibuffer-default-group))

(defun blc-download-rfc (&optional arg)
  "Download (write) current RFC buffer into `irfc-directory'."
  (interactive "P")
  (if-let ((dir (or (bound-and-true-p irfc-directory)
                    (f-expand "~")))
           (arg arg))
      (let ((default-directory dir))
        (call-interactively #'write-file))
    (write-file dir t)))

(defun blc-turn-off-makefile-tab-face ()
  "Disable tab face visualisation in `makefile-mode'."
  (setq-local whitespace-style (delq 'tabs whitespace-style)))

(defun blc-turn-off-line-numbers (&rest _)
  "Locally disable display of line numbers."
  (interactive)
  (blc-turn-off-modes #'nlinum-mode
                      #'linum-mode))

(defvar blc-org-todo-keywords
  '((("NEXT" . "n")
     ("TODO" . "t")
     ("MEET" . "m")
     ("WAIT" . "w@")
     ("BALK" . "b"))
    (("DONE" . "d!")
     ("VOID" . "v@")))
  "List of `org' TODO/DONE alists of keywords and selectors.
The two alists represent the TODO/DONE states, respectively.")

(defun blc-org-cycle ()
  "Call a prefixed `org-cycle'.
Without the prefix, visibility cycling in `outline-minor-mode'
and `orgstruct-mode' never seems to enter the SUBTREE state."
  (interactive)
  (org-cycle t))

(defun blc-turn-off-prettify-symbols (&rest _)
  "Disable `prettify-symbols-mode'."
  (interactive)
  (blc-turn-off-modes #'prettify-symbols-mode))

(defun blc-turn-off-flycheck (&rest _)
  "Disable `flycheck-mode'."
  (interactive)
  (blc-turn-off-modes #'flycheck-mode))

(defun blc-use-c++-comments ()
  "Default to single-line C++-style comments."
  (setq comment-start "//"
        comment-end     ""))

(defun blc-enable-disaster ()
  "Enable `disaster' in `c-mode' derivatives."
  (bind-key "C-c d" #'disaster c-mode-base-map))

(defun blc-some-recentf (&optional count)
  "Return first COUNT or 5 items in `recentf-list'."
  (seq-take recentf-list (or count (lsh ivy-height -1))))

(defun blc-align-all-csv-fields ()
  "Align all fields in the current CSV buffer."
  (csv-align-fields nil (point-min) (point-max)))

(defun blc-turn-off-cursor-blink (&optional frame &rest _)
  "Disable `blink-cursor-mode'."
  (and (display-graphic-p frame)
       blink-cursor-mode
       (blc-turn-off-modes #'blink-cursor-mode)))

(defun blc-kill-git-commit-buffer ()
  "Ensure message buffer is killed post-git-commit."
  (and git-commit-mode
       buffer-file-name
       (string-match-p git-commit-filename-regexp buffer-file-name)
       (kill-buffer)))

(defun blc-download (&optional url file)
  "Download contents of URL to a file named FILE.
Wraps `w3m-download' or emulates it when unavailable, working
with both raw URLs and links."
  (interactive "i\nF")
  (if (fboundp 'w3m-download)
      (w3m-download url file)
    (url-copy-file (or url
                       ;; Raw URL
                       (url-get-url-at-point)
                       ;; Link
                       (and (shr-copy-url)
                            (substring-no-properties (pop kill-ring))))
                   file
                   ;; Confirm existing file
                   0)))

(defun blc-account-concat (category &optional accounts acct-sep list-sep)
  "Join ACCOUNTS with their account CATEGORY.
Return a LIST-SEP-delimited (default \" \") string of account
prefixed by CATEGORY and ACCT-SEP (default \":\")."
  (let ((list-sep (or list-sep " "))
        (acct-sep (or acct-sep ":")))
    (mapconcat (lambda (account)
                 (string-join `(,category ,account) acct-sep))
               accounts
               list-sep)))

(defun blc-delight-isearch ()
  "Shorten lighter of `isearch-mode'."
  (setq isearch-mode " 🔍"))

(defun blc-turn-on-pdf-tools (&optional frame &rest _)
  "Install and enable PDF Tools on graphic FRAME."
  (when (display-graphic-p frame)
    (pdf-tools-install t t t)))

(defun blc-turn-off-scroll-bar (&optional frame &rest _)
  "Disable scroll bar."
  (with-selected-frame frame
    (blc-turn-off-modes #'toggle-scroll-bar)))

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

(defun blc-configure-beamer ()
  "Configure LaTeX Beamer intermediate suffixes."
  (add-to-list 'LaTeX-clean-intermediate-suffixes "\\.vrb"))

(defun blc-configure-latexmk ()
  "Configure Latexmk commands and intermediate suffixes."
  (let* ((exe "latexmk")
         (nom (capitalize exe)))
    (dolist (pvc '(nil t))
      (let* ((nom (format "%s%s"     nom (if pvc " PVC" "")))
             (cmd (format "%s%s %%t" exe (if pvc "-pvc -view=none" "")))
             (dsc (format "Run %s"   nom)))
        (add-to-list
         'TeX-command-list
         `(,nom                         ; Command name
           ,cmd                         ; Non-expanded shell command
           TeX-run-command              ; Process handler
           nil                          ; Confirm expanded shell command
           (latex-mode LaTeX-mode)      ; Applicable modes
           :help ,dsc))))               ; Command description

    (setq TeX-command-default nom))

  (add-to-list 'LaTeX-clean-intermediate-suffixes "\\.fdb_latexmk"))

(defun blc-indent-relative-first-indent-point ()
  "Switch `indent-line-function' to `insert-tab'."
  (setq-local indent-line-function #'indent-relative-first-indent-point))

(defun blc-turn-on-xref-js2 ()
  "Register xref-js2 backend and sanitise keymap."
  (unbind-key "M-." js2-mode-map)     ; Reused by xref
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))

(defun blc-turn-on-xterm-mouse (&optional frame &rest _)
  "Enable `xterm-mouse-mode' with first terminal frame created."
  (or (display-graphic-p frame)
      xterm-mouse-mode
      (xterm-mouse-mode)))


;;; Editing

(defun blc-fast-line-number (&optional pos)
  "Return line number at POS or current buffer location.
Should outperform `line-number-at-pos' under normal conditions,
for some definition of \"normal\".

See URL `http://emacs.stackexchange.com/a/3822' for limitations."
  (interactive)
  (let ((line-number-display-limit-width most-positive-fixnum)
        (line-number-display-limit       nil))
    (save-excursion
      (goto-char (or pos (point)))
      (string-to-number (format-mode-line "%l")))))

(defun blc-fast-line-count ()
  "Return number of lines within accessible portion of buffer.
Uses `fast-line-number', which see."
  (let ((pmax (point-max)))
    (save-excursion
      (goto-char pmax)
      (funcall (if (= pmax (line-beginning-position)) #'1- #'identity)
               (blc-fast-line-number pmax)))))

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

(defun blc-join (type &rest paths)
  "Join PATHS, potentially with a trailing slash.
If TYPE is `dir', always append a trailing slash; if it is
`file', never append a slash; otherwise, if it is `check', append
a slash when PATHS correspond to an existing directory. "
  (file-truename
   (funcall (pcase type
              ('dir   #'file-name-as-directory)
              ('check #'f-slash)
              (_      #'identity))
            (apply #'f-join paths))))

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
  (blc-turn-off-modes
   #'blc-turn-off-line-numbers
   #'font-lock-mode))

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
  (let ((suffix (unless (string-blank-p suffix) (f-swap-ext "" suffix))))
    (find-file (make-temp-file prefix nil suffix))))

(defun blc-sudo-find-file ()
  "Open current file as root via `sudo'."
  (interactive)
  (when-let ((file (and buffer-read-only buffer-file-name)))
    (find-alternate-file (format "/sudo::%s" file))))

(defun blc-make-graphic-display ()
  "Make a graphical frame.
Display is determined by the environment variable DISPLAY."
  (interactive)
  (make-frame-on-display (getenv "DISPLAY")))

;; TODO: Operate on region as well?
(defun blc-iwb ()
  "Indent Whole Buffer and delete trailing whitespace.
See URL `http://emacsblog.org/2007/01/17/indent-whole-buffer/'."
  (interactive)
  (let ((pmin (point-min-marker))
        (pmax (point-max-marker)))
    (delete-trailing-whitespace)
    (indent-region pmin pmax)
    (untabify      pmin pmax)))

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

(defvar blc-small-scroll-step 6
  "Number of lines constituting a small scroll.")

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

(defun blc-set-font (&optional frame &rest _)
  "Set font on graphic FRAME."
  (when (display-graphic-p frame)
    (set-face-attribute 'default frame :font "DejaVu Sans Mono 8")))

(defun blc-google-contacts-fontify ()
  "Customise `google-contacts-mode' faces."
  (map-do #'face-remap-add-relative
          '((google-contacts-familyname . font-lock-keyword-face)
            (google-contacts-givenname  . font-lock-keyword-face)
            (google-contacts-header     . font-lock-string-face ))))

(defun blc-man-fontify ()
  "Customise `Man-mode' faces."
  (map-do #'face-remap-add-relative
          '((Man-overstrike . font-lock-keyword-face)
            (Man-underline  . font-lock-string-face ))))

(defun blc-message-header-fontify ()
  "Customise `message-mode' header faces."
  (map-do #'face-remap-add-relative
          '((message-header-name . font-lock-builtin-face))))

(defun blc-woman-fontify ()
  "Customise `woman-mode' faces."
  (map-do #'face-remap-add-relative
          '((woman-bold   . font-lock-keyword-face)
            (woman-italic . font-lock-string-face ))))

(defun blc-zenburn-assoc-default (colour &optional default)
  "Return the `zenburn-theme' values associated with COLOURS.
For each colour name in COLOURS return its corresponding CDR slot
in `zenburn-default-colors-alist'."
  (if-let ((val (assoc-string colour zenburn-default-colors-alist)))
      (cdr val)
    default))

(defun blc-zenburn-brighten-fci ()
  "Distinguish FCI rule from background under 256 colours."
  (setq-default fci-rule-color (blc-zenburn-assoc-default 'zenburn-bg+1)))

(defun blc-zenburn-darken-ivy ()
  "Darken background of `ivy' matches under `zenburn-theme'."
  (seq-mapn
   #'set-face-background
   (cdr ivy-minibuffer-faces)
   (mapcar #'blc-zenburn-assoc-default
           '(zenburn-red-4 zenburn-blue-4 zenburn-green-1))))

(defun blc-zenburn-darken-linum ()
  "Darken foreground of face `linum' under `zenburn-theme'."
  (set-face-foreground 'linum (blc-zenburn-assoc-default 'zenburn-bg+3)))

(defun blc-zenburn-fontify-org-todo ()
  "Customise `org' TODO keyword faces."
  (setq-default
   org-todo-keyword-faces
   (seq-mapn #'cons
             (mapcan #'map-keys blc-org-todo-keywords)
             (mapcar #'blc-zenburn-assoc-default
                     '(zenburn-magenta     ; NEXT
                       org-todo            ; TODO
                       zenburn-yellow      ; MEET
                       zenburn-cyan        ; WAIT
                       zenburn-fg          ; BALK
                       org-done            ; DONE
                       zenburn-blue-2))))) ; VOID

(defun blc-setup-theme-zenburn ()
  "Customise `zenburn-theme' to taste."
  (set-face-background 'highlight (blc-zenburn-assoc-default 'zenburn-bg-1))

  (map-do #'add-hook
          `((   fci-mode-hook . ,#'blc-zenburn-brighten-fci    )
            (   ivy-mode-hook . ,#'blc-zenburn-darken-ivy      )
            (nlinum-mode-hook . ,#'blc-zenburn-darken-linum    )
            (   org-load-hook . ,#'blc-zenburn-fontify-org-todo))))

;;; Constants

(defconst blc-chars-per-line 80
  "Target maximum number of characters per line.")

;;; Variables

(defvar blc-repos-dir (blc-join 'dir user-emacs-directory "repos")
  "Directory containing symlinks to user Git repositories.")

(defvar blc-bib-file "~/.bib.bib"
  "Default user BibTeX file.")

(defvar blc-fundamental-hooks
  (mapcar (-cut blc-symcat <> "-mode-hook")
          '(conf ess haskell-cabal hledger mustache prog text))
  "Hooks whose modes derive from `fundamental-mode' or nothing.")

(defvar blc-gnus-log-buffers '("*imap log*" "*nntp-log*")
  "List of buffer names associated with Gnus logs.")


;;;; MISCELLANEA

;; FIXME: Make isearch lazy highlights stand out
(let ((theme 'zenburn))
  (and (load-theme theme t)
       (blc-apply-safe (blc-symcat "blc-setup-theme-" theme))))

(blc-with-every-frame #'blc-set-font)

(defalias #'yes-or-no-p #'y-or-n-p)

(setq-default
 source-directory
 (blc-join 'dir blc-repos-dir "localsrc" "emacs")
 ;; Movement/drawing
 recenter-redisplay              nil
 scroll-conservatively           most-positive-fixnum
 scroll-margin                   1
 scroll-preserve-screen-position t
 scroll-step                     1
 ;; Spacing
 fill-column                     blc-chars-per-line
 indent-tabs-mode                nil
 tab-width                       2
 x-gtk-use-system-tooltips       nil)

;;; Bindings

(bind-keys
 ;; Alignment
 ("C-c P"       .    blc-align-punctuation)
 ;; Line
 ("C-c i"       .          indent-relative)
 ("C-x l"       . blc-echo-fast-line-count)
 ("C-x C-p"     .   blc-open-previous-line)
 ("C-x C-n"     .   blc-open-next-line    )
 ;; Frame / window / buffer
 ("C-c s"       .       blc-sudo-find-file)
 ("C-x 5 3"     . blc-make-graphic-display)
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
  :ensure
  :defer)

(use-package ac-js2
  :disabled
  :ensure)

(use-package ace-window
  :ensure
  :bind* ("M-o" . ace-window))

(use-package ag
  :ensure
  :defer
  :config
  (setq-default ag-highlight-search t)
  (add-to-list 'ag-arguments "-C 5"))

(use-package alert
  :ensure
  :defer
  :init
  (setq-default alert-default-style 'notifications))

(use-package apt-sources
  ;; FIXME
  :load-path "/usr/share/emacs/site-lisp/debian-el"
  :mode (("\\.sources\\'"                   . apt-sources-mode)
         ("sources\\.list\\'"               . apt-sources-mode)
         ("sources\\.list\\.d/.*\\.list\\'" . apt-sources-mode))
  :init
  (add-hook 'apt-sources-mode-hook #'blc-turn-off-local-electric-indent))

(use-package ascii
  :ensure
  :defer)

(use-package ascii-art-to-unicode
  :ensure
  :defer)

(use-package asm-mode
  :defer
  :config
  (setq-default asm-comment-char ?#))

(use-package auctex-latexmk
  :ensure
  :defer)

;; TODO: Configure
(use-package auth-password-store
  :ensure
  :defer)

(use-package auth-source
  :defer
  :config
  (add-to-list 'auth-sources
               (blc-join 'file user-emacs-directory "authinfo.gpg"))

  (setq-default
   auth-source-cache-expiry 900
   auth-source-debug        t)

  ;; Add SMTPS
  (map-put auth-source-protocols 'smtp '("smtp" "smtps" "25" "587")))

(use-package avy
  :ensure
  :bind (("M-]"   . avy-goto-char-timer)
         ("M-g f" . avy-goto-line      )))

(use-package babel
  :ensure
  :defer)

(use-package base16-theme
  :disabled
  :ensure)

(use-package battery
  :defer
  :init
  (add-hook 'after-init-hook #'display-battery-mode)
  :config
  (setq-default battery-mode-line-format (concat " " battery-mode-line-format)))

(use-package better-shell
  :ensure
  :defer)

(use-package bison-mode
  :ensure
  :defer)

(use-package bongo
  :ensure
  :defer)

(use-package browse-url
  :defer
  :init
  (setq-default browse-url-browser-function #'blc-counsel-browse-url))

(use-package bug-reference
  :defer
  :init
  (map-do #'add-hook
          `((prog-mode-hook . ,#'bug-reference-prog-mode)
            (text-mode-hook . ,#'bug-reference-mode     ))))

(use-package calendar
  :defer
  :init
  (setq-default
   calendar-date-style                  'iso
   calendar-christian-all-holidays-flag t
   calendar-islamic-all-holidays-flag   t))

(use-package calfw-cal
  :ensure calfw
  :bind ("<f9>" . cfw:open-diary-calendar))

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

    (map-put c-default-style 'other name))

  (advice-add #'c-lineup-arglist :before-until #'blc-c++-lambda-indent--advice))

(use-package chess
  :ensure
  :defer
  :config
  (let ((dir (package-desc-dir (cadr (assq 'chess package-alist)))))
    (setq-default
     chess-images-default-size blc-chars-per-line
     chess-images-directory    (blc-join 'dir dir "pieces" "xboard"))))

(use-package color-moccur
  :disabled
  :ensure)

(use-package color-theme-sanityinc-solarized
  :disabled
  :ensure)

(use-package color-theme-solarized
  :disabled
  :ensure
  :init
  (setq-default solarized-italic     nil
                solarized-termcolors 256)
  (load-theme 'solarized t))

(use-package comint
  :defer
  :init
  (add-hook 'comint-mode-hook #'blc-turn-off-line-numbers))

(use-package comment-dwim-2
  :ensure
  :bind ([remap comment-dwim] . comment-dwim-2))

(use-package company
  :ensure
  :bind ("M-#" . company-manual-begin)
  :config
  (setq-default company-idle-delay nil)
  (global-company-mode))

(use-package conf-mode
  :mode ("\\.dirs\\'" . conf-unix-mode)
  :init
  (add-hook 'conf-mode-hook #'blc-turn-off-local-electric-indent))

(use-package counsel
  :ensure
  :commands ivy-set-sources
  :bind
  (("C-c g"   . counsel-ag)
   ("C-c t"   . counsel-git)
   ("C-c u"   . counsel-unicode-char)
   ("C-h C-j" . counsel-describe-face)
   ("C-x C-l" . counsel-locate)
   ("C-c j d" . counsel-dired-jump)
   ("C-c j f" . counsel-file-jump)
   ([remap execute-extended-command] . counsel-M-x)
   ([remap yank-pop                ] . counsel-yank-pop)
   ([remap isearch-forward         ] . counsel-grep-or-swiper)
   ([remap describe-function       ] . counsel-describe-function)
   ([remap describe-variable       ] . counsel-describe-variable)
   ([remap info-lookup-symbol      ] . counsel-info-lookup-symbol)
   ([remap find-file               ] . counsel-find-file))
  :config
  (setq-default
   ;; Search with smart case and shell expansion
   counsel-grep-base-command "ag --nocolor \"%s\" %s"
   ;; Do not match start of input for counsel, man or org commands
   ivy-initial-inputs-alist
   (map-remove (lambda (cmd _)
                 (or (memq cmd '(man woman))
                     (string-match-p "^\\(?:org\\|counsel\\)-"
                                     (blc-as-string cmd))))
               ivy-initial-inputs-alist))

  (ivy-set-sources
   #'counsel-locate
   '((blc-some-recentf)
     (original-source))))

(use-package counsel-gtags
  :ensure
  :defer)

(use-package counsel-projectile
  :ensure
  :defer)

(use-package crontab-mode
  :ensure
  :mode "\\.cron\\(?:tab\\)??\\'" "cron\\(?:tab\\)??\\.")

(use-package csharp-mode
  :ensure
  :defer)

(use-package cssh
  :ensure
  :defer)

(use-package cus-edit
  :defer
  :init
  (when-let* ((custom (blc-join 'file user-emacs-directory "custom.el"))
              (exists (f-exists-p (setq-default custom-file custom))))
    (lwarn 'blc :warning "Custom file %s exists but is not loaded." custom)))

(use-package csv-mode
  :ensure
  :defer
  :init
  (add-hook 'csv-mode-hook #'blc-align-all-csv-fields)
  :config
  (setq-default csv-align-style 'auto))

(use-package dafny-mode
  :ensure boogie-friends
  :defer
  :init
  (mapc (-cut add-hook 'dafny-mode-hook <>)
        `(,#'blc-turn-off-local-electric-indent
          ,#'blc-turn-off-flycheck
          ,#'blc-turn-off-prettify-symbols)))

(use-package dash
  :defer
  :config
  (dash-enable-font-lock))

(use-package debbugs
  :ensure
  :defer)

(use-package debian-changelog-mode
  :ensure
  :defer)

(use-package debpaste
  :ensure
  :defer)

(use-package define-word
  :ensure
  :bind ("C-c /" . define-word-at-point))

(use-package delight
  :ensure
  :defer)

(use-package delsel
  :defer
  :init
  (delete-selection-mode))

(use-package diary-lib
  :defer
  :init
  (setq-default
   diary-comment-start     ";"
   diary-number-of-entries 3))

(use-package dictionary
  :ensure
  :defer)

(use-package dired
  :defer
  :init
  (setq-default
   dired-auto-revert-buffer t
   dired-listing-switches
   (string-join '("--almost-all"
                  "--classify"
                  "--group-directories-first"
                  "--human-readable"
                  "-l")
                " ")))

(use-package dired-aux
  :defer
  :config
  (let* ((cmds     'dired-compress-files-alist)
         (cmds-old (symbol-value cmds))
         (cmds-new '(("\\.tar\\.7z\\'" . "tar -c %i | 7zr a -si %o")
                     ("\\.7z\\'"       . "7zr a %o %i"))))
    (set-default cmds (-union cmds-new cmds-old))))

(use-package dired-x
  :bind (("C-x C-j"   . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  :init
  ;; Autoload dired-x but do not omit files by default
  (add-hook 'dired-mode-hook #'blc-turn-off-dired-omit)
  :config
  (setq-default dired-omit-files
                (string-join `("\\`\\.[^.]" ,dired-omit-files) "\\|"))

  (map-do (lambda (cmd suffs)
            (let ((patt (apply #'blc-regexp-opt suffs)))
              (add-to-list 'dired-guess-shell-alist-user
                           `(,(format "\\.%s\\'" patt) ,cmd))))
          '(("localc"   . ("ods" "xls" "xlsx"))
            ("lowriter" . ("odt" "doc" "docx"))
            ("mpv"      . ("mp4" "mkv"))
            ("pdf"      . ("pdf")))))

(use-package disaster
  :ensure
  :commands disaster
  :init
  (add-hook 'c-mode-common-hook #'blc-enable-disaster)
  :config
  (setq-default disaster-objdump "objdump -D -M att -Sl --no-show-raw-insn"))

(use-package discover-my-major
  :ensure
  :bind ("C-h C-m" . discover-my-major))

(use-package ducpel
  :ensure
  :defer)

;; FIXME: Add current project to `ebib-bib-search-dirs'
(use-package ebib
  :ensure
  :bind ("C-c e" . ebib)
  :config
  (setq-default
   ebib-bibtex-dialect 'biblatex
   ebib-use-timestamp  t)

  (add-to-list 'ebib-preload-bib-files blc-bib-file))

(use-package elisp-mode
  :defer
  :init
  (add-hook 'emacs-lisp-mode-hook #'blc-rainbow-font-lock-faces)

  (delight '((      emacs-lisp-mode "ελ" :major)
             (lisp-interaction-mode "λι" :major))))

(use-package embrace
  :ensure
  :defer)

(use-package emms
  :ensure
  :defer)

(use-package engine-mode
  :ensure
  :commands engine-mode engine/execute-search engine/get-query
  :bind-keymap ("C-x /" . engine-mode-map)
  :config
  (defengine book-depository
    "https://www.bookdepository.com/search?searchTerm=%s"
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
    "https://www.haskell.org/hoogle/?hoogle=%s"
    :keybinding "h")
  (defengine imdb
    "http://www.imdb.com/find?q=%s"
    :keybinding "i")
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "v")
  (defengine wikipedia
    "https://en.wikipedia.org/w/index.php?search=%s"
    :keybinding "w")
  (engine-mode))

(use-package eshell
  :commands eshell-pcomplete
  :init
  (add-hook 'eshell-mode-hook #'blc-turn-off-line-numbers)
  :config
  (advice-add #'eshell-pcomplete :override #'blc-pcomplete--advice))

(use-package ess
  :ensure
  :defer
  :config
  (setq-default ess-default-style   'DEFAULT
                ess-indent-from-lhs nil))

(use-package eww
  :bind (:map eww-bookmark-mode-map
              ("n" .             next-line)
              ("p" .         previous-line)
              ("w" . blc-eww-bookmark-save))
  :init
  (add-hook 'eww-mode-hook #'blc-increase-readability)
  (setq-default
   eww-search-prefix
   "https://encrypted.google.com/search?ie=utf-8&oe=utf-8&q="))

(use-package exec-path-from-shell
  :ensure
  :defer
  :config
  (mapc (-cut add-to-list 'exec-path-from-shell-variables <>)
        '("SSH_AGENT_PID" "SSH_AUTH_SOCK")))

(use-package executable
  :defer
  :init
  (add-hook 'after-save-hook
            #'executable-make-buffer-file-executable-if-script-p))

(use-package expand-region
  :ensure
  :bind ("M-+" . er/expand-region))

(use-package eyebrowse
  :ensure
  :defer)

(use-package fic-mode
  :ensure
  :defer
  :init
  (mapc (-cut add-hook <> #'fic-mode) blc-fundamental-hooks)
  :config
  (mapc (-cut add-to-list 'fic-highlighted-words <>)
        '("HACK" "KLUDGE" "NOTE" "WARN")))

(use-package figlet
  :ensure
  :defer)

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
  :ensure
  :commands turn-off-fci-mode
  :init
  ;; Enable in whitelist, disable in blacklist
  (let* ((whitelist blc-fundamental-hooks)
         (blacklist '(lisp-interaction-mode-hook org-mode-hook))
         (togglers  `(,#'turn-on-fci-mode ,#'turn-off-fci-mode))
         (togglees  `(,whitelist ,blacklist))
         (hookers   (-cut -rpartial #'add-hook <>)))
    (seq-mapn #'mapc (mapcar hookers togglers) togglees))

  (setq-default fci-rule-color  "#696969"
                fci-rule-column blc-chars-per-line))

(use-package find-file
  :defer
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
  :ensure
  :defer)

(use-package flex-mode
  :load-path "lisp"
  :mode "\\.lex\\'")

(use-package flx
  :ensure
  :defer)

(use-package font-lock
  :defer
  :init
  (setq-default font-lock-maximum-decoration t))

(use-package frame
  :defer
  :init
  (blc-with-every-frame #'blc-turn-off-cursor-blink))

(use-package free-keys
  :ensure
  :defer)

(use-package ggtags
  :ensure
  :bind (:map ggtags-mode-map
              ("M-F" . ggtags-find-reference))
  :commands ggtags-find-reference
  :config
  (unbind-key "M-<" ggtags-navigation-map) ; Such rude ;_;
  (unbind-key "M->" ggtags-navigation-map)
  (unbind-key "M-]" ggtags-mode-map))

(use-package ghc
  :ensure
  :defer
  :init
  (add-hook 'haskell-mode-hook #'ghc-init))

(use-package git-commit
  :ensure magit
  ;; Need to load package to know when to load package :(
  :mode ("/\\(?:\
\\(?:\\(?:COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\
\\|BRANCH_DESCRIPTION\\)\\'" . git-commit-mode)

  :init
  (add-hook 'with-editor-post-finish-hook #'blc-kill-git-commit-buffer)

  :config
  (setq-default git-commit-summary-max-length 50
                git-commit-fill-column        68)

  (add-to-list 'git-commit-style-convention-checks 'overlong-summary-line)

  (global-git-commit-mode))

(use-package gitconfig-mode
  :ensure
  :defer)

(use-package gitignore-mode
  :ensure
  :defer)

(use-package gnus
  :defer
  :init
  (setq-default
   gnus-home-directory user-emacs-directory
   gnus-init-file      (blc-join 'file gnus-home-directory "gnus")))

;; FIXME:
;; * Do not delete message body/citations/signature at startup or alias switch
;; * Add newline after signature
(use-package gnus-alias
  :ensure
  :bind (:map message-mode-map
              ("<f6>" . gnus-alias-select-identity))
  :init
  (setq-default
   gnus-alias-allow-forward-as-reply t
   gnus-alias-verbosity              9)

  (add-hook 'message-load-hook #'gnus-alias-init)

  :config
  (let ((ids (blc-mail-ids)))
    (setq-default
     gnus-alias-default-identity (or (caar ids) "")
     gnus-alias-identity-alist
     (map-apply (lambda (alias from)
                  `(,alias . ("" ,(format "\"%s\" <%s>" user-full-name from)
                              "" () "" ,(concat user-full-name "\n"))))
                ids))))

(use-package golden-ratio-scroll-screen
  :disabled
  :ensure
  :bind (("M-," . golden-ratio-scroll-screen-down)
         ("M-." . golden-ratio-scroll-screen-up)))

(use-package google-contacts-gnus
  :ensure google-contacts
  :after gnus
  :init
  (add-hook 'google-contacts-mode-hook #'blc-google-contacts-fontify))

(use-package google-contacts-message
  :after message)

(use-package google-maps
  :ensure
  :defer)

(use-package google-this
  :ensure
  :defer)

;; FIXME
(use-package gscholar-bibtex
  :ensure
  :defer
  :config
  (setq-default
   gscholar-bibtex-database-file  blc-bib-file
   gscholar-bibtex-default-source
   (map-contains-key gscholar-bibtex-available-sources "Google Scholar")))

(use-package hacker-typer
  :ensure
  :defer
  :config
  (require 'mm-util)                    ; ;_;
  (let* ((dir    (blc-join 'dir source-directory "src"))
         (files  (directory-files dir t "\\.c\\'" t))
         (urls   (mapcar (-cut concat "file://" <>) files))
         (double (mapcar (-cut * 2 <>) hacker-typer-random-range)))
    (setq-default
     hacker-typer-files          urls
     hacker-typer-random-range   double
     hacker-typer-show-hackerman t)))

(use-package haskell-cabal
  :ensure haskell-mode
  :defer
  :init
  (add-hook 'haskell-cabal-mode-hook #'blc-turn-off-local-electric-indent))

(use-package haskell-mode
  :ensure
  :defer)

(use-package hayoo
  :ensure
  :defer)

;; TODO: Delight
(use-package helm
  :disabled
  :ensure
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

(use-package helm-make
  :ensure
  :defer
  :init
  (setq-default
   helm-make-cache-targets      t
   helm-make-completion-method  'ivy
   helm-make-list-target-method 'qp
   helm-make-require-match      nil))

(use-package helm-pass
  :ensure
  :defer)

(use-package helm-proc
  :ensure
  :defer)

(use-package helm-projectile
  :disabled
  :ensure
  :after helm projectile
  :commands helm-projectile-on
  :config
  (helm-projectile-on))

(use-package hi-lock
  :commands turn-on-hi-lock-if-enabled
  :init
  (add-hook 'hi-lock-mode-hook #'blc-hi-lock-no-eof-nl)
  (advice-add #'turn-on-hi-lock-if-enabled
              :before #'blc-hi-lock-exclude-derived-modes--advice)
  (global-hi-lock-mode)
  :config
  (mapc (-cut add-to-list 'hi-lock-exclude-modes <>)
        `(,#'comint-mode
          ,#'completion-list-mode
          ,#'erc-mode
          ,#'eshell-mode)))

(use-package highlight-escape-sequences
  :ensure
  :defer
  :init
  (turn-on-hes-mode))

(use-package hl-line
  :defer
  :init
  (add-hook 'git-rebase-mode-hook #'hl-line-mode))

(use-package hledger-mode
  :ensure
  :mode "\\.journal\\'"
  :config
  (setq-default
   hledger-currency-string "€"
   hledger-jfile           "~/.hledger.journal"
   hledger-ratios-essential-expense-accounts
   (blc-account-concat "expenses" '("housing" "groceries"))
   hledger-ratios-liquid-asset-accounts
   (blc-account-concat "assets"   '("boi" "cash"))))

(use-package holidays
  :commands calendar-holiday-list
  :init
  (setq-default
   holiday-bahai-holidays    ()
   holiday-oriental-holidays ())
  :config
  (advice-add #'calendar-holiday-list
              :filter-return #'blc-holiday-list--advice))

(use-package i18next-wrap
  :disabled
  :load-path "lisp"
  :bind ("C-c I" . i18next-query-replace))

;; TODO:
;; * Configure:
;;   - Diff buffers
;;   - Org buffers
;;   - *LPR Print*
;;   - *Ledger Report*
;;   - *Shell Command Output*
;; * Look into:
;;   - ibuffer-git
;;   - ibuffer-projectile
;;   - ibuffer-tramp
;;   - ibuffer-vc

(use-package ibuf-ext
  :bind ([remap list-buffers] . ibuffer)
  :init
  (mapc (-cut add-hook 'ibuffer-mode-hook <>)
        `(,#'ibuffer-auto-mode
          ,#'blc-turn-on-ibuffer-filter-groups))

  :config
  ;; Define before use
  (mapc (lambda (filter)
          (push filter ibuffer-saved-filters))
        `(("package" (or (directory . ,source-directory)
                         (directory . ,(f-parent data-directory))
                         (directory . ,(f-join   package-user-dir))))
          ("REPL" (or (mode . eshell-mode)
                      (mode . inferior-emacs-lisp-mode)
                      (mode . lisp-interaction-mode)))))

  (setq-default
   ibuffer-always-compile-formats         t
   ibuffer-default-sorting-mode           'alphabetic
   ibuffer-old-time                       12
   ibuffer-saved-filter-groups
   `((,blc-ibuffer-default-group
      ("Code" (and (derived-mode . prog-mode)
                   (not (saved . "package"))
                   (not (saved . "REPL"))))
      ("Dir"  (mode . dired-mode))
      ("Doc"  (or (mode . apropos-mode)
                  (mode . help-mode)
                  (mode . Info-mode)
                  (mode . Man-mode)
                  (mode . woman-mode)))
      ("Gnus" (or (saved . "gnus")
                  (predicate
                   . (when-let ((drool (bound-and-true-p gnus-dribble-buffer)))
                       (string= (buffer-name) (buffer-name drool))))))
      ("Git"  (derived-mode . magit-mode))
      ("Img"  (mode . image-mode))
      ("Log"  (or (derived-mode . compilation-mode)
                  (mode . messages-buffer-mode)
                  (name . "\\*WoMan-Log\\*")
                  ,@(mapcar (-cut cons 'name <>)
                            blc-gnus-log-buffers)))
      ("PDF"  (mode . pdf-view-mode))
      ("Pkg"  (saved . "package"))
      ("REPL" (saved . "REPL"))
      ("TeX"  (saved . "TeX"))
      ("Text" (saved . "text document"))
      ("Web"  (saved . "web"))))
   ibuffer-show-empty-filter-groups       nil
   ibuffer-use-other-window               t))

(use-package ido
  :defer
  :init
  (setq-default ido-enable-flex-matching t))

(use-package idris-mode
  :ensure
  :defer)

(use-package ielm
  :defer
  :init
  ;; Display IELM buffer in other window by default
  (push '("\\`\\*ielm" () (inhibit-same-window . t)) display-buffer-alist))

(use-package "indent"
  :defer
  :init
  (setq-default indent-line-function #'insert-tab))

(use-package info
  :defer
  :init
  (add-hook 'Info-mode-hook #'blc-turn-off-line-numbers))

(use-package interleave
  :ensure
  :defer)

(use-package irfc
  :ensure
  :mode ("rfc[0-9]+\\.txt\\'" . irfc-mode)
  :bind (:map irfc-mode-map
              ("DEL" . scroll-down)))

(use-package "isearch"
  :defer
  :init
  (setq-default isearch-allow-scroll t)
  (add-hook 'isearch-mode-hook #'blc-delight-isearch))

(use-package isearch+
  :disabled
  :ensure)

(use-package isearch-prop
  :ensure
  :defer)

(use-package ivy
  :ensure
  :delight ivy-mode
  :commands ivy--regex-ignore-order ivy-set-sources
  :bind (([remap switch-to-buffer] . ivy-switch-buffer)
         ([remap switch-to-buffer-other-window]
          . ivy-switch-buffer-other-window)
         ("M-D"     . ivy-dispatching-done)
         ("C-c r"   . ivy-resume))
  :init
  (setq-default completing-read-function #'ivy-completing-read)

  :config
  (mapc (pcase-lambda (`(,map ,key ,val))
          (map-put (symbol-value map) key val))
        `((ivy-sort-functions-alist
           ;; Reverse parsed order
           ,#'Info-complete-menu-item
           ,(defalias 'blc-ivy-sort-reverse (-const t)
              "Predicate that the elements being compared should be swapped."))
          (ivy-re-builders-alist
           ;; Default behaviour
           t
           ,#'ivy--regex-ignore-order)))

  (setq-default
   ivy-count-format          "(%d/%d) "
   ivy-format-function       'ivy-format-function-arrow
   ivy-on-del-error-function #'ignore
   ivy-use-virtual-buffers   t)

  (ivy-mode))

(use-package ivy-bibtex
  :ensure
  :commands bibtex-completion-format-entry
  :config
  ;; Fit entries to window
  (advice-add #'bibtex-completion-format-entry
              :filter-args #'blc-narrow-candidate--advice)

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
  :ensure
  :defer)

(use-package ivy-pages
  :ensure
  :defer)

(use-package ivy-rich
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
  :defer
  :init
  (setq-default
   jit-lock-stealth-load 60
   jit-lock-stealth-time  4))

(use-package js
  :defer
  :config
  (setq-default
   js-enabled-frameworks
   (seq-intersection '(dojo javascript prototype) js-enabled-frameworks)
   js-indent-level         4
   js-switch-indent-offset 4))

(use-package js2-mode
  :ensure
  :bind (:map js2-mode-map
              ("RET" . js2-line-break))
  :mode "\\.js\\'"
  :interpreter "node" "nodejs"
  :commands js2-line-break
  :init
  (setq-default js2-bounce-indent-p t)

  (mapc (-cut add-hook 'js2-mode-hook <>)
        `(,#'js2-highlight-unused-variables-mode
          ,#'blc-turn-off-local-electric-indent))

  :config
  (setq-default
   js2-allow-rhino-new-expr-initializer nil
   js2-concat-multiline-strings         'eol
   js2-global-externs                   '("define" "location")
   js2-highlight-level                  3
   js2-include-node-externs             t
   js2-mode-assume-strict               t
   js2-skip-preprocessor-directives     t))

  ;; ;; FIXME
  ;; (map-do #'set-face-foreground
  ;;         '((js2-error             . "#ff0000")
  ;;           (js2-external-variable . "#ff0000")
  ;;           (js2-function-param    . "#5fd7af")))

  ;; (defun blc-js2-moar-colour ()
  ;;   "Further customise `js2-mode' faces."
  ;;   (interactive)
  ;;   (map-do #'set-face-foreground
  ;;           '((js2-function-call   . "#fce94f")
  ;;             (js2-object-property . "#fcaf3e")))))

(use-package js2-refactor
  :ensure
  :defer
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package js3-mode
  :ensure
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
  (map-do #'set-face-foreground
          '((js3-function-param-face    . "#ffffff")
            (js3-external-variable-face . "#ff0000")
            (js3-error-face             . "#ff0000"))))

(use-package json-mode
  :ensure
  :defer)

(use-package ledger-mode
  :ensure
  :mode "\\.ledger\\'"
  :commands ledger-pcomplete
  :config
  (advice-add #'ledger-pcomplete :override #'blc-pcomplete--advice)
  (setq-default
   ledger-post-amount-alignment-at :decimal
   ledger-use-iso-dates            t))

(use-package lisp-mode
  :defer
  :init
  (add-hook 'lisp-mode-hook #'blc-turn-off-local-electric-indent))

(use-package list-processes+
  :ensure
  :defer)

(use-package list-unicode-display
  :ensure
  :defer)

(use-package lorem-ipsum
  :ensure
  :defer)

(use-package lpr
  :defer
  :init
  (setq-default lpr-add-switches nil
                lpr-command      "hp-print"))

(use-package lunar
  :defer
  :config
  (setq-default
   lunar-phase-names
   (mapcar (lambda (name)
             (char-to-string (char-from-name (concat name " symbol") t)))
           lunar-phase-names)))

(use-package know-your-http-well
  :ensure
  :defer)

(use-package macrostep
  :ensure
  :defer)

(use-package magit
  :ensure
  :commands magit-display-buffer-same-window-except-diff-v1
  :bind ("C-x g" . magit-status)
  :init
  (setq-default magit-repository-directories `((,blc-repos-dir . 2)))
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

  (setq-default
   magit-branch-popup-show-variables t
   magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1)

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

    (set-default logargs (blc-tree-sed logre logcommits
                                       (symbol-value logargs) t t 1))

    (mapc (lambda (fmt)
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
  ;; (map-do #'set-face-foregrounds
  ;;         '((magit-dimmed . "#808080")
  ;;           (magit-hash   . "#808080"))))

(use-package magit-gh-pulls
  :disabled
  :ensure                               ; gh.el doesn't speak ssh?
  :commands turn-on-magit-gh-pulls
  :init
  (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(use-package magithub
  :ensure
  :after magit
  :commands magithub-feature-autoinject
  :init
  (setq-default magithub-debug-mode     t
                ;; KLUDGE: Enable magithub in a sandboxed state
                magithub-hub-executable "")
  :config
  ;; KLUDGE: Allow magithub to be enabled on next
  ;;         `magithub-toggle-pull-requests' or
  ;;         `magithub-toggle-issues'.
  (custom-reevaluate-setting 'magithub-hub-executable)
  (magithub-feature-autoinject t))

(use-package make-mode
  :defer
  :init
  (add-hook 'makefile-mode-hook #'blc-turn-off-makefile-tab-face)
  :config
  (setq-default makefile-macro-assign " := "))

(use-package man
  :defer
  :init
  (add-hook 'Man-mode-hook #'blc-man-fontify))

(use-package markdown-mode
  :ensure
  :bind (:map markdown-mode-map
              ("TAB" . markdown-cycle))
  :mode "\\.md\\'" "\\.markdown\\'"
  :commands markdown-cycle markdown-enter-key
  :config
  (blc-trim-before-newline #'markdown-enter-key))

(use-package matlab
  :ensure matlab-mode
  :defer)

(use-package menu-bar
  :defer
  :init
  (blc-turn-off-modes #'menu-bar-mode))

(use-package message
  :commands message-insert-formatted-citation-line message-send
  :init
  (map-do #'add-hook
          `((message-mode-hook  . ,#'blc-message-header-fontify)
            (message-setup-hook . ,#'footnote-mode)))

  (advice-add #'message-send :around #'blc-set-sender--advice)

  (setq-default message-directory (blc-join 'dir user-emacs-directory "Mail"))

  :config
  (setq-default
   message-alternative-emails     (regexp-opt (map-values (blc-mail-ids)))
   message-citation-line-format   "On %a, %b %d %Y, at %R, %f wrote:\n"
   message-citation-line-function #'message-insert-formatted-citation-line
   message-cite-reply-position    'above
   message-fill-column            66
   message-from-style             'angles
   message-signature              user-full-name))

(use-package minimap
  :ensure
  :defer
  :config
  (setq-default
   minimap-highlight-line  nil
   minimap-recenter-type   'relative
   minimap-width-fraction  0.05
   minimap-window-location 'right)
  (set-face-background 'minimap-active-region-background "#696969")
  (set-face-attribute  'minimap-font-face nil :font "DejaVu Sans Mono 1"))

(use-package mm-decode
  :defer
  :init
  (setq-default
   mm-decrypt-option            'ask
   mm-default-directory         "~/Downloads/"
   mm-external-terminal-program "gnome-terminal"
   mm-inline-large-images       'resize
   mm-sign-option               'guided
   mm-verify-option             'always))

(use-package mustache-mode
  :ensure
  :defer)

(use-package nlinum
  :ensure
  :defer)

(use-package nodejs-repl
  :ensure
  :defer)

;; KLUDGE: Force installation of newer version of built-in package
(eval-and-compile
  (unless (assq 'org package-alist)
    (map-delete package--builtins 'org)))

(use-package org
  :ensure
  :ensure org-plus-contrib
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))

  :init
  (mapc (-cut add-hook <> #'orgstruct-mode)
        '(git-commit-setup-hook outline-minor-mode-hook))

  :config
  (require 'remember)

  (let* ((loads 'org-babel-load-languages)
         (langs '(C haskell java js latex ledger lisp makefile
                    ocaml org perl python scheme shell))
         (elems (mapcar (-cut cons <> t) langs)))
    (set-default loads (map-merge 'list elems (symbol-value loads))))

  (setq-default
   org-agenda-files                                  `(,remember-data-file)
   org-archive-location                              "~/.archive::"
   org-catch-invisible-edits                         'smart
   org-checkbox-hierarchical-statistics              nil
   org-ctrl-k-protect-subtree                        t
   org-export-coding-system                          'utf-8
   org-footnote-section                              nil
   org-goto-interface                                'outline-path-completion
   org-hierarchical-todo-statistics                  nil
   org-list-demote-modify-bullet                     '(("+" . "-") ("-" . "+"))
   org-list-use-circular-motion                      t
   org-log-into-drawer                               t
   org-lowest-priority                               ?D
   org-M-RET-may-split-line                          nil
   org-outline-path-complete-in-steps                nil
   org-refile-allow-creating-parent-nodes            'confirm
   org-refile-targets
   `((nil . (:maxlevel . ,org-goto-max-level)))
   org-refile-use-outline-path                       'file
   org-special-ctrl-a/e                              t
   org-startup-indented                              t
   org-todo-keywords
   (let ((keywords (mapcar (-cut map-apply (-cut format "%s(%s)" <> <>) <>)
                           blc-org-todo-keywords)))
     `((type ,@(apply #'append (-interpose '("|") keywords)))))
   org-treat-S-cursor-todo-selection-as-state-change nil))

(use-package org-mime
  :ensure
  :defer)

(use-package org-ref
  :ensure
  :defer)

(use-package outline
  :bind ("C-c C-i" . blc-org-cycle))

(use-package pacmacs
  :ensure
  :defer)

(use-package palette
  :ensure
  :defer)

(use-package paradox
  :ensure
  :defer
  :init
  (setq-default paradox-execute-asynchronously t
                paradox-github-token           t))

(use-package "paragraphs"
  :defer
  :init
  (setq-default sentence-end-double-space nil))

(use-package paren
  :defer
  :init
  (show-paren-mode))

(use-package paren-face
  :ensure
  :defer
  :init
  (add-hook 'prog-mode-hook #'global-paren-face-mode))

(use-package pascal
  :defer
  :init
  (add-hook 'pascal-mode-hook #'blc-use-c++-comments))

(use-package pass
  :ensure
  :defer)

(use-package passmm
  :ensure
  :defer)

(use-package pcre2el
  :ensure
  :defer)

(use-package pdf-tools
  :ensure
  :defer
  :init
  (blc-with-every-frame #'blc-turn-on-pdf-tools)
  ;; (add-hook'pdf-view-mode-hook #'turn-on-auto-revert-mode)
  (setq-default pdf-view-display-size 'fit-page))

(use-package perl-mode
  :mode "\\.latexmkrc\\'")

(use-package perspective
  ;; (void-function make-variable-frame-local)
  :disabled
  :ensure
  :defer)

;; TODO: Delight
(use-package projectile
  :ensure
  :bind-keymap ("C-c p" . projectile-command-map)
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
  (when-let* ((cmds '("epylint3" "epylint" "pyflakes"))
              (cmd  (seq-some #'executable-find cmds)))
    (setq-default python-check-command cmd))

  (setq-default python-shell-interpreter "ipython3"))

(use-package rainbow-mode
  :ensure
  :defer)

(use-package recentf
  :defer
  :init
  (add-hook 'ivy-mode-hook #'recentf-mode))

(use-package reftex
  :defer
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
  :ensure
  :defer)

(use-package remember
  :bind (("<f7>" . remember-notes)
         ("<f8>" . remember-notes-save-and-bury-buffer))
  :config
  (setq-default remember-notes-initial-major-mode #'org-mode))

(use-package sass-mode
  :ensure
  :defer
  :init
  (add-hook 'sass-mode-hook #'blc-use-c++-comments))

(use-package saveplace
  :defer
  :init
  (save-place-mode))

(use-package scroll-bar
  :defer
  :init
  (blc-with-every-frame #'blc-turn-off-scroll-bar))

(use-package server
  :defer
  :init
  (setq-default server-kill-new-buffers nil))

(use-package sh-script
  :defer
  :config
  (setq-default sh-basic-offset 2
                sh-indentation  2))

(use-package shr
  :defer
  :init
  (setq-default shr-bullet  "• "
                shr-hr-line ?─
                shr-width   blc-chars-per-line))

(use-package simple
  :commands turn-on-auto-fill
  :bind
  (("C-x C-k"                       . kill-whole-line)
   ([remap delete-horizontal-space] .   cycle-spacing)
   ([remap         capitalize-word] . capitalize-dwim)
   ([remap           downcase-word] .   downcase-dwim)
   ([remap             upcase-word] .     upcase-dwim))

  :init
  (setq-default read-mail-command 'gnus)

  (mapc (-cut add-hook <> #'turn-on-auto-fill)
        '(LaTeX-mode-hook org-mode-hook))

  (add-hook 'special-mode-hook #'blc-turn-off-line-numbers)
  (with-current-buffer (messages-buffer)
    (blc-turn-off-line-numbers))

  (column-number-mode))

(use-package skype
  :ensure
  :defer)

(use-package sl
  :ensure
  :defer)

(use-package smart-mode-line
  :ensure
  :defer)

(use-package smerge-mode
  :defer
  :init
  (add-hook 'find-file-hook #'blc-sniff-smerge t))

(use-package smime
  :defer
  :init
  (setq-default smime-certificate-directory
                (blc-join 'dir message-directory "certs")))

(use-package smtpmail
  :defer
  :init
  (setq-default
   message-send-mail-function             #'smtpmail-send-it
   smtpmail-debug-info                    t
   smtpmail-debug-verb                    t
   smtpmail-queue-dir                     (blc-join 'dir message-directory
                                                    "queued-mail")
   smtpmail-smtp-server                   "smtp.gmail.com"
   smtpmail-smtp-service                  "smtps"
   smtpmail-stream-type                   'tls
   smtpmail-warn-about-unknown-extensions t))

(use-package solar
  :defer
  :init
  (setq-default
   calendar-latitude      53.3
   calendar-longitude      6.3
   calendar-location-name "Dublin, IE"))

(use-package solarized-theme
  :disabled
  :ensure)

(use-package speedbar
  :defer
  :config
  (setq-default
   speedbar-show-unknown-files t
   speedbar-update-flag        nil
   speedbar-use-images         t
   speedbar-vc-do-check        nil))

(use-package sr-speedbar
  :ensure
  :after speedbar
  :bind ("C-x t" . sr-speedbar-toggle)
  :config
  (setq-default sr-speedbar-auto-refresh nil))

(use-package "startup"
  :defer
  :init
  (setq-default
   inhibit-default-init   t
   inhibit-startup-screen t)

  (let* ((cowfile "-f$(shuf -en1 $(cowsay -l | tail -n+2))")
         (state   "$(shuf -en1 -- '' -b -d -g -p -s -t -w -y)")
         (command (format "fortune -aes | cowsay %s %s -n" cowfile state))
         (fortune (with-temp-buffer
                    (call-process-shell-command command nil t)
                    (let ((comment-empty-lines t)
                          (comment-start       ";;"))
                      (comment-region (point-min) (point-max)))
                    (delete-trailing-whitespace)
                    (insert ?\n)
                    (buffer-string))))
    (setq-default initial-scratch-message fortune))

  (mapc (-cut add-hook 'after-init-hook <> t)
        `(,#'blc-report-init-time
          ,#'blc-restore-gc-thresh)))

(use-package subword
  :defer
  :delight subword-mode
  :init
  (global-subword-mode))

(use-package sudoku
  :ensure
  :defer
  :defines sudoku-builtin-puzzles)

(use-package swiper
  :ensure
  :defer)

(use-package sx
  :ensure
  :defer)

(use-package systemd
  :ensure
  :defer)

(use-package tex
  :ensure auctex
  :bind (:map TeX-mode-map
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
  :defer
  :init
  (mapc (-cut add-hook 'text-mode-hook <>)
        `(,#'blc-indent-relative-first-indent-point
          ,#'blc-turn-off-local-electric-indent)))

(use-package time
  :defer
  :init
  (add-hook 'after-init-hook #'display-time)

  (let ((fmt "%a %d %b %R %z"))
    (setq-default
     display-time-format                 fmt
     display-time-load-average-threshold 0
     display-time-mail-string            "✉"
     display-time-world-list             '(("Europe/Dublin" "Dublin"  )
                                           ("Africa/Harare" "Harare"  )
                                           ("Europe/Athens" "Athens"  )
                                           ("Asia/Tel_Aviv" "Tel Aviv"))
     display-time-world-time-format      fmt)))

(use-package tool-bar
  :defer
  :init
  (blc-turn-off-modes #'tool-bar-mode))

(use-package top-mode
  :ensure
  :defer)

(use-package tuareg
  :ensure
  :defer)

(use-package typit
  :ensure
  :defer)

(use-package unfill
  :ensure
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package uniquify
  :defer
  :init
  (setq-default uniquify-buffer-name-style 'forward))

(use-package url
  :defer
  :config
  (add-to-list 'url-cookie-untrusted-urls "economist\\.com"))

(use-package use-package
  :commands blc-debug-use-package use-package-autoload-keymap
  :config
  (defun blc-debug-use-package ()
    "Toggle `use-package' debugging."
    (interactive)
    (setq-default use-package-debug (not use-package-debug))))

(use-package vc-hooks
  :defer
  :config
  ;; Git or Magit only
  (let ((backends (-list (assoc-string 'Git vc-handled-backends))))
    (setq-default vc-handled-backends backends)))

(use-package visual-fill-column
  :ensure
  :defer
  :init
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package visual-regexp-steroids
  :ensure
  :after pcre2el
  :defer
  :config
  (setq-default vr/match-separator-use-custom-face t))

(use-package vlf
  :disabled
  :ensure
  :defer)

(use-package w3m
  :ensure
  :defer)

(use-package wc-mode
  :ensure
  :commands wc-mode
  :config
  (setq-default wc-modeline-format "[%tll]"))

(use-package web-mode
  :ensure
  :mode ("\\.html\\'" "\\.mustache\\'"))

(use-package wgrep
  :ensure
  :defer)

(use-package whitespace
  :defer
  :delight global-whitespace-mode
  :init
  (setq-default whitespace-style '(face tabs trailing empty tab-mark))
  (global-whitespace-mode))

(use-package wiki-summary
  :ensure
  :defer)

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
  :ensure
  :delight wrap-region-mode
  :commands wrap-region-add-wrapper
  :init
  (wrap-region-global-mode)
  :config
  (setq-default
   wrap-region-only-with-negative-prefix t
   wrap-region-tag-active-modes          '(html-mode mustache-mode web-mode))
  (wrap-region-add-wrapper "{{#i18n}}" "{{/i18n}}" "i"))

(use-package writeroom-mode
  :ensure
  :bind (:map writeroom-mode-map
              ("C-M-<" . writeroom-decrease-width)
              ("C-M->" . writeroom-increase-width)
              ("C-M-=" . writeroom-adjust-width  ))
  :config
  ;; Less jumpy with `auto-fill-mode'
  (setq visual-fill-column-width (+ fill-column 20)))

(use-package wttrin
  :ensure
  :defer
  :config
  (setq-default
   wttrin-default-cities
   (mapcar (-cut string-join <> ", ")
           '(("Athens"     "Greece"  )
             ("Avoca"      "Ireland" )
             ("Dublin"     "Ireland" )
             ("Kfar Qasim" "Israel"  )
             ("Harare"     "Zimbabwe")
             (             "Moon"    )))))

(use-package xref-js2
  :ensure
  :defer
  :init
  (add-hook 'js2-mode-hook #'blc-turn-on-xref-js2))

(use-package xt-mouse
  :defer
  :init
  (blc-with-every-frame #'blc-turn-on-xterm-mouse))

(use-package yaml-mode
  :ensure
  :defer)

;;; init.el ends here
