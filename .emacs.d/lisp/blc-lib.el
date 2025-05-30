;;; blc-lib.el --- conveniences for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil@contovou.net>
;; Homepage: https://git.sr.ht/~blc/dotfiles

;;; Commentary:

;;; Code:

(require 'map)
(eval-when-compile
  (require 'goto-addr)
  (require 'subr-x)
  (require 'thunk))

(autoload 'dom-node         "dom")
(autoload 'shr-dom-to-xml   "shr")
(autoload 'shr-url-at-point "shr")
(autoload 'xdg-user-dir     "xdg")

(defgroup blc ()
  "Conveniences for blc."
  :group 'local)

;;; Sequences

(defun blc-keep (fn seq)
  "Map FN over SEQ and return list of non-nil results."
  (mapcan (lambda (item)
            (and-let* ((result (funcall fn item)))
              (list result)))
          seq))

(define-inline blc-get (alist key &optional default)
  "Like `alist-get', but with `equal' as TESTFN."
  (inline-quote (alist-get ,key ,alist ,default nil #'equal)))

;;; Functions

(defun blc-const (object)
  "Return variadic function which always returns OBJECT."
  (lambda (&rest _) object))

;;; Regular expressions

(defalias 'blc-sed #'replace-regexp-in-string)

(defun blc-rx (form)
  "Like `rx-to-string', but without shy groups."
  (rx-to-string form t))

(defun blc-search-forward (regexp)
  "Like `re-search-forward', but unbounded and silent."
  (re-search-forward regexp nil t))

(defun blc-tree-map (fn tree)
  "Return copy of TREE applying FN to each leaf node."
  (cond ((consp tree)
         (cons (blc-tree-map fn (car tree))
               (blc-tree-map fn (cdr tree))))
        (tree (funcall fn tree))))

(defun blc-sed-tree (regexp rep tree &optional fixedcase literal subexp)
  "Like `blc-sed', but performed recursively on TREE."
  (blc-tree-map (lambda (leaf)
                  (if (stringp leaf)
                      (blc-sed regexp rep leaf fixedcase literal subexp)
                    leaf))
                tree))

;;; DOM

(defun blc-dom-to-xml (tag &optional attributes &rest children)
  "Return DOM node with TAG, ATTRIBUTES and CHILDREN as XML."
  (shr-dom-to-xml (apply #'dom-node tag attributes children)))

;;; Datetimes

(defun blc-mins-to-secs (mins)
  "Return number of seconds in MINS."
  (* 60 mins))

(defun blc-rfc2822 (&optional time zone)
  "Like `format-time-string', but in RFC-2822 format."
  (format-time-string "%a, %d %b %Y %T %z" time zone))

;;; Files

(defmacro blc-with-contents (path &rest body)
  "Evaluate BODY in a buffer with the contents of file PATH.
Return result of last form in BODY or nil if PATH is unreadable."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (when (ignore-error file-error (insert-file-contents ,path))
       ,@body)))

(defun blc-file (&rest paths)
  "Join PATHS as a file truename."
  (file-truename
   (seq-reduce (lambda (path base)
                 (expand-file-name base path))
               paths
               default-directory)))

(defun blc-dir (&rest paths)
  "Join PATHS as a directory truename."
  (file-name-as-directory (apply #'blc-file paths)))

(defun blc-file-lessp (file1 file2)
  "Return non-nil if FILE1 precedes FILE2.
Order is breadth-first lexicographic."
  (let (cmp beg1 end1 beg2 end2)
    (while (and (setq end1 (string-search "/" file1 beg1)
                      end2 (string-search "/" file2 beg2))
                end1
                (eq t (setq cmp (compare-strings file1 beg1 end1
                                                 file2 beg2 end2))))
      (setq beg1 (1+ end1) beg2 beg1))
    (cond ((integerp cmp)
           (minusp cmp))
          (end2)
          ((not end1)
           (string-lessp file1 file2)))))

(defun blc-user-dir (dir)
  "Like `xdg-user-dir', but return directory name."
  (and-let* ((file (xdg-user-dir dir)))
    (file-name-as-directory file)))

(defvar blc-index-dir (blc-dir user-emacs-directory "index")
  "Directory containing bookmarks, caches, symlinks, etc.")

(defvar blc-dataroot-dir (file-name-parent-directory data-directory)
  "Machine-independent data root directory.")

(defun blc-dataroot-to-src (file)
  "Map FILE under `blc-dataroot-dir' to `source-directory'.
Return FILE unchanged if not under `blc-dataroot-dir'."
  (if (and (stringp file)
           (file-name-absolute-p file)
           (file-in-directory-p file blc-dataroot-dir))
      (expand-file-name (file-relative-name file blc-dataroot-dir)
                        source-directory)
    file))

(defalias 'blc-src-path
  (thunk-delay (mapcar #'blc-dataroot-to-src load-path))
  "Thunk mapping `blc-dataroot-to-src' over `load-path'.")

(defun blc-switch-to-temp-file (&optional prefix suffix)
  "Create and switch to a temporary file.
A non-empty filename PREFIX can help identify the file or its
purpose, whereas a non-empty SUFFIX will help determine the
relevant major-mode."
  (interactive "sfile prefix: \nsfile extension: ")
  (find-file (make-temp-file prefix nil (and suffix
                                             (not (string-blank-p suffix))
                                             (concat "." suffix)))))

(defun blc-system-location ()
  "Return location in `/etc/timezone' or nil."
  (blc-with-contents "/etc/timezone"
    (and (blc-search-forward (rx (group (+ nonl)) (? ?\n) eos))
         (cadr (split-string (match-string 1) "/" t)))))

(defun blc--opusenc-files (dir)
  "Return mapping of new flac to opus filenames under DIR.
See `blc-opusenc-flac'."
  (let ((flacre (rx ".flac" eos)))
    (mapcan (lambda (flac)
              (and-let* ((opus (blc-sed flacre ".opus" flac t t))
                         ((file-newer-than-file-p flac opus)))
                (list (cons flac opus))))
            (directory-files-recursively dir flacre))))

(defvar blc-opusenc-switches '("--bitrate" "128" "--quiet")
  "List of `opusenc' switches for `blc-opusenc-flac'.")

(defun blc-opusenc-flac (dir)
  "Asynchronously transcode new flac files under DIR to opus.
Only those flac files which lack a corresponding and as recent
opus file are transcoded.  At any given time, there may exist as
many opusenc processes as there are available processing units."
  (interactive (list (read-directory-name "Transcode flacs under directory: "
                                          (blc-user-dir "MUSIC") nil t)))
  (let* ((start  (current-time))
         (files  (blc--opusenc-files dir))
         (nfile  (length files))
         (npart  (max 1 (min nfile (num-processors))))
         (parts  (make-vector npart ()))
         (journo (make-progress-reporter
                  (format "Transcoding %d flac(s)..." nfile) 0 nfile)))
    ;; Partition files across all processors
    (dotimes (i nfile)
      (push (pop files) (aref parts (% i npart))))
    ;; Spawn subprocess chains
    (setq nfile 0)
    (dotimes (i npart)
      (funcall
       (seq-reduce (pcase-lambda (sentinel `(,flac . ,opus))
                     (lambda ()
                       (make-process
                        :connection-type 'pipe
                        :name "opusenc"
                        :buffer "*opusenc*"
                        :command `("opusenc" ,@blc-opusenc-switches ,flac ,opus)
                        :sentinel (lambda (proc event)
                                    (if (not (blc-process-success-p proc))
                                        (lwarn 'blc :error "opusenc: %s" event)
                                      (progress-reporter-update
                                       journo (incf nfile))
                                      (funcall sentinel))))))
                   (aref parts i)
                   (lambda ()
                     (when (>= nfile (aref (cdr journo) 2))
                       (message "%sdone (%.3fs)"
                                (aref (cdr journo) 3)
                                (float-time (time-since start))))))))))

(defun blc--url-fmt (fmt url)
  "Truncate URL for viewing and substitute in FMT string."
  (format fmt (url-truncate-url-for-viewing url (ash (frame-width) -1))))

(defvar blc-read-file-dir nil
  "Directory for `blc-read-file' to start completion in.
If nil, this variable is set to XDG_DOWNLOAD_DIR when needed.")

(defun blc-read-file (prompt &optional def)
  "Read regular file name with PROMPT in `blc-read-file-dir'.
Like `read-file-name', but intended for selecting a file to write
to with protection from accidental overwriting.  DEF is like the
INITIAL argument to `read-file-name', and is additionally used as
the base file name when a directory is selected."
  (or blc-read-file-dir (setq blc-read-file-dir (blc-user-dir "DOWNLOAD")))
  (let ((init def)
        file)
    (while (progn
             (setq file (read-file-name prompt blc-read-file-dir nil nil def))
             (when (file-directory-p file)
               (setq file (expand-file-name init file)))
             (setq blc-read-file-dir (file-name-directory file))
             (and (file-exists-p file)
                  (not (yes-or-no-p
                        (format "File `%s' exists; overwrite? " file)))))
      (setq def (file-name-nondirectory file)))
    file))

(defun blc-download (&optional url _new-window file)
  "Write contents of URL to FILE.
Try to detect any raw or image URLs at point when URL is nil.
FILE is read interactively if nil.  If FILE then names an
existing directory, URL is downloaded to a similarly named file
under that directory, as per `copy-file' et al.  NEW-WINDOW is
for compatibility with `browse-url' and ignored."
  (interactive)
  (if-let* ((url  (or url
                      (thing-at-point 'url)    ; Raw URL
                      (shr-url-at-point nil))) ; Link/image
            (file (or file
                      (blc-read-file (blc--url-fmt "Copy `%s' to: " url)
                                     (url-file-nondirectory url)))))
      (url-copy-file url file 0)
    (user-error "No URL specified or found at point")))

(defvar blc-bib-dir (blc-dir "~/.bibs")
  "Default bibliography file directory.")

(defun blc-bib-files ()
  "Return list of bibliography files under `blc-bib-dir'."
  (directory-files blc-bib-dir t (rx ".bib" eos)))

;;; Processes

(defun blc-process-success-p (proc)
  "Determine whether PROC exited successfully."
  (and (eq (process-status proc) 'exit)
       (zerop (process-exit-status proc))))

(defun blc-system-procs-by-attr (attr &optional def)
  "Return ATTR or DEF of all running processes."
  ;; Look only at local, not remote, processes.
  (let ((default-directory temporary-file-directory))
    (mapcar (lambda (pid)
              (alist-get attr (process-attributes pid) def))
            (list-system-processes))))

(defalias 'blc-msmtp-addresses
  (thunk-delay
   (blc-with-contents "~/.msmtprc"
     (let (addresses)
       (rx-let ((email (eval `(regexp ,goto-address-mail-regexp))))
         (while (blc-search-forward (rx bol "from" (+ space) (group email) eol))
           (push (match-string 1) addresses)))
       (nreverse addresses))))
  "Return list of unique addresses in ~/.msmtprc.")

(defalias 'blc-mbsync-stores
  (thunk-delay
   (blc-with-contents "~/.mbsyncrc"
     (let (stores)
       (while (blc-search-forward (rx bol "IMAPStore" (+ space)
                                      (group (+ (not space))) "-near" eol))
         (let* ((name (match-string-no-properties 1))
                (chan (blc-search-forward
                       (rx bol "Channel" (+ space) (literal name) eol))))
           (push (cons name chan) stores)))
       (nreverse stores))))
  "Return alist of store names in ~/.mbsyncrc.
Non-nil cdr means the store is connected to a channel.")

(defalias 'blc-mbsync-channels
  (thunk-delay (mapcar #'car (seq-filter #'cdr (blc-mbsync-stores))))
  "Return list of channels in ~/.mbsyncrc.")

(defalias 'blc--mbsync-args
  (thunk-delay
   (let ((chans (blc-mbsync-channels)))
     (append '("--all") chans
             (mapcan (lambda (chan)
                       (mapcar (lambda (box) (concat chan ":" box))
                               (process-lines "doveadm" "mailbox" "list"
                                              "-8su" chan)))
                     chans))))
  "Return completion table for mbsync channel/mailbox arguments.")

(defun blc-mbsync (&rest args)
  "Call mbsync with ARGS asynchronously via a shell.
Interactively, read multiple channels/mailboxes with completion."
  (interactive (let ((opts (blc--mbsync-args)))
                 (completing-read-multiple "Synchronise mbsync channels: " opts
                                           nil t nil 'blc-mbsync-history opts)))
  (let ((cmd (cons "mbsync" args)))
    (async-shell-command (mapconcat #'shell-quote-argument cmd " ")
                         (format "*%s*" (string-join cmd " ")))))

(defun blc-mbsync-all ()
  "Call `blc-mbsync' for each channel in `blc-mbsync-channels'."
  (interactive)
  (mapc #'blc-mbsync (blc-mbsync-channels)))

;;; Buffers

(defvar blc-gnus-log-buffers '("*imap log*" "*nntp-log*")
  "List of buffer names associated with Gnus logs.")

(defun blc-count-lines ()
  "Like `count-lines-page', but not limited to current page."
  (interactive)
  (let ((total  (line-number-at-pos (point-max)))
        (before (line-number-at-pos)))
    (message "Buffer has %d lines (%d + %d)" total before (- total before))))

(defun blc-derived-buffers (&rest modes)
  "Return subset of `buffer-list' derived from MODES."
  (seq-filter (lambda (buf)
                (provided-mode-derived-p
                 (buffer-local-value 'major-mode buf) modes))
              (buffer-list)))

(defun blc-bury-buffer (&optional buffer-or-name unbury)
  "Un/bury buffer.
With prefix argument UNBURY, call `unbury-buffer'.  Otherwise
pass BUFFER-OR-NAME to `bury-buffer'."
  (interactive "i\nP")
  (if unbury
      (unbury-buffer)
    (bury-buffer buffer-or-name)))

(defun blc-rename-buffer (&optional unique)
  "Like `rename-buffer', but with completion."
  (interactive "P")
  (let ((name (completing-read
               "Rename buffer (to new name): " (sort buffer-name-history)
               nil nil nil 'buffer-name-history (buffer-name))))
    (rename-buffer name unique)))

(defun blc-iwb ()
  "Indent Whole Buffer and delete trailing whitespace.
See URL `http://emacsblog.org/2007/01/17/indent-whole-buffer/'."
  (interactive)
  (let ((pmin (point-min-marker))
        (pmax (point-max-marker)))
    (delete-trailing-whitespace)
    (indent-region pmin pmax)
    (untabify      pmin pmax)))

(defun blc--open-line (n)
  "Open empty line (N - 1) lines in front of current line."
  (save-excursion
    (end-of-line n)
    (open-line 1)))

(defun blc-open-previous-line ()
  "Open empty line before current line."
  (interactive)
  (blc--open-line 0))

(defun blc-open-next-line ()
  "Open empty line after current line."
  (interactive)
  (blc--open-line 1))

(defun blc-move-line-up ()
  "Save current column and exchange current and previous lines."
  (interactive)
  (undo-boundary)
  (let ((col (current-column)))
    (transpose-lines  1)
    (forward-line    -2)
    (move-to-column col)))

(defun blc-move-line-down ()
  "Save current column and exchange current and next lines."
  (interactive)
  (undo-boundary)
  (let ((col (current-column)))
    (forward-line     1)
    (transpose-lines  1)
    (forward-line    -1)
    (move-to-column col)))

(defun blc-move-to-column (col)
  "Like `move-to-column', but extend short lines if needed."
  ;; Sync with (interactive-form #'move-to-column).
  (interactive "NMove to column: ")
  (move-to-column col t))

(defun blc-indent-relative (&optional below)
  "Indent relative to previous or next line.
Like `indent-relative', but with prefix argument BELOW, first
exchange current and next lines."
  (interactive "P")
  (when below (blc-move-line-down))
  (let ((indent-tabs-mode nil))
    (indent-relative))
  (when below (blc-move-line-up)))

(defun blc--mode-p (symbol)
  "Return non-nil if SYMBOL names an interactive mode function."
  (and (commandp symbol)
       (string-suffix-p "-mode" (symbol-name symbol))))

(defun blc-scratch (&optional mode)
  "Pop to \"*scratch*\" buffer.
If MODE is non-nil, use it instead of `initial-major-mode' in a
correspondingly named scratch buffer.  When called interactively
with a prefix argument, read MODE with completion."
  (interactive
   (when current-prefix-arg
     (list (intern (completing-read "Mode: " obarray #'blc--mode-p
                                    t nil 'extended-command-history
                                    (symbol-name initial-major-mode))))))
  (if (or (not mode) (eq mode initial-major-mode))
      (pop-to-buffer (get-scratch-buffer-create))
    (pop-to-buffer (format "*scratch-%s*"
                           (string-remove-suffix "-mode" (symbol-name mode))))
    (call-interactively mode)))

(defun blc-fortune-filter ()
  "Translate all `fortune' program markup after point.
This converts backspace escape sequences used by the `fortune'
program to text representable in Emacs.  See also the URL
`https://www.emacswiki.org/emacs/UnterminalString'."
  (while (blc-search-forward (rx (+ (in ?\^G ?\b))))
    (let ((len (- (point) (match-beginning 0)))
          (chr (preceding-char)))
      (replace-match "" t t)
      (cond ((= chr ?\^G)
             ;; Ignore ^G.
             (delete-char len))
            ((= (skip-chars-backward "_" (- (point) len)) (- len))
             ;; "__\b\b" means underline next 2 chars, but we upcase instead.
             (delete-char len)
             (upcase-region (point) (min (+ (point) len) (point-max))))
            ((< (point-min) (point) (point-max))
             ;; "'\be" gets translated to "C-x 8 ' e".
             (backward-char)
             (let ((str (buffer-substring-no-properties (point) (+ (point) 2))))
               (pcase (lookup-key #'iso-transl-ctl-x-8-map str)
                 (`[,(and (pred characterp) char)]
                  (delete-char 2)
                  (insert char)))))))))

;;; Windows

(defvar blc-other-window-action '(() (inhibit-same-window . t))
  "A `display-buffer' action for displaying in another window.")

(defvar blc-small-scroll-step 6
  "Number of lines constituting a small scroll.")

(defun blc-small-scroll-up ()
  "Like `scroll-up-command', but with a smaller default distance.
Scroll a default of `blc-small-scroll-step' lines."
  (interactive)
  (scroll-up-command (or current-prefix-arg blc-small-scroll-step)))

(defun blc-small-scroll-down ()
  "Like `scroll-down-command', but with a smaller default distance.
Scroll a default of `blc-small-scroll-step' lines."
  (interactive)
  (scroll-down-command (or current-prefix-arg blc-small-scroll-step)))

;;; Frames

(defun blc-text-frame-p (&optional frame)
  "Return non-nil if FRAME is a normal text terminal frame.
This excludes frames on the initial daemon terminal.
FRAME defaults to the selected one."
  (not (or (display-graphic-p frame)
           (string-equal "initial_terminal"
                         (terminal-name (frame-terminal frame))))))

(defun blc-make-frame (&optional params)
  "Like `make-frame', but select the new frame."
  (select-frame (make-frame params)))

(defun blc-make-graphic-display ()
  "Make a graphical frame.
Display is determined by the environment variable DISPLAY."
  (interactive)
  (make-frame-on-display (getenv "DISPLAY")))

(defun blc-delete-spare-frame (&optional frame force)
  "Delegate to `delete-frame' unless FRAME is alone in terminal."
  (unless (eq (next-frame) (selected-frame))
    (delete-frame frame force)))

(defun blc-export-frame (file &optional type &rest frames)
  "Export image data of FRAMES in TYPE format to FILE.
Interactively, prompt for FILE and TYPE with completion.  With a
prefix argument, FRAMES is all frames in Z order and TYPE is PDF.
This is a thin interactive wrapper around `x-export-frames',
which see."
  (interactive
   (let* ((fmts '("pdf" "png" "svg"))
          (fmt  (if current-prefix-arg
                    (car fmts)
                  (completing-read (format-prompt "Export as" fmts)
                                   fmts nil t nil nil fmts))))
     `(,(blc-read-file "Export to file: " (concat "emacs-scrot." fmt))
       ,(intern fmt)
       ,@(and current-prefix-arg (frame-list-z-order)))))
  (declare-function x-export-frames "xfns.c" (&optional frames type))
  (with-temp-file file
    (set-buffer-multibyte nil)
    (insert (x-export-frames frames type))))

;;; Settings

(defmacro blc-hook (&rest plists)
  "Add functions to hooks using `add-hook'.
Elements of PLISTS should form a plist with the following
recognised keys corresponding to the arguments of `add-hook':

:hooks HOOKS -- HOOKS is either a single hook variable or a list
thereof.

:fns FNS -- FNS is either a single function to be added to HOOKS
or a list thereof.

:depth DEPTH -- See `add-hook'.

:local LOCAL -- See `add-hook'."
  (declare (indent 0))
  (macroexp-progn
   (mapcan (pcase-lambda ((map :hooks :fns :depth :local))
             (mapcan (lambda (hook)
                       (mapcar (lambda (fn)
                                 `(add-hook ',hook #',fn ,depth ,local))
                               (ensure-list fns)))
                     (ensure-list hooks)))
           plists)))

(defmacro blc-define-keys (&rest alist)
  "Bind multiple keys per multiple keymaps.
Elements of ALIST should have the form (KEYMAP . BINDINGS), where
KEYMAP is an expression evaluating to a keymap.  For each element
of the alist BINDINGS of the form (KEY DEF [REMOVE]),
`define-key' is called on KEYMAP, KEY, DEF, and REMOVE."
  (declare (indent 0))
  (macroexp-progn
   (map-apply (lambda (map bindings)
                (macroexp-let2* (map)
                  (macroexp-progn
                   (map-apply (lambda (key def)
                                `(define-key ,map ,key ,(car def) ,(cadr def)))
                              bindings))))
              alist)))

(defmacro blc-with-nonce (sym where fn &rest body)
  "Run BODY with SYM temporarily advised at WHERE by FN."
  (declare (indent 3))
  (macroexp-let2* (fn)
    `(unwind-protect
         ,(macroexp-progn
           `((advice-add ',sym ,where ,fn)
             ,@body))
       (advice-remove ',sym ,fn))))

(defvar blc-locations
  '(("Athens"
     :country "GR" :lat [37 59 north] :long [23 44 east])
    ("Budapest"
     :country "HU" :lat [47 30 north] :long [19 02 east])
    ("Dublin"
     :country "IE" :lat [53 21 north] :long [06 16 west])
    ("Harare"
     :country "ZW" :lat [17 52 south] :long [31 02 east])
    ("Kfar Qasim"
     :country "IL" :lat [32 07 north] :long [34 59 east] :tz "Tel_Aviv")
    ("Lausanne"
     :country "CH" :lat [46 31 north] :long [06 38 east] :tz "Zurich")
    ("Tel Aviv"
     :country "IL" :lat [32 04 north] :long [34 47 east])
    ("Xylokastro"
     :country "GR" :lat [38 04 north] :long [22 38 east] :tz "Athens"))
  "Map location names to related properties.")

(defvar blc-countries
  '(("CH" :name "Switzerland" :area "Europe")
    ("GR" :name "Greece"      :area "Europe")
    ("HU" :name "Hungary"     :area "Europe")
    ("IE" :name "Ireland"     :area "Europe")
    ("IL" :name "Israel"      :area "Asia"  )
    ("ZW" :name "Zimbabwe"    :area "Africa"))
  "Map ISO 3166-1 alpha-2 codes to country properties.")

(defun blc--country-xref (&rest plist)
  "Lookup PLIST `:country' property in `blc-countries'."
  (blc-get blc-countries (plist-get plist :country)))

(defun blc--location-to-tz (location &rest plist)
  "Return LOCATION timezone in zoneinfo format.
LOCATION properties are looked up in `blc-locations' unless PLIST
overrides them."
  (let ((props (or plist (blc-get blc-locations location))))
    (format "%s/%s"
            (plist-get (apply #'blc--country-xref props) :area)
            (or (plist-get props :tz)
                (subst-char-in-string ?\s ?_ location)))))

(defun blc-turn-off (&rest fns)
  "Call all FNS, if valid functions, with argument 0."
  (dolist (fn fns)
    (when (functionp fn)
      (funcall fn 0))))

(defun blc-turn-off-electric-indent-local (&rest _)
  "Disable `electric-indent-local-mode'."
  (interactive)
  (blc-turn-off #'electric-indent-local-mode))

(defun blc-turn-off-indent-tabs ()
  "Locally disable tab indentation."
  (setq indent-tabs-mode nil))

(defun blc-turn-off-prettify-symbols (&rest _)
  "Disable `prettify-symbols-mode'."
  (interactive)
  (blc-turn-off #'prettify-symbols-mode))

(defun blc-turn-on-double-space-sentence-ends ()
  "Locally enable `sentence-end-double-space'."
  (setq-local sentence-end-double-space t))

(defun blc-turn-off-trailing-whitespace ()
  "Locally disable `show-trailing-whitespace'."
  (setq show-trailing-whitespace nil))

(defun blc-turn-on-line-truncation ()
  "Locally disable continuation lines."
  (setq truncate-lines t))

(defun blc-turn-on-visual-lines ()
  "Locally enable word wrap and continuation lines."
  (setq truncate-lines nil)
  (setq word-wrap t))

(defconst blc-chars-per-line 80
  "Target maximum number of characters per line.")

;;; Modes

(defalias 'blc-rainbow--faces
  (thunk-delay
   (mapcar
    (lambda (face)
      `(,(rx symbol-start (? ?@) ;; Tree-sitter capture name
             (group (literal (symbol-name face))) symbol-end)
        1 ',face))
    '(font-lock-bracket-face
      font-lock-builtin-face
      font-lock-comment-delimiter-face
      font-lock-comment-face
      font-lock-constant-face
      font-lock-delimiter-face
      font-lock-doc-face
      font-lock-doc-markup-face
      font-lock-escape-face
      font-lock-function-call-face
      font-lock-function-name-face
      font-lock-keyword-face
      font-lock-misc-punctuation-face
      font-lock-negation-char-face
      font-lock-number-face
      font-lock-operator-face
      font-lock-preprocessor-face
      font-lock-property-name-face
      font-lock-property-use-face
      font-lock-punctuation-face
      font-lock-regexp-face
      font-lock-regexp-grouping-backslash
      font-lock-regexp-grouping-construct
      font-lock-string-face
      font-lock-type-face
      font-lock-variable-name-face
      font-lock-variable-use-face
      font-lock-warning-face)))
  "Thunk with `font-lock-keywords' for `blc-rainbow-mode'.")

(define-minor-mode blc-rainbow-mode
  "Highlight font lock face variable names."
  :group 'blc
  :after-hook (font-lock-flush)
  (funcall (if blc-rainbow-mode
               #'font-lock-add-keywords
             #'font-lock-remove-keywords)
           nil
           (blc-rainbow--faces)))

(provide 'blc-lib)

;;; blc-lib.el ends here
