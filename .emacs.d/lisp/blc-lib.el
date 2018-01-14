;;; blc-lib.el --- conveniences for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://github.com/basil-conto/dotfiles

;;; Commentary:

;;; Code:

(require 'map)
(require 'seq)
(eval-when-compile
  (require 'subr-x)
  (require 'thunk))

(eval-when-compile
  (declare-function fci-mode "ext:fill-column-indicator"))

(autoload 'comint-output-filter "comint")
(autoload 'dom-node             "dom")
(autoload 'notifications-notify "notifications")
(autoload 'shell-mode           "shell")
(autoload 'shr-dom-to-xml       "shr")
(autoload 'shr-url-at-point     "shr")
(autoload 'url-get-url-at-point "url-util")
(autoload 'xdg-user-dir         "xdg")

(defgroup blc ()
  "Conveniences for blc."
  :group 'local)

;;; Sequences

(eval-and-compile
  (pcase-defmacro plist (&rest args)
    "Build a `pcase' pattern matching property list elements.
ARGS should itself form a plist whose values are bound to
elements of the `pcase' expression corresponding to its keys.
Lookup is performed using `plist-get', which always succeeds, so
this is more useful for destructuring than pattern matching."
    `(and (pred seqp)
          ,@(mapcar (pcase-lambda ((seq key var))
                      `(app (pcase--flip plist-get ,key) ,var))
                    (seq-partition args 2)))))

(defun blc-true-list-p (object)
  "Like `format-proper-list-p', but faster."
  (declare (pure t))
  (null (nthcdr (safe-length object) object)))

(defun blc-as-list (object)
  "Ensure OBJECT is either a list or within one."
  (declare (pure t))
  (funcall (if (listp object) #'identity #'list) object))

(defun blc-keep (fn seq &optional copy)
  "Map FN over SEQ and return list of non-nil results.
SEQ is modified destructively unless COPY is non-nil."
  (funcall (if copy #'seq-mapcat #'mapcan)
           (lambda (item)
             (and-let* ((result (funcall fn item)))
               (list result)))
           seq))

(defun blc-elt (map key &optional testfn default)
  "Like `map-elt', but TESTFN defaults to `equal'."
  (declare (pure t))
  (map-elt map key default (or testfn #'equal)))

(defmacro blc-put (map key value &optional testfn)
  "Like `map-put', but TESTFN defaults to `equal'."
  `(map-put ,map ,key ,value ,(or testfn '#'equal)))

;;; Regular expressions

(defalias 'blc-sed #'replace-regexp-in-string)

(defun blc-rx (form)
  "Like `rx-to-string', but without shy groups."
  (rx-to-string form t))

(defun blc-search-forward (regexp)
  "Like `re-search-forward', but unbounded and silent."
  (re-search-forward regexp nil t))

(defun blc-sed-tree (regexp rep tree &optional fixedcase literal subexp)
  "Like `blc-sed', but performed recursively on TREE."
  (let ((sed (lambda (node &optional leaf)
               (funcall (if leaf #'blc-sed #'blc-sed-tree)
                        regexp rep node fixedcase literal subexp))))
    (pcase tree
      ((pred stringp)         (funcall sed tree t))
      ((pred blc-true-list-p) (mapcar  sed tree))
      (`(,head . ,tail)       (apply #'cons (mapcar sed (list head tail))))
      (_                      tree))))

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
  (declare (indent 1))
  (macroexp-let2 nil path path
    `(when (file-readable-p ,path)
       (with-temp-buffer
         (insert-file-contents ,path)
         ,@body))))

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

(defun blc-parent-dir (path)
  "Return parent directory of absolute PATH."
  (file-name-directory (directory-file-name path)))

(defun blc-user-dir (dir)
  "Like `xdg-user-dir', but return directory name."
  (and-let* ((file (xdg-user-dir dir)))
    (file-name-as-directory file)))

(defvar blc-dataroot-dir (blc-parent-dir data-directory)
  "Machine-independent data root directory.")

(defun blc-dataroot-to-src (file)
  "Map FILE under `blc-dataroot-dir' to `source-directory'.
Return FILE unchanged if not under `blc-dataroot-dir'."
  (if (and (stringp file)
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

(defalias 'blc-msmtp-addresses
  (thunk-delay
   (blc-with-contents "~/.msmtprc"
     (let (addresses)
       (while (blc-search-forward
               (rx bol "account" (+ space) (group (+ (not space))) eol))
         (when (blc-search-forward
                (rx bol "from" (+ space)
                    (group (+ (not space)) ?@ (+ (not space))
                           ?. (+ (not space)))
                    eol))
           (push (match-string 1) addresses)))
       (nreverse addresses))))
  "Return list of unique addresses in ~/.msmtprc.")

(defun blc--opusenc-files (dir)
  "Return mapping of new flac to opus filenames under DIR.
See `blc-opusenc-flac'."
  (let ((flacre (rx ?. "flac" eos))
        files)
    (dolist (flac (directory-files-recursively dir flacre) files)
      (when-let* ((opus (blc-sed flacre ".opus" flac t t))
                  ((file-newer-than-file-p flac opus)))
        (push (cons flac opus) files)))))

(defvar blc-opusenc-switches '("--bitrate" "128" "--quiet")
  "List of `opusenc' switches for `blc-opusenc-flac'. ")

(defun blc-opusenc-flac (dir)
  "Asynchronously transcode new flac files under DIR to opus.
Only those flac files which lack a corresponding and as recent
opus file are transcoded. At any given time, there may exist as
many opusenc processes as there are available processing units."
  (interactive (list (read-directory-name "Transcode flacs under directory: "
                                          (blc-user-dir "MUSIC") nil t)))
  (let* ((start  (current-time))
         (files  (blc--opusenc-files dir))
         (nfile  (length files))
         (npart  (max 1 (min nfile
                             (string-to-number (car (process-lines "nproc"))))))
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
                                       journo (setq nfile (1+ nfile)))
                                      (funcall sentinel))))))
                   (aref parts i)
                   (lambda ()
                     (when (>= nfile (aref (cdr journo) 2))
                       (message "%sdone (%.3fs)"
                                (aref (cdr journo) 3)
                                (float-time (time-subtract nil start))))))))))

(defun blc--url-prompt (fmt url)
  "Truncate URL for viewing and substitute in FMT string.
Resulting %-occurences are escaped as %%."
  (let ((url (url-truncate-url-for-viewing url (ash (frame-width) -1))))
    (blc-sed "%" "%%" (format fmt url) t t)))

(defun blc-download (&optional url _new-window file)
  "Write contents of URL to FILE.
Try to detect any raw or image URLs at point when URL is nil.
FILE is read interactively if nil. If FILE then names an existing
directory, URL is downloaded to a similarly named file under that
directory, as per `copy-file' et al. NEW-WINDOW is for
compatibility with `browse-url' and ignored."
  (interactive)
  (if-let* ((url    (or url
                        (url-get-url-at-point)   ; Raw URL
                        (shr-url-at-point nil))) ; Link/image
            (remote (url-file-nondirectory url))
            (local  (or file
                        (read-file-name (blc--url-prompt "Copy `%s' to: " url)
                                        nil nil nil remote)))
            (file   (apply #'blc-file
                           local
                           (and (file-accessible-directory-p local)
                                (list remote)))))
      (url-copy-file url file 0)
    (user-error "No URL specified or found at point")))

;;; Processes

(defun blc-process-success-p (proc)
  "Determine whether PROC exited successfully."
  (and (eq (process-status proc) 'exit)
       (zerop (process-exit-status proc))))

(defun blc-system-procs-by-attr (attr &optional def)
  "Return ATTR or DEF of all running processes."
  (mapcar (lambda (pid)
            (map-elt (process-attributes pid) attr def))
          (list-system-processes)))

(defun blc--dropbox (shell &rest args)
  "Call external dropbox daemon with ARGS.
Use `shell-command' when SHELL is non-nil."
  (if-let* ((nom "dropbox")
            (cmd (cons nom args))
            (buf (get-buffer-create (format "*%s*" nom)))
            (shell))
      (shell-command (string-join cmd " ") buf)
    (with-current-buffer buf
      (insert (propertize (format-time-string "%F %T") 'font-lock-face 'shadow)
              ?\n)
      (make-process :name            nom
                    :buffer          buf
                    :command         cmd
                    :connection-type 'pty
                    :filter          #'comint-output-filter)
      (unless (derived-mode-p #'shell-mode)
        (shell-mode)))))

(defun blc-dropbox-status (&optional shell)
  "Print status of external dropbox daemon.
Use `shell-command' when called interactively."
  (interactive "p")
  (blc--dropbox shell "status"))

(defun blc-dropbox-start (&optional shell)
  "Start external dropbox daemon.
Use `async-shell-command' when called interactively."
  (interactive "p")
  (when (or shell
            (not (seq-some (apply-partially #'string-match-p "\\`dropbox\\'")
                           (blc-system-procs-by-attr 'comm ""))))
    (blc--dropbox shell "start" "&")))

(defun blc-dropbox-stop (&optional shell)
  "Stop external dropbox daemon.
Use `async-shell-command' when called interactively."
  (interactive "p")
  (blc--dropbox shell "stop" "&"))

;;; Buffers

(defun blc-revert-buffer ()
  "Reconcile current buffer with what lives on the disk.
Offer to revert from the auto-save file, if that exists."
  (interactive)
  (revert-buffer nil t))

(defun blc-fast-line-number ()
  "Return current line number.
Should outperform `line-number-at-pos' and scale better with
buffer size. See URL `http://emacs.stackexchange.com/a/3822' for
limitations."
  (interactive)
  (string-to-number
   (let ((line-number-display-limit-width most-positive-fixnum)
         line-number-display-limit)
     (format-mode-line "%l"))))

(defun blc-fast-line-count ()
  "Return number of lines within accessible portion of buffer.
Uses `blc-fast-line-number', which see."
  (save-excursion
    (goto-char (point-max))
    (funcall (if (bolp) #'1- #'identity)
             (blc-fast-line-number))))

(defun blc-echo-fast-line-count ()
  "Emulate `count-lines-page' using `blc-fast-line-count'."
  (interactive)
  (let* ((total   (blc-fast-line-count))
         (current (blc-fast-line-number))
         (before  (min current total))
         (after   (- total before)))
    (message "Buffer has %d lines (%d + %d)" total before after)))

(defun blc-derived-buffers (&rest modes)
  "Return subset of `buffer-list' derived from MODES."
  (seq-filter (lambda (buf)
                (with-current-buffer buf
                  (apply #'derived-mode-p modes)))
              (buffer-list)))

(defun blc-bury-buffer (&optional buffer-or-name unbury)
  "Un/bury buffer.
With prefix argument UNBURY, call `unbury-buffer'. Otherwise pass
BUFFER-OR-NAME to `bury-buffer'."
  (interactive "i\nP")
  (if unbury
      (unbury-buffer)
    (bury-buffer buffer-or-name)))

(defun blc-rename-buffer (&optional unique)
  "Like `rename-buffer', but with completion."
  (interactive "P")
  (rename-buffer (completing-read "Rename buffer (to new name): "
                                  (sort (copy-sequence buffer-name-history)
                                        #'string-lessp)
                                  nil nil nil
                                  'buffer-name-history
                                  (buffer-name))
                 unique))

(defun blc-strip-buffer ()
  "Disable features in an attempt to maximise responsiveness."
  (interactive)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (fundamental-mode)
  (blc-turn-off #'fci-mode #'font-lock-mode))

(defun blc-strip-large-buffer ()
  "Conditionally call `blc-strip-buffer'.
Strip buffer when current buffer size exceeds
`large-file-warning-threshold'."
  (when (> (buffer-size) large-file-warning-threshold)
    (blc-strip-buffer)))

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
  (move-to-column col t))

(function-put
 #'blc-move-to-column 'interactive-form (interactive-form #'move-to-column))

(defun blc-indent-relative (&optional below)
  "Indent relative to previous or next line.
Like `indent-relative', but with prefix argument BELOW, first
exchange current and next lines."
  (interactive "P")
  (when below (blc-move-line-down))
  (let (indent-tabs-mode)
    (indent-relative))
  (when below (blc-move-line-up)))

;;; Windows

(defun blc-transpose-split ()
  "Alternate between vertical and horizontal frame split.
Assumes frame is split exactly in two. Adapted from Wilfred's
function at URL
`https://www.emacswiki.org/emacs/ToggleWindowSplit'."
  (interactive)
  (unless (= (length (window-list)) 2)
    (user-error "Can only toggle a frame split in twain"))
  (let ((split (if (window-combined-p)
                   #'split-window-right
                 #'split-window-below)))
    (delete-window)
    (funcall split)
    (pop-to-buffer-same-window nil)))

(defvar blc-small-scroll-step 6
  "Number of lines constituting a small scroll.")

(defun blc-small-scroll-up (&optional step)
  "Scroll up `blc-small-scroll-step' lines."
  (interactive "P")
  (scroll-up (if step
                 (prefix-numeric-value step)
               blc-small-scroll-step)))

(defun blc-small-scroll-down (&optional step)
  "Scroll down `blc-small-scroll-step' lines."
  (interactive "P")
  (scroll-down (if step
                   (prefix-numeric-value step)
                 blc-small-scroll-step)))

;;; Frames

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
  (when (cdr (frames-on-display-list frame))
    (delete-frame frame force)))

(defun blc-with-every-frame (&rest fns)
  "Run abnormal hooks in current frame and with every new one."
  (let ((frame (selected-frame)))
    (dolist (fn fns)
      (funcall fn frame)))
  (mapc (apply-partially #'add-hook 'after-make-frame-functions) fns))

(defun blc-but-fringes (width &rest subtrahends)
  "Subtract SUBTRAHENDS and total fringe columns from WIDTH."
  (apply #'- width `(,@subtrahends ,@(mapcar #'fringe-columns '(left right)))))

(defun blc-set-font-height (height &optional frame)
  "Set font HEIGHT in 1/10 pt for FRAME.
When called interactively without a prefix argument, or when
FRAME is nil, set font height for the current as well as all new
frames."
  (interactive (list (read-face-attribute 'default :height)
                     (and current-prefix-arg (selected-frame))))
  (let ((font (format "%s-%d" (face-attribute 'default :family) (/ height 10))))
    (funcall (if frame
                 (apply-partially #'modify-frame-parameters frame)
               #'modify-all-frames-parameters)
             `((font . ,font)))))

;;; Settings

(defmacro blc-hook (&rest plists)
  "Add functions to hooks using `add-hook'.
Elements of PLISTS should form a plist with the following
recognised keys corresponding to the arguments of `add-hook':

:hooks HOOKS -- HOOKS is either a single hook variable or a list
thereof.

:fns FNS -- FNS is either a single function to be added to HOOKS
or a list thereof.

:append APPEND -- See `add-hook'.

:local LOCAL -- See `add-hook'."
  (declare (indent 0))
  (macroexp-progn
   (mapcan (pcase-lambda ((plist :hooks  hooks  :fns   fns
                                 :append append :local local))
             (mapcan (lambda (hook)
                       (mapcar (lambda (fn)
                                 `(add-hook ',hook #',fn ,append ,local))
                               (blc-as-list fns)))
                     (blc-as-list hooks)))
           plists)))

(defmacro blc-define-keys (&rest alist)
  "Bind multiple keys per multiple keymaps.
Elements of ALIST should have the form (KEYMAP . BINDINGS), where
KEYMAP is an expression evaluating to a keymap. For each element
of the alist BINDINGS of the form (KEY . DEF), `define-key' is
called on KEYMAP, KEY and DEF."
  (declare (indent 0))
  (macroexp-progn
   (map-apply (lambda (map bindings)
                (macroexp-let2 nil map map
                  (macroexp-progn
                   (map-apply (lambda (key def)
                                `(define-key ,map ,key ,def))
                              bindings))))
              alist)))

(defvar blc-locations
  '(("Athens"
     :country "GR" :lat [37 59 north] :long [23 44 east])
    ("Dublin"
     :country "IE" :lat [53 21 north] :long [06 16 west])
    ("Harare"
     :country "ZW" :lat [17 52 south] :long [31 02 east])
    ("Kfar Qasim"
     :country "IL" :lat [32 07 north] :long [34 59 east] :tz "Tel_Aviv")
    ("Tel Aviv"
     :country "IL" :lat [32 04 north] :long [34 47 east])
    ("Xylokastro"
     :country "GR" :lat [38 04 north] :long [22 38 east] :tz "Athens"))
  "Map location names to related properties.")

(defvar blc-countries
  '(("GR" :name "Greece"   :area "Europe")
    ("IE" :name "Ireland"  :area "Europe")
    ("IL" :name "Israel"   :area "Asia"  )
    ("ZW" :name "Zimbabwe" :area "Africa"))
  "Map ISO 3166-1 alpha-2 codes to country properties.")

(defun blc--country-xref (&rest plist)
  "Lookup PLIST `:country' property in `blc-countries'."
  (blc-elt blc-countries (plist-get plist :country)))

(defun blc--location-to-tz (location &rest plist)
  "Return LOCATION timezone in zoneinfo format.
LOCATION properties are looked up in `blc-locations' unless PLIST
overrides them."
  (let ((props (or plist (blc-elt blc-locations location))))
    (format "%s/%s"
            (plist-get (apply #'blc--country-xref props) :area)
            (or (plist-get props :tz)
                (subst-char-in-string ?\s ?_ location)))))

(defun blc-standard-value (var)
  "Return `standard-value' property of symbol VAR."
  (eval (car (plist-get (symbol-plist var) 'standard-value))))

(defun blc-turn-off (&rest fns)
  "Call all FNS, if valid functions, with argument 0."
  (dolist (fn fns)
    (when (functionp fn)
      (funcall fn 0))))

(defun blc-turn-off-electric-indent-local (&rest _)
  "Disable `electric-indent-local-mode'."
  (interactive)
  (blc-turn-off #'electric-indent-local-mode))

(defun blc-indent-relative-first-indent-point ()
  "Locally switch to `insert-relative-first-indent-point'."
  (setq-local indent-line-function #'indent-relative-first-indent-point))

(defun blc-turn-off-indent-tabs ()
  "Locally disable tab indentation."
  (setq indent-tabs-mode nil))

(defun blc-restore-tab-width ()
  "Restore `standard-value' of `tab-width'."
  (setq tab-width (blc-standard-value 'tab-width)))

(defun blc-gnus-summary-line-format (&rest from)
  "Return format string suitable for `gnus-summary-line-format'.
Strings FROM override the default `f' format spec."
  (apply #'concat
         `("%U"                                   ; Read status
           "%R"                                   ; Replied status
           "%z"                                   ; Article zcore
           "%O "                                  ; Download mark
           "%B"                                   ; Thread tree
           "%(%2{%-24,24" ,@(or from '("f")) "%}" ; From/To
           "%-29= : "                             ; Colon
           "%3{%-50,50s%}%)"                      ; Subject
           "%4{%9&user-date;%}"                   ; Age-sensitive data
           "\n")))

(defun blc-turn-on-lexical-binding ()
  "Locally enable lexical binding."
  (setq lexical-binding t))

(defun blc-turn-on-double-space-sentence-ends ()
  "Locally enable `sentence-end-double-space'."
  (setq-local sentence-end-double-space t))

(defun blc-turn-off-prettify-symbols (&rest _)
  "Disable `prettify-symbols-mode'."
  (interactive)
  (blc-turn-off #'prettify-symbols-mode))

(defun blc-sort-reverse (_x _y)
  "Predicate that the order of X and Y should be swapped."
  t)

(defun blc-turn-on-xterm-mouse (&optional frame &rest _)
  "Enable `xterm-mouse-mode' with first terminal frame created."
  (or (display-graphic-p frame)
      xterm-mouse-mode
      (xterm-mouse-mode)))

;;; Modes

(defvar blc-dropbox-timers ()
  "List of active timers for `blc-dropbox-mode'.")

(defvar blc-dropbox-interval (blc-mins-to-secs 30)
  "Number of seconds between dropbox start/stop runs.")

(defun blc-turn-off-dropbox-mode ()
  "Disable `blc-dropbox-mode'."
  (blc-turn-off #'blc-dropbox-mode))

(define-minor-mode blc-dropbox-mode
  "Periodically start/stop dropbox with timers."
  :global t
  :group  'blc
  (if blc-dropbox-mode
      (progn
        (setq blc-dropbox-timers
              (map-apply (lambda (off fn)
                           (run-at-time (blc-mins-to-secs off)
                                        blc-dropbox-interval
                                        fn))
                         `((1 . ,#'blc-dropbox-start)
                           (2 . ,#'blc-dropbox-stop))))
        (add-hook 'kill-emacs-hook #'blc-turn-off-dropbox-mode))
    (remove-hook 'kill-emacs-hook #'blc-turn-off-dropbox-mode)
    (while blc-dropbox-timers
      (cancel-timer (pop blc-dropbox-timers)))
    (blc-dropbox-stop)))

(defalias 'blc-rainbow--faces
  (thunk-delay
   (mapcar
    (lambda (face)
      (cons (blc-rx `(: symbol-start ,(symbol-name face) symbol-end)) face))
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

(defvar blc-tomato-timer ()
  "Active timer for `blc-tomato-mode'.")

(defvar blc-tomato-interval (blc-mins-to-secs 20)
  "Number of seconds between `blc-tomato' runs.")

(defun blc-tomato (start)
  "Send a notification about elapsed time since START."
  (notifications-notify
   :title "🍅"
   :body  (format-seconds "%H %z%M" (float-time (time-subtract nil start)))))

(define-minor-mode blc-tomato-mode
  "Run `blc-tomato' every `blc-tomato-interval'."
  :global t
  :group  'blc
  (setq blc-tomato-timer
        (if blc-tomato-mode
            (run-at-time blc-tomato-interval
                         blc-tomato-interval
                         #'blc-tomato
                         (current-time))
          (ignore (cancel-timer blc-tomato-timer)))))

;;; Constants

(defconst blc-chars-per-line 80
  "Target maximum number of characters per line.")

;;; Variables

(defvar blc-bib-file "~/.bib.bib"
  "Default user BibTeX file.")

(defvar blc-repos-dir (blc-dir user-emacs-directory "repos")
  "Directory containing symlinks to user Git repositories.")

(defvar blc-gnus-log-buffers '("*imap log*" "*nntp-log*")
  "List of buffer names associated with Gnus logs.")

(provide 'blc-lib)

;;; blc-lib.el ends here
