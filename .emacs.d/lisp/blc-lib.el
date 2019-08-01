;;; blc-lib.el --- conveniences for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://gitlab.com/basil-conto/dotfiles

;;; Commentary:

;;; Code:

(require 'map)
(require 'seq)
(eval-when-compile
  (require 'subr-x)
  (require 'thunk))

(autoload 'comint-output-filter "comint")
(autoload 'dom-node             "dom")
(autoload 'shell-mode           "shell")
(autoload 'shr-dom-to-xml       "shr")
(autoload 'shr-url-at-point     "shr")
(autoload 'xdg-user-dir         "xdg")

(defgroup blc ()
  "Conveniences for blc."
  :group 'local)

;;; Sequences

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

(defun blc-get (alist key &optional default testfn)
  "Like `alist-get', but TESTFN defaults to `equal'."
  (alist-get key alist default nil (or testfn #'equal)))

(defmacro blc-put (alist key val &optional testfn)
  "Associate KEY with VAL in ALIST and return VAL.
TESTFN is as in `alist-get'."
  `(setf (alist-get ,key ,alist nil nil ,testfn) ,val))

(defmacro blc-put* (alist key val)
  "Like `blc-put', but with `equal' as TESTFN."
  `(blc-put ,alist ,key ,val #'equal))

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

(defun blc-file-lessp (file1 file2)
  "Return non-nil if FILE1 precedes FILE2.
Order is breadth-first lexicographic."
  (let (cmp beg1 end1 beg2 end2)
    (while (and (setq end1 (string-match-p "/" file1 beg1)
                      end2 (string-match-p "/" file2 beg2))
                end1
                (eq t (setq cmp (compare-strings file1 beg1 end1
                                                 file2 beg2 end2))))
      (setq beg1 (1+ end1) beg2 beg1))
    (cond ((integerp cmp)
           (< cmp 0))
          (end2)
          ((not end1)
           (string-lessp file1 file2)))))

(defun blc-user-dir (dir)
  "Like `xdg-user-dir', but return directory name."
  (and-let* ((file (xdg-user-dir dir)))
    (file-name-as-directory file)))

(defvar blc-index-dir (blc-dir user-emacs-directory "index")
  "Directory containing bookmarks, caches, symlinks, etc.")

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
             (and (file-exists-p file)
                  (not (yes-or-no-p
                        (format "File `%s' exists; overwrite? " file)))))
      (setq blc-read-file-dir (file-name-directory file))
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
  (mapcar (lambda (pid)
            (alist-get attr (process-attributes pid) def))
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

(defvar blc-gnus-log-buffers '("*imap log*" "*nntp-log*")
  "List of buffer names associated with Gnus logs.")

(defun blc-revert-buffer ()
  "Reconcile current buffer with what lives on the disk.
Offer to revert from the auto-save file, if that exists."
  (interactive)
  (revert-buffer nil t))

(defun blc-count-lines ()
  "Like `count-lines-page', but not limited to current page."
  (interactive)
  (save-restriction
    (widen)
    (let* ((total  (line-number-at-pos (point-max)))
           (before (line-number-at-pos)))
      (message "Buffer has %d lines (%d + %d)" total before (- total before)))))

(defun blc-derived-buffers (&rest modes)
  "Return subset of `buffer-list' derived from MODES."
  (seq-filter (lambda (buf)
                (with-current-buffer buf
                  (apply #'derived-mode-p modes)))
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
  (rename-buffer (completing-read "Rename buffer (to new name): "
                                  (sort (copy-sequence buffer-name-history)
                                        #'string-lessp)
                                  nil nil nil
                                  'buffer-name-history
                                  (buffer-name))
                 unique))

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
      (pop-to-buffer (startup--get-buffer-create-scratch))
    (pop-to-buffer (format "*scratch-%s*"
                           (string-remove-suffix "-mode" (symbol-name mode))))
    (call-interactively mode)))

;;; Windows

(defvar blc-other-window-action '(() (inhibit-same-window . t))
  "A `display-buffer' action for displaying in another window.")

(defun blc-transpose-split ()
  "Alternate between vertical and horizontal frame split.
Assumes frame is split exactly in two.  Adapted from Wilfred's
function at URL
`https://www.emacswiki.org/emacs/ToggleWindowSplit'."
  (interactive)
  (unless (= (count-windows) 2)
    (user-error "Can only toggle a frame split in twain"))
  (let ((split (if (window-combined-p)
                   #'split-window-right
                 #'split-window-below)))
    (delete-window)
    (funcall split)
    (pop-to-buffer-same-window nil)))

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
   (mapcan (pcase-lambda ((map (:hooks  hooks)  (:fns   fns)
                               (:append append) (:local local)))
             (mapcan (lambda (hook)
                       (mapcar (lambda (fn)
                                 `(add-hook ',hook #',fn ,append ,local))
                               (blc-as-list fns)))
                     (blc-as-list hooks)))
           plists)))

(defmacro blc-define-keys (&rest alist)
  "Bind multiple keys per multiple keymaps.
Elements of ALIST should have the form (KEYMAP . BINDINGS), where
KEYMAP is an expression evaluating to a keymap.  For each element
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

(defmacro blc-with-nonce (sym where fn &rest body)
  "Run BODY with SYM temporarily advised at WHERE by FN."
  (declare (indent 3))
  (macroexp-let2 nil fn fn
    `(unwind-protect
         ,(macroexp-progn
           `((advice-add ',sym ,where ,fn)
             ,@body))
       (advice-remove ',sym ,fn))))

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

(defun blc-turn-on-double-space-sentence-ends ()
  "Locally enable `sentence-end-double-space'."
  (setq-local sentence-end-double-space t))

(defun blc-turn-off-trailing-whitespace ()
  "Locally disable `show-trailing-whitespace'."
  (setq show-trailing-whitespace nil))

(defun blc-turn-off-prettify-symbols (&rest _)
  "Disable `prettify-symbols-mode'."
  (interactive)
  (blc-turn-off #'prettify-symbols-mode))

(defun blc-sort-reverse (_x _y)
  "Predicate that the order of X and Y should be swapped."
  t)

(defconst blc-chars-per-line 80
  "Target maximum number of characters per line.")

;;; Modes

(defvar blc-dropbox-timers ()
  "List of active timers for `blc-dropbox-mode'.")

(defvar blc-dropbox-interval (blc-mins-to-secs 5)
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

(provide 'blc-lib)

;;; blc-lib.el ends here
