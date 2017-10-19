;;; blc-lib.el --- conveniences for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://github.com/basil-conto/dotfiles

;;; Commentary:

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "mod" user-emacs-directory)))
(require 'realpath)

(require 'map)
(require 'seq)
(eval-when-compile
  (require 'subr-x)
  (require 'thunk))

(autoload 'dom-node       "dom")
(autoload 'shr-dom-to-xml "shr")
(autoload 'xdg-user-dir   "xdg")

(defgroup blc ()
  "Conveniences for blc."
  :group 'local)

;;; Sequences

(defun blc-true-list-p (object)
  "Like `format-proper-list-p', but faster."
  (declare (pure t))
  (null (nthcdr (safe-length object) object)))

(defun blc-as-list (object)
  "Ensure OBJECT is either a list or within one."
  (declare (pure t))
  (funcall (if (listp object) #'identity #'list) object))

(defun blc-insert-at (symbol item index)
  "Insert ITEM into sequence SYMBOL at INDEX."
  (let* ((seq  (symbol-value symbol))
         (type (if (listp seq) #'list (type-of seq))))
    (set symbol (seq-concatenate type
                                 (seq-take seq index)
                                 (funcall type item)
                                 (seq-drop seq index)))))

(defun blc-string-equal (s1 s2)
  "Like `string-equal', but case-insensitive."
  (eq t (compare-strings s1 nil nil s2 nil nil t)))

(defun blc-keep (fn seq &optional copy)
  "Map FN over SEQ and return list of non-nil results.
SEQ is modified destructively unless COPY is non-nil."
  (funcall (if copy #'seq-mapcat #'mapcan)
           (lambda (item)
             (and-let* ((result (funcall fn item)))
               `(,result)))
           seq))

(defun blc-elt (map key &optional default testfn)
  "Like `map-elt', but TESTFN defaults to `equal'."
  (declare (pure t))
  (map-elt map key default (or testfn #'equal)))

(defmacro blc-put (map key value &optional testfn)
  "Like `map-put', but TESTFN defaults to `equal'."
  `(map-put ,map ,key ,value ,(or testfn '#'equal)))

;;; Symbols

(defun blc-symcat (&rest objects)
  "Concatenate unquoted printed OBJECTS as a symbol."
  (intern (mapconcat (lambda (obj)
                       (prin1-to-string obj t))
                     objects
                     "")))

(defun blc-standard-value (var)
  "Return `standard-value' property of symbol VAR."
  (eval (car (plist-get (symbol-plist var) 'standard-value))))

;;; Functions

(defun blc-safe-funcall (fn &rest args)
  "Call FN with ARGS if FN is a valid function."
  (and (functionp fn)
       (apply #'funcall fn args)))

(defun blc--mapargs (map fns &rest args)
  "Apply FNS to ARGS using MAP."
  (funcall map
           (lambda (fn)
             (apply fn args))
           fns))

(defun blc-funcs (fns &rest args)
  "Apply FNS to ARGS for side effects only."
  (apply #'blc--mapargs #'mapc fns args))

(defun blc-funcalls (fns &rest args)
  "Apply FNS to ARGS and return list of results.
Like `run-hook-with-args', but works during initialisation and
returns results."
  (apply #'blc--mapargs #'mapcar fns args))

(defun blc-turn-off (&rest fns)
  "Call all FNS with argument 0."
  (blc-funcs fns 0))

;;; Regular expressions

(defalias 'blc-sed #'replace-regexp-in-string)

(defun blc-rx (form)
  "Like `rx-to-string', but without shy groups."
  (rx-to-string form t))

(defun blc-search-forward (regexp)
  "Like `re-search-forward', but unbounded and silent."
  (re-search-forward regexp nil t))

(defun blc-matches (needle haystack &rest nums)
  "Return matched groups NUMS of NEEDLE in HAYSTACK."
  (save-match-data
    (mapcar (lambda (num)
              (match-string-no-properties num haystack))
            (and (string-match needle haystack)
                 nums))))

(defun blc-sed-tree (regexp rep tree &optional fixedcase literal subexp)
  "Like `blc-sed', but performed recursively on TREE."
  (let ((sed (lambda (node &optional leaf)
               (funcall (if leaf #'blc-sed #'blc-sed-tree)
                        regexp rep node fixedcase literal subexp))))
    (pcase tree
      ((pred stringp)         (funcall sed tree t))
      ((pred blc-true-list-p) (mapcar  sed tree))
      (`(,head . ,tail)       (apply #'cons (mapcar sed `(,head ,tail))))
      (_                      tree))))

;;; DOM

(defun blc-dom-to-xml (tag &optional attributes &rest children)
  "Return DOM node with TAG, ATTRIBUTES and CHILDREN as XML."
  (shr-dom-to-xml (apply #'dom-node tag attributes children)))

;;; Datetimes

(defun blc-mins-to-secs (mins)
  "Convert number of seconds in MINS."
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
  (realpath-truename
   (seq-reduce (lambda (path base)
                 (expand-file-name base path))
               paths
               default-directory)))

(defun blc-dir (&rest paths)
  "Join PATHS as a directory truename."
  (file-name-as-directory (apply #'blc-file paths)))

(defun blc-parent-dir (path)
  "Return parent directory of PATH."
  (file-name-directory (directory-file-name path)))

(defun blc-user-dir (dir)
  "Like `xdg-user-dir', but return directory name."
  (and-let* ((file (xdg-user-dir dir)))
    (file-name-as-directory file)))

(defun blc-switch-to-temp-file (&optional prefix suffix)
  "Create and switch to a temporary file.
A non-empty filename PREFIX can help identify the file or its
purpose, whereas a non-empty SUFFIX will help determine the
relevant major-mode."
  (interactive "sfile prefix: \nsfile extension: ")
  (find-file (make-temp-file prefix nil (and suffix
                                             (not (string-blank-p suffix))
                                             (concat "." suffix)))))

(defalias 'blc-mbsync-maildirs
  (thunk-delay
   (blc-with-contents "~/.mbsyncrc"
     (let (maildirs)
       (while (blc-search-forward (rx bol "MaildirStore"))
         (when (blc-search-forward
                (rx bol "Path" (+ space) (group (+ (not space))) eol))
           (let ((maildir (match-string-no-properties 1)))
             (push `(,(file-name-nondirectory (directory-file-name maildir))
                     . ,(expand-file-name maildir))
                   maildirs))))
       (nreverse maildirs))))
  "Thunk with alist of maildirs to dirnames in ~/.mbsyncrc.")

(defvar blc-mbsync-history ()
  "Completion history for mbsync commands issued.")

(defun blc--mbsync-crm (prompt)
  "Read multiple mbsync channels with PROMPT.
Candidates offered are the keys of `blc-mbsync-maildirs', as well
as the catch-all value `--all', making them all valid arguments
for the mbsync executable."
  (let ((dirs `("--all" ,@(map-keys (blc-mbsync-maildirs)))))
    (completing-read-multiple
     prompt dirs nil 'confirm nil 'blc-mbsync-history dirs)))

(defun blc--mbsync-uid (file)
  "Return maildir UID of FILE or nil."
  (and (string-match (rx ",U=" (group (+ digit))) file)
       (match-string 1 file)))

(defun blc--mbsync-folders (&rest args)
  "Return a list of subdirectories of maildirs ARGS.
See `blc--mbsync-folders' for valid ARGS."
  (let ((maildirs (blc-mbsync-maildirs)))
    (seq-filter #'file-accessible-directory-p
                (mapcan (lambda (maildir)
                          (directory-files
                           maildir t directory-files-no-dot-files-regexp))
                        (mapcar (apply-partially #'blc-elt maildirs)
                                (if (member "--all" args)
                                    (map-keys maildirs)
                                  args))))))

(defun blc--mbsync-rename (window alist)
  "Rename files according to ALIST with WINDOW selected.
Prompt for confirmation before proceeding with the (FROM . TO)
rename in ALIST.

This is a subroutine of `blc-mbsync-deduplicate' and intended as
the QUIT-FUNCTION of `with-temp-buffer-window' or similar."
  (with-selected-window window
    (if-let* ((n (length alist))
              ((> n 0))
              ((y-or-n-p (format "Rename %d files? " n))))
        (dotimes-with-progress-reporter (i n)
            (format "Renaming %d files..." n)
          (pcase-let ((`(,from . ,to) (pop alist)))
            (rename-file from to)))
      (kill-buffer-and-window))))

(defun blc-mbsync-deduplicate (&rest args)
  "Deduplicate maildir UIDs in all subdirectories of ARGS.
The user is shown a transcript of pending rename operations and
asked to confirm their execution.

See `blc--mbsync-crm' for valid ARGS."
  (interactive (blc--mbsync-crm "Deduplicate mbsync maildirs: "))
  (with-current-buffer-window
   "*blc-mbsync-dups*" () #'blc--mbsync-rename
   (let* ((folders  (apply #'blc--mbsync-folders args))
          (nfolders (length folders))
          renames)
     (dotimes-with-progress-reporter (i nfolders (nreverse renames))
         (format "Scanning %d folders..." nfolders)
       (let ((map (make-hash-table :test #'equal)))
         (dolist (file (directory-files-recursively (pop folders) ""))
           (when-let* ((uid (blc--mbsync-uid file)))
             (let ((dups (gethash uid map)))
               (unless (member file dups)
                 (puthash uid (cons file dups) map)))))
         (maphash (lambda (_ dups)
                    (dolist (dup (butlast (sort dups #'file-newer-than-file-p)))
                      (let ((new (replace-regexp-in-string
                                  (rx ",U=" (+ (not (in ?,)))) "" dup t t)))
                        (insert (format "Rename %s\n    -> %s\n" dup new))
                        (push (cons dup new) renames))))
                  map))))))

(defun blc-mbsync-max-uid (folder &optional here)
  "Return largest maildir UID string under FOLDER.
With optional prefix argument HERE non-nil, insert result at
point. When called interactively, print result in echo area."
  (interactive
   `(,(read-directory-name "Find max UID under folder: "
                           (blc-parent-dir (cdar (blc-mbsync-maildirs)))
                           nil t)
     ,current-prefix-arg))
  (funcall (cond (here #'insert)
                 ((called-interactively-p 'interactive)
                  (apply-partially #'message "Max. UID: %s"))
                 (t #'identity))
           (number-to-string
            (apply #'max
                   `(0 ,@(blc-keep (lambda (file)
                                     (and-let* ((uid (blc--mbsync-uid file)))
                                       (string-to-number uid)))
                                   (directory-files-recursively folder "")))))))

(defvar blc-opusenc-switches '("--bitrate" "128" "--quiet")
  "List of `opusenc' switches for `blc-opusenc-flac'. ")

(defun blc-opusenc-flac (dir)
  "Recursively transcode all flac files under DIR to opus."
  (interactive `(,(read-directory-name "Transcode flacs under directory: "
                                       (blc-user-dir "MUSIC") nil t)))
  (let* ((len   0)
         (files (let (files)
                  (dolist (flac (directory-files-recursively dir "\\.flac\\'")
                                files)
                    (when-let* ((opus (blc-sed "\\.flac\\'" ".opus" flac t t))
                                ((file-newer-than-file-p flac opus)))
                      (push `[,(setq len (1+ len)) ,flac , opus] files)))))
         (rep   (make-progress-reporter
                 (format "Transcoding %d flac(s)..." len) 0 len)))
    (funcall (seq-reduce
              (pcase-lambda (sentinel `[,i ,flac ,opus])
                (lambda ()
                  (make-process
                   :connection-type 'pipe
                   :name     "opusenc"
                   :buffer   "*opusenc*"
                   :command  `("opusenc" ,@blc-opusenc-switches ,flac ,opus)
                   :sentinel (lambda (proc event)
                               (if (not (blc-process-success-p proc))
                                   (lwarn 'blc :error "opusenc: %s" event)
                                 (progress-reporter-update rep i)
                                 (funcall sentinel))))))
              files
              (apply-partially #'progress-reporter-done rep)))))

;;; Processes

(defun blc-process-success-p (proc)
  "Determine whether PROC exited successfully."
  (and (eq (process-status proc) 'exit)
       (zerop (process-exit-status proc))))

(defun blc-dropbox-status ()
  "Print status of external dropbox daemon."
  (interactive)
  (shell-command "dropbox status" "*Dropbox Status*"))

(defun blc-dropbox-start ()
  "Start external dropbox daemon."
  (interactive)
  (async-shell-command "dropbox start" "*Dropbox Control*"))

(defun blc-dropbox-stop ()
  "Stop external dropbox daemon."
  (interactive)
  (async-shell-command "dropbox stop" "*Dropbox Control*"))

(defun blc-mbsync (&rest args)
  "Call mbsync with ARGS asynchronously via a shell.
When called interactively, the user is prompted with completion
for multiple channels to synchronise. Otherwise, ARGS should form
a list of shell-quoted strings to pass to mbsync."
  (interactive (blc--mbsync-crm "Synchronise mbsync channels: "))
  (let ((cmd (string-join `("mbsync" ,@args) " ")))
    (async-shell-command cmd (format "*%s*" cmd))))

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
  (blc-turn-off #'font-lock-mode))

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

(define-symbol-prop
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

(defun blc-align-punctuation ()
  "Horizontally align mode-specific punctuation in region."
  (interactive)
  (unless (use-region-p)
    (mark-paragraph))
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)\\s."))

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
    (switch-to-buffer nil)))

(defun blc-split-window (&optional window)
  "Split WINDOW in a way suitable for `display-buffer'.
Like `split-window-sensibly' (which see), but prioritise
horizontal over vertical splitting."
  (when-let* ((window (or window (selected-window)))
              (split  (or (and (window-splittable-p window t)
                               #'split-window-right)
                          (and (or (window-splittable-p window)
                                   (and (frame-root-window-p window)
                                        (not (window-minibuffer-p window))
                                        (let ((split-height-threshold 0))
                                          (window-splittable-p window))))
                               #'split-window-below))))
    (with-selected-window window
      (funcall split))))

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

(defun blc-make-graphic-display ()
  "Make a graphical frame.
Display is determined by the environment variable DISPLAY."
  (interactive)
  (make-frame-on-display (getenv "DISPLAY")))

(defun blc-with-every-frame (&rest fns)
  "Run abnormal hooks in current frame and with every new one."
  (blc-funcs fns (selected-frame))
  (mapc (apply-partially #'add-hook 'after-make-frame-functions) fns))

(defun blc-but-fringes (width &rest subtrahends)
  "Subtract SUBTRAHENDS and total fringe columns from WIDTH."
  (apply #'- width `(,@subtrahends ,@(mapcar #'fringe-columns '(left right)))))

(defun blc-set-font-height (height &optional frame)
  "Set font HEIGHT in 1/10 pt for FRAME.
When called interactively without a prefix argument, or when
FRAME is nil, set font height for the current as well as all new
frames."
  (interactive `(,(read-face-attribute 'default :height)
                 ,(and current-prefix-arg (selected-frame))))
  (let ((font (format "%s-%d" (face-attribute 'default :family) (/ height 10))))
    (funcall (if frame
                 (apply-partially #'modify-frame-parameters frame)
               #'modify-all-frames-parameters)
             `((font . ,font)))))

;;; Settings

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

(defun blc-turn-off-cursor-blink (&optional frame &rest _)
  "Disable `blink-cursor-mode'."
  (and (display-graphic-p frame)
       blink-cursor-mode
       (blc-turn-off #'blink-cursor-mode)))

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

(defun blc-turn-off-prettify-symbols (&rest _)
  "Disable `prettify-symbols-mode'."
  (interactive)
  (blc-turn-off #'prettify-symbols-mode))

(defun blc-turn-off-scroll-bar (&optional frame &rest _)
  "Disable scroll bar in FRAME."
  (with-selected-frame frame
    (blc-turn-off #'toggle-scroll-bar)))

(defun blc-sort-reverse (_x _y)
  "Predicate that the order of X and Y should be swapped."
  t)

(defun blc-turn-on-xterm-mouse (&optional frame &rest _)
  "Enable `xterm-mouse-mode' with first terminal frame created."
  (or (display-graphic-p frame)
      xterm-mouse-mode
      (xterm-mouse-mode)))

;;; Modes

(defalias 'blc-rainbow--faces
  (thunk-delay
   (mapcar
    (lambda (face)
      `(,(blc-rx `(: symbol-start ,(symbol-name face) symbol-end)) . ,face))
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

;; (defvar blc-sunny-default-height 140
;;   "")

;; (defalias 'blc-toggle-sunny-mode
;;   (let ((height (face-attribute 'default :height)))
;;     (lambda (&optional force)
;;       (interactive "P")
;;       (message ">>> %s" blc-sunny-mode)
;;       (if (or force (not blc-sunny-mode))
;;           (progn (setq height (face-attribute 'default :height))
;;                  (blc-set-font-height blc-sunny-default-height)
;;                  (mapc #'disable-theme custom-enabled-themes))
;;         (blc-set-font-height height)
;;         (thread-last (lambda (sym)
;;                        (and (get sym 'theme-settings)
;;                             (custom-theme-name-valid-p sym)))
;;           (all-completions "" obarray)
;;           (mapcar #'intern)
;;           (mapc #'enable-theme)))))
;;   "")

;; (define-minor-mode blc-sunny-mode
;;   "Toggle."
;;   :lighter "😎"
;;   :global  t
;;   :group   'blc
;;   (blc-toggle-sunny-mode blc-sunny-mode))

;;; Constants

(defconst blc-chars-per-line 80
  "Target maximum number of characters per line.")

;;; Variables

(defvar blc-repos-dir (blc-dir user-emacs-directory "repos")
  "Directory containing symlinks to user Git repositories.")

(defvar blc-gnus-log-buffers '("*imap log*" "*nntp-log*")
  "List of buffer names associated with Gnus logs.")

(provide 'blc-lib)

;; Local Variables:
;; byte-compile-dynamic: t
;; End:

;;; blc-lib.el ends here
