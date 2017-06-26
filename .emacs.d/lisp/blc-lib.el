;;; blc-lib.el --- conveniences for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://github.com/basil-conto/dotfiles

;;; Commentary:

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "mod" user-emacs-directory)))
(require 'realpath)

(require 'seq)
(eval-when-compile
  (require 'subr-x)
  (require 'thunk))

(defgroup blc ()
  "Conveniences for blc."
  :group 'local)

;;; Sequences

(defun blc-true-list-p (object)
  "Determine whether the last `cdr' of OBJECT is nil."
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

(defun blc-keep (fn seq &optional copy)
  "Map FN over SEQ and return list of non-nil results.
SEQ is modified destructively unless COPY is non-nil."
  (funcall (if copy #'seq-mapcat #'mapcan)
           (lambda (item)
             (when-let (result (funcall fn item))
               `(,result)))
           seq))

(defun blc-assoc (alist key &optional case-fold)
  "Return cell keyed by KEY in ALIST.
Non-nil CASE-FOLD means perform case-insensitive lookup using
`assoc-string' instead of the default `assoc'."
  (declare (pure t))
  (if case-fold
      (assoc-string key alist case-fold)
    (assoc key alist)))

(defun blc-aget (alist key &optional default case-fold)
  "Return value mapped by KEY in ALIST or DEFAULT.
Non-nil CASE-FOLD means perform case-insensitive lookup using
`assoc-string' instead of the default `assoc'."
  (declare (pure t))
  (if-let (cell (blc-assoc alist key case-fold))
      (cdr cell)
    default))

(defun blc-aput (symbol key &optional val case-fold)
  "Map KEY of any type to VAL in alist SYMBOL and return VAL.
Overwrite any existing value mapped by KEY. Non-nil CASE-FOLD
means perform case-insensitive lookup using `assoc-string'
instead of the default `assoc'."
  (if-let (cell (blc-assoc (symbol-value symbol) key case-fold))
      (setcdr cell val)
    (add-to-list symbol `(,key . ,val))))

;;; Symbols

(defun blc-symcat (&rest objects)
  "Concatenate unquoted printed OBJECTS as a symbol."
  (intern (mapconcat (lambda (obj) (prin1-to-string obj t)) objects "")))

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

;;; Datetimes

(defun blc-mins-to-secs (mins)
  "Convert number of seconds in MINS."
  (* 60 mins))

(defun blc-rfc2822 (&optional time zone)
  "Like `format-time-string', but in RFC-2822 format."
  (format-time-string "%a, %d %b %Y %T %z" time zone))

;;; Processes

(defun blc-process-success-p (proc)
  "Determine whether PROC exited successfully."
  (and (eq (process-status proc) 'exit)
       (zerop (process-exit-status proc))))

;;; Files

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

;; TODO: Cross-platform?
(defun blc-user-dir (dir)
  "Return path of XDG user DIR via `xdg-user-dir' executable."
  (let ((path (car (process-lines "xdg-user-dir" dir))))
    (and (stringp path) (file-name-as-directory path))))

(defun blc-switch-to-temp-file (&optional prefix suffix)
  "Create and switch to a temporary file.
A non-empty filename PREFIX can help identify the file or its
purpose, whereas a non-empty SUFFIX will help determine the
relevant major-mode."
  (interactive "sfile prefix: \nsfile extension: ")
  (find-file (make-temp-file prefix nil (and suffix
                                             (not (string-blank-p suffix))
                                             (concat "." suffix)))))

(defvar blc-opusenc-switches '("--bitrate" "128" "--quiet")
  "List of `opusenc' switches for `blc-opusenc-flac'. ")

(defun blc-opusenc-flac (dir)
  "Recursively transcode all flac files under DIR to opus."
  (interactive `(,(read-directory-name "Transcode flacs under directory: "
                                       (blc-user-dir "MUSIC") nil t)))
  (let* ((len   0)
         (files (let (files)
                  (dolist (flac (directory-files-recursively dir "\\.flac\\'"))
                    (let ((opus (blc-sed "\\.flac\\'" ".opus" flac t t)))
                      (when (file-newer-than-file-p flac opus)
                        (push `(,(setq len (1+ len)) ,flac , opus) files))))
                  files))
         (rep   (make-progress-reporter
                 (format "Transcoding %d flacs..." len) 0 len))
         (buf   (get-buffer-create "*opusenc*")))
    (blc-safe-funcall
     (seq-reduce
      (pcase-lambda (sentinel `(,i ,flac ,opus))
        (lambda ()
          (if (= i len)
              (progress-reporter-done rep)
            (progress-reporter-update rep i))
          (make-process
           :connection-type 'pipe
           :name     "opusenc"
           :buffer   buf
           :command  `("opusenc" ,@blc-opusenc-switches ,flac ,opus)
           :sentinel (and sentinel
                          (lambda (proc event)
                            (if (blc-process-success-p proc)
                                (funcall sentinel)
                              (lwarn 'blc :debug "opusenc: %s" event)))))))
      files ()))))

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
                                  buffer-name-history
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
  (interactive "p")
  (scroll-up (or step blc-small-scroll-step)))

(defun blc-small-scroll-down (&optional step)
  "Scroll down `blc-small-scroll-step' lines."
  (interactive "p")
  (scroll-down (or step blc-small-scroll-step)))

;;; Frames

(defun blc-make-graphic-display ()
  "Make a graphical frame.
Display is determined by the environment variable DISPLAY."
  (interactive)
  (make-frame-on-display (getenv "DISPLAY")))

(defun blc-with-every-frame (&rest fns)
  "Run abnormal hooks in current frame and with every new one."
  (blc-funcs fns (selected-frame))
  (mapc (lambda (fn)
          (add-hook 'after-make-frame-functions fn))
        fns))

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
                 (lambda (alist)
                   (modify-frame-parameters frame alist))
               #'modify-all-frames-parameters)
             `((font . ,font)))))

;;; Settings

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

(defvar blc-gnus-log-buffers '("*nntp-log*")
  "List of buffer names associated with Gnus logs.")

(provide 'blc-lib)

;; Local Variables:
;; byte-compile-dynamic: t
;; End:

;;; blc-lib.el ends here
