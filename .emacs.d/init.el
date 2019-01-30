;;; init.el --- init file for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://gitlab.com/basil-conto/dotfiles

;;; Code:

;;;; BOOTSTRAPPING

(defalias 'blc-report-init-time
  (let ((file load-file-name))
    (lambda ()
      (message "Loading %s...done (%.3fs)" file
               (float-time (time-subtract after-init-time before-init-time)))))
  "Print 3 d.p. `emacs-init-time' after `load'-style message.")

;; User
(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

(require 'blc-lib)
(require 'blc-pkg)

;; Built-in
(require 'map)
(require 'seq)
(eval-when-compile
  (require 'subr-x))

(autoload 'blc-mbsync                     "blc-mbsync" nil t)
(autoload 'blc-mbsync-all                 "blc-mbsync" nil t)
(autoload 'blc-mbsync-deduplicate         "blc-mbsync" nil t)
(autoload 'blc-mbsync-maximise-uid        "blc-mbsync" nil t)
(autoload 'blc-pass-backend-parse         "blc-pass")
(autoload 'dired-jump                     "dired-x" nil t)
(autoload 'dired-jump-other-window        "dired-x" nil t)
(autoload 'engine-mode-prefixed-map       "engine-mode" nil t 'keymap)
(autoload 'eww-open-in-new-buffer         "eww" nil t)
(autoload 'ffap-gnus-hook                 "ffap")
(autoload 'flex-mode                      "flex-mode" nil t)
(autoload 'samba-generic-mode             "generic-x" nil t)
(autoload 'gnus-find-subscribed-addresses "gnus")
(autoload 'turn-on-hi-lock-if-enabled     "hi-lock")
(autoload 'irfc-follow                    "irfc" nil t)
(autoload 'irfc-mode                      "irfc" nil t)
(autoload 'irfc-visit                     "irfc" nil t)
(autoload 'ivy-completion-in-region       "ivy")
(autoload 'magit-pop-revision-stack       "magit-extras" nil t)
(autoload 'mailcap-file-name-to-mime-type "mailcap")
(autoload 'meme                           "meme" nil t)
(autoload 'meme-file                      "meme" nil t)
(autoload 'TeX-doc                        "tex" nil t)
(autoload 'youtube-dl                     "youtube-dl" nil t)
(autoload 'youtube-dl-list                "youtube-dl" nil t)
(autoload 'youtube-dl-playlist            "youtube-dl" nil t)

;;;; ADVICE

;; fns.c

(advice-add #'yes-or-no-p :override #'y-or-n-p)

;; battery

(define-advice battery-linux-sysfs (:filter-return (alist) blc-unicodify)
  "Transcribe Linux sysfs AC line status in ALIST to Unicode."
  (let ((cell (assq ?L alist)))
    (setcdr cell (pcase (cdr cell)
                   ("AC"  "🔌")
                   ("BAT" "🔋")
                   (_     "¿?"))))
  alist)

;; bbdb-com

(define-advice bbdb-complete-mail (:around (complete &rest args) blc-minibuffer)
  "Replace *Completions* buffer with `completing-read'."
  (let* (cands
         (temp-buffer-show-function
          (lambda (buf)
            (kill-buffer buf)
            (choose-completion-string
             (completing-read "Address: " cands nil 'confirm
                              (buffer-substring
                               (car completion-base-position)
                               (or (cadr completion-base-position)
                                   (point)))
                              'blc-bbdb-mail-history)
             (current-buffer)
             completion-base-position))))
    (blc-with-nonce display-completion-list
        :before (apply-partially #'set 'cands)
      (apply complete args))))

;; browse-url

(define-advice browse-url-firefox (:around (fn &rest args) blc-no-wait)
  "Detach from spawned subprocess."
  (blc-with-nonce start-process :override
                  (lambda (_name _buf prog &rest args)
                    (apply #'call-process prog nil 0 nil args))
    (apply fn args)))

;; cc-align

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

;; elisp-mode

(define-advice elisp-completion-at-point (:filter-return (ret) blc-elisp-pred)
  "Filter unwanted symbols from `elisp-completion-at-point'."
  (when-let* (((consp ret))
              (props (cdddr ret))
              (pred  (plist-get props :predicate)))
    (plist-put props :predicate
               (lambda (sym)
                 (and (funcall pred sym)
                      (not (string-suffix-p "--cmacro" (symbol-name sym)))))))
  ret)

;; em-cmpl

(define-advice eshell-pcomplete (:override (&rest _) blc-completion-at-point)
  "Use default inline completion."
  (completion-at-point))

;; eww

(define-advice eww-open-in-new-buffer (:around (eww url) blc-read-url)
  "Like `eww', but fetch URL in a new EWW buffer."
  (interactive (advice-eval-interactive-spec (cadr (interactive-form #'eww))))
  (let ((eww-suggest-uris (list (lambda () url))))
    (funcall eww)))

;; files

(define-advice save-buffers-kill-emacs
    (:around (kill &rest args) blc-confirm-daemon)
  "Ensure `confirm-kill-emacs' is bound when `daemonp'."
  (let ((confirm-kill-emacs (or confirm-kill-emacs
                                (and (daemonp)
                                     (lambda (&rest _)
                                       (yes-or-no-p "Really kill daemon? "))))))
    (apply kill args)))

;; find-func

(define-advice find-function-search-for-symbol
    (:around (search sym type lib) blc-dataroot-to-src)
  "Pass LIB through `blc-dataroot-to-src'."
  (funcall search sym type (blc-dataroot-to-src lib)))

;; gnus-sum

(define-advice gnus-summary-exit (:after (&rest _) blc-gnus-single-group-frame)
  "Allow only the selected frame to display `gnus-group-buffer'."
  (dolist (win (cdr (get-buffer-window-list gnus-group-buffer nil 'visible)))
    (when (frame-root-window-p win)
      (blc-delete-spare-frame (window-frame win)))))

;; gnus-win

(define-advice gnus-configure-frame (:before (&rest _) blc-no-gnus-frames)
  "Empty lists of frames kept by Gnus.
Emptying `gnus-frame-list' undedicates Gnus frames from their
initial purpose, so their window configuration, if since changed,
is left alone.
Emptying `gnus-created-frames' protects against deletion of the
last visible Emacs client frame."
  (setq gnus-frame-list     ())
  (setq gnus-created-frames ()))

;; help

(define-advice view-echo-area-messages (:filter-return (win) blc-select-window)
  "Pop to `messages-buffer'."
  (when-let (frame (and win (window-frame win)))
    (select-frame-set-input-focus frame)
    (select-window win))
  win)

;; help-fns

(define-advice help-fns-short-filename (:around (abbr file) blc-src-load-path)
  "Dynamically bind `load-path' with `blc-src-path'."
  (let ((load-path (blc-src-path)))
    (funcall abbr file)))

(advice-add #'find-lisp-object-file-name :filter-return #'blc-dataroot-to-src)

;; hi-lock

(define-advice turn-on-hi-lock-if-enabled (:before () blc-exclude-derived-modes)
  "Exempt derived modes from hi-lock highlighting.
Include every major mode derived from the current
`hi-lock-exclude-modes' in that blacklist."
  (when (apply #'derived-mode-p hi-lock-exclude-modes)
    (add-to-list 'hi-lock-exclude-modes major-mode)))

;; ibuffer

(define-advice ibuffer (:filter-args (args) blc-ibuffer)
  "Like `ibuffer', but prefer default `ibuffer-filter-groups'."
  (seq-let (other name quals nosel shrink filters &rest tail) args
    (nconc (list other name quals nosel shrink
                 (or filters (default-value 'ibuffer-filter-groups)))
           tail)))

;; ivy-bibtex

(define-advice bibtex-completion-format-entry
    (:around (fmt entry width) blc-narrow)
  "Decrease `ivy-bibtex' entry width due to other formatting.
`ivy-bibtex-default-action' only considers `frame-width', which
does not, for example, take the effect of `ivy-format-function'
into account."
  (funcall fmt entry
           (blc-but-fringes
            width (string-width (funcall ivy-format-function '(""))))))

;; ledger-complete

(define-advice ledger-pcomplete (:override (&rest _) blc-completion-at-point)
  "Use default inline completion."
  (completion-at-point))

;; magit-diff

(define-advice magit-diff-show-or-scroll
    (:around (fn &rest args) blc-visible-frames)
  "Show and scroll Magit diff buffer across frames."
  (blc-with-nonce get-buffer-window :around
                  (lambda (get &optional buf _frames)
                    (funcall get buf 'visible))
    (apply fn args)))

;; magit-extras

(define-advice magit-pop-revision-stack
    (:around (fn &rest args) blc-message-narrow-to-body)
  "Narrow to `message-mode' body before popping a revision."
  (let ((mail (derived-mode-p #'message-mode)))
    (save-restriction
      (when mail
        (narrow-to-region (if (message-in-body-p)
                              (save-excursion (message-goto-body))
                            (point-min))
                          (save-excursion
                            (when (message-goto-signature)
                              (re-search-backward message-signature-separator)
                              (end-of-line 0))
                            (point))))
      (apply fn args))
    (save-excursion
      (and mail
           (message-goto-signature)
           (re-search-backward message-signature-separator)
           (or (= (line-beginning-position 0) (line-end-position 0))
               (insert ?\n))))))

;; magit-log

(define-advice magit-log-maybe-update-revision-buffer-1
    (:around (fn) blc-all-frames)
  "Update Magit log buffer across frames."
  (blc-with-nonce magit-mode-get-buffer :around
                  (lambda (get mode &optional create _frame value)
                    (funcall get mode create nil value))
    (funcall fn)))

;; magit-remote

(define-advice magit-clone (:around (clone repo dir) blc-git-clone-subdir)
  "Clone into subdirectory of DIR if non-empty."
  (setq dir (blc-dir dir))
  (and (file-directory-p dir)
       (directory-files dir nil directory-files-no-dot-files-regexp t)
       (setq dir (blc-dir dir
                          (and (string-match
                                (rx (* nonl) (in ?/ ?:) (group (+? nonl))
                                    (? ".git") eos)
                                repo)
                               (match-string 1 repo)))))
  (funcall clone repo dir))

;; mail-extr

(define-advice mail-extract-address-components
    (:before-until (address &optional all) blc-delegate-gnus)
  "Try to cut corners with `gnus-extract-address-components'.
This is much less accurate but also much more performant than
`mail-extract-address-components'."
  (and (not all)
       (stringp address)
       (gnus-extract-address-components address)))

;; make-mode

(defun blc-delete-hspace-backward--advice (&rest _)
  "Delete horizontal whitespace before point."
  (delete-horizontal-space t))

(dolist (fn (list #'makefile-insert-gmake-function
                  #'makefile-insert-target-ref))
  (advice-add fn :after #'blc-delete-hspace-backward--advice))

;; mpc

(define-advice mpc (:around (mpc) blc-ensure-dedicated)
  "Start mpd and temporarily dedicate selected window."
  (let* ((win  (selected-window))
         (flag (window-dedicated-p win))
         (cmds (blc-system-procs-by-attr 'comm)))
    ;; Fire ze missiles
    (when-let* ((cmd "mpd")
                ((not (member cmd cmds))))
      (call-process cmd))
    (when-let* ((cmd "mpDris2")
                ((not (member cmd cmds))))
      (call-process-shell-command (concat cmd " &")))
    ;; Hold on to your butts
    (unwind-protect
        (progn
          (set-window-dedicated-p win t)
          (call-interactively mpc))
      (set-window-dedicated-p win flag))))

;; newst-treeview

(define-advice newsticker--treeview-frame-init
    (:around (init &rest args) blc-anonymous-frame)
  "Create frame sans `name' parameter."
  (blc-with-nonce make-frame :filter-args #'ignore
    (apply init args)))

;; org

(define-advice org-read-date (:around (read &rest args) blc-avoid-frames)
  "Temporarily disable `pop-up-frames'."
  (let (pop-up-frames)
    (apply read args)))

;; org-capture

(define-advice org-capture-refile (:after (&rest _) blc-org-save)
  "Save target buffer of `org-capture-refile'."
  (save-window-excursion
    (org-capture-goto-last-stored)
    (save-buffer)))

;; org-pcomplete

(define-advice pcomplete/org-mode/tex (:override () blc-complete-entity)
  "Perform Org entity completion via `completion-in-region'.
Offer all entities found in `org-entities-user' and
`org-entities' except those multiples of en space."
  (when (equal "tex" (car-safe (org-thing-at-point)))
    (completion-in-region
     (save-excursion
       (skip-chars-backward "^\\\\")
       (point))
     (point)
     (sort (blc-keep #'car-safe (append org-entities-user org-entities))
           #'string-lessp)
     (lambda (entity)
       (/= ?_ (string-to-char entity))))))

;; project

(define-advice project-find-file-in (:around (fn file dirs proj) blc-root-dir)
  "Bind `default-directory' to the first root of PROJ in DIRS."
  (let ((default-directory (blc-dir (car dirs))))
    (funcall fn file dirs proj)))

(define-advice project-file-completion-table
    (:filter-return (fn) blc-relative)
  "Return relative file names."
  (lambda (str pred action)
    (let ((res (funcall fn str pred action)))
      (if (eq action t)
          (mapcar #'file-relative-name res)
        res))))

;; sx-question-mode

(define-advice sx-question-mode--get-window (:override () blc-sx-question-win)
  "Return first SX question window found on any visible frame."
  (get-window-with-predicate (lambda (win)
                               (with-selected-window win
                                 (derived-mode-p #'sx-question-mode)))
                             'never 'visible))

;; whitespace

(defun blc-whitespace-enable--advice ()
  "Advice intended for predicate `whitespace-enable-predicate'."
  (not (or (derived-mode-p #'dired-mode
                           #'magit-mode
                           #'markdown-mode
                           #'shell-mode)
           (equal (buffer-name) "*scratch*"))))

(with-eval-after-load 'whitespace
  (add-function :before-while whitespace-enable-predicate
                #'blc-whitespace-enable--advice))

;; window

(defun blc-pop-up-frame--advice (fn &rest args)
  "Keep focus on old frame when popping up a new one to display."
  (let ((frame (selected-frame)))
    (unwind-protect
        (apply fn args)
      (select-frame-set-input-focus frame))))

(add-function :around pop-up-frame-function #'blc-pop-up-frame--advice)

;;;; DEFINITIONS

;; auctex

(defun blc-LaTeX-command-default ()
  "Locally restore `default-value' of `TeX-command-default'."
  (setq TeX-command-default (default-value 'TeX-command-default)))

(defun blc-TeX-doc (arg)
  "Prompt for and display TeX documentation.
Like `TeX-doc', but with prefix ARG pass it to
`TeX-documentation-texdoc' instead."
  (interactive "P")
  (if arg
      (TeX-documentation-texdoc arg)
    (call-interactively #'TeX-doc)))

;; bbdb

(defun blc-bbdb-set-gnus-summary-line-format ()
  "Prepare `gnus-summary-line-format' for `bbdb' unification."
  (setq-default
   gnus-summary-line-format
   (blc-gnus-summary-line-format "u" bbdb-mua-summary-unify-format-letter)))

;; browse-url

(defun blc-print-url--lpr (url)
  "Asynchronously print URL using `lpr-command'.
This function is written with a print command like `hp-print' in
mind, which is passed `lpr-switches' and URL as arguments."
  (interactive "fPrint file: ")
  (make-process :name            "LPR Print"
                :command         `(,lpr-command ,@lpr-switches ,url)
                :connection-type 'pipe))

(defun blc-print-url--webkit (url pdf callback)
  "Asynchronously convert contents of URL to PDF using `wkhtmltopdf'.
Call CALLBACK with no arguments on success."
  (let ((name "WebKit Print"))
    (make-process :name            name
                  :command         (list "wkhtmltopdf" url pdf)
                  :connection-type 'pipe
                  :sentinel
                  (lambda (proc event)
                    (if (blc-process-success-p proc)
                        (funcall callback)
                      (lwarn 'blc :error "%s: %s" name event))))))

(defun blc-print-url (url &rest _)
  "Print contents of URL.
See `browse-url' for a description of the arguments."
  (require 'eww)
  (let ((mime   (mailcap-file-name-to-mime-type url))
        (remote (url-handler-file-remote-p url)))
    (cond ((and remote
                (member mime '("application/pdf" "application/postscript")))
           (blc-print-url--lpr (url-file-local-copy url)))
          ((not (or remote (eww-html-p mime)))
           (blc-print-url--lpr (url-filename (url-generic-parse-url url))))
          (t
           (let ((tmp (make-temp-file "blc-" nil ".pdf")))
             (blc-print-url--webkit
              url tmp (apply-partially #'blc-print-url--lpr tmp)))))))

(function-put #'blc-print-url 'interactive-form (interactive-form #'browse-url))

(defun blc-print-url-pdf (url &rest _)
  "Save contents of URL as a PDF file.
See `browse-url' for a description of the arguments."
  (cond ((not (equal (mailcap-file-name-to-mime-type url) "application/pdf"))
         (let* ((base (file-name-sans-extension (url-file-nondirectory url)))
                (pdf  (blc-read-file (blc--url-fmt "Save URL `%s' to: " url)
                                     (concat base ".pdf"))))
           (blc-print-url--webkit url pdf #'ignore)))
        ((url-handler-file-remote-p url)
         (blc-download url))
        (t
         (message "PDF `%s' already saved" url))))

(function-put
 #'blc-print-url-pdf 'interactive-form (interactive-form #'browse-url))

(defun blc-browse-url-irfc (url &rest _)
  "Visit RFC URL via `irfc-visit'.
URL is parsed using the regular expressions found in
`auto-mode-alist' and `ffap-alist' for `irfc-mode' and
`ffap-rfc', respectively."
  (require 'ffap)
  (if-let ((res (blc-keep (pcase-lambda (`(,key . ,alist))
                            (and-let* ((re (car (rassq key alist))))
                              `(regexp ,re)))
                          `((irfc-mode . ,auto-mode-alist)
                            (ffap-rfc  . ,ffap-alist)))))
      (if (string-match (blc-rx `(| ,@res)) (url-file-nondirectory url))
          (irfc-visit (string-to-number (match-string 1)))
        (user-error "Invalid RFC URL: %s" url))
    (user-error "Regexp not found for RFC URL: %s" url)))

(defvar browse-url-firefox-arguments)
(defvar browse-url-generic-program)

(defun blc-browse-url-surf (&rest args)
  "Like `browse-url-generic', but using the Surf browser."
  (let ((browse-url-generic-program "surf"))
    (apply #'browse-url-generic args)))

(defun blc-browse-url-firefox (&rest args)
  "Like `browse-url-firefox', but private."
  (let ((browse-url-firefox-arguments
         `("-P" "private" "-private-window" ,@browse-url-firefox-arguments)))
    (apply #'browse-url-firefox args)))

(defvar blc-browser-alist
  `(("Firefox"            . ,#'browse-url-firefox    )
    ("Firefox private"    . ,#'blc-browse-url-firefox)
    ("EWW"                . ,#'eww-browse-url        )
    ("Download"           . ,#'blc-download          )
    ("Print"              . ,#'blc-print-url         )
    ("Print to PDF"       . ,#'blc-print-url-pdf     )
    ("Emacs IRFC"         . ,#'blc-browse-url-irfc   )
    ("XDG"                . ,#'browse-url-xdg-open   )
    ("Surf"               . ,#'blc-browse-url-surf   )
    ("Chromium"           . ,#'browse-url-chromium   )
    ("Elinks"             . ,#'browse-url-elinks     )
    ("Xterm text browser" . ,#'browse-url-text-xterm )
    ("Emacs text browser" . ,#'browse-url-text-emacs ))
  "Map preferred browsers to their calling function.")

(defun blc-browse-url (url &rest args)
  "Read WWW browser name to open URL with completion.
See `blc-browser-alist' for known browsers and `browse-url' for a
description of the arguments to this function."
  (when-let* ((prompt (if (string-blank-p url)
                          "Open browser: "
                        (blc--url-fmt "Open URL `%s' in: " url)))
              (browser
               (blc-get blc-browser-alist
                        (completing-read prompt blc-browser-alist nil t))))
    (apply browser url args)))

(function-put
 #'blc-browse-url 'interactive-form (interactive-form #'browse-url))

;; cc-mode

(defun blc-turn-on-c++-comments ()
  "Default to C++-style line comments."
  (if (bound-and-true-p c-buffer-is-cc-mode)
      (c-toggle-comment-style -1)
    (setq comment-start "//"
          comment-end   "")))

;; counsel

(defun blc-counsel-find-file (&optional file)
  "Like `counsel-find-file', but return buffer, not name of FILE.
This likens `counsel-find-file' to `find-file' more and makes it
suitable for assigning to `ffap-file-finder'."
  (interactive)
  (if file
      (find-file file)
    (set-buffer (or (find-buffer-visiting (counsel-find-file))
                    (other-buffer nil t)))))

(defun blc-counsel-M-x-other-window (cmd)
  "Open new window before executing CMD.
Intended as an Ivy action for `counsel-M-x'."
  (switch-to-buffer-other-window (current-buffer))
  (when-let* ((cmd (intern-soft cmd)))
    (setq real-this-command cmd)
    (setq this-command      cmd)
    (setq prefix-arg        current-prefix-arg)
    (command-execute cmd t)))

;; csv-mode

(defun blc-csv-align-all-fields ()
  "Align all fields in the current CSV buffer."
  (csv-align-fields nil (point-min) (point-max)))

;; eww

(defun blc-eww-bookmark-save ()
  "Copy the URL of the current bookmark into the kill ring."
  (interactive)
  (if-let* ((eww-data
             (get-text-property (line-beginning-position) 'eww-bookmark)))
      (eww-copy-page-url)
    (user-error "No bookmark on the current line")))

(defun blc-eww-toggle-images ()
  "Toggle display of images in current `eww' buffer.
Adapted from URL
`https://emacs.stackexchange.com/a/38639/15748'."
  (interactive)
  (setq-local shr-inhibit-images (not shr-inhibit-images))
  (eww-reload t))

;; flycheck

(defun blc-turn-off-flycheck (&rest _)
  "Disable `flycheck-mode'."
  (interactive)
  (when (bound-and-true-p flycheck-mode)
    (blc-turn-off #'flycheck-mode)))

;; ghub

(defconst blc-github-token-scopes '(notifications)
  "List of `ghub' scopes for `blc'.")

(defun blc-github-notifications ()
  "Asynchronously notify of any GitHub notifications."
  (interactive)
  (require 'ghub)
  (require 'notifications)
  (ghub-get
   "/notifications" ()
   :auth 'blc
   :callback
   (lambda (nots &rest _)
     (notifications-notify
      :title (blc-dom-to-xml
              'a '((href . "https://github.com/notifications")) "GitHub")
      :body (let ((repos '(("other" . 0))))
              (dolist (not nots)
                (let ((name (map-nested-elt not '(repository name) "other")))
                  (blc-put* repos name (1+ (blc-get repos name 0)))))
              (let ((fmt (format
                          "%%-%ds %%%dd"
                          (apply #'max (map-keys-apply #'string-width repos))
                          (1+ (floor (log (apply #'max 1 (map-values repos))
                                          10))))))
                (mapconcat (pcase-lambda (`(,name . ,count))
                             (format fmt name count))
                           repos "\n")))))
   :errorback (lambda (err &rest _)
                (signal 'error (list "GitHub error: %S" (cdr err))))))

;; git-commit

(defun blc-git-commit-set-fill-column ()
  "Set local `fill-column' for `git-commit-mode' buffers."
  ;; Benefit over setq: displays debugging message
  (set-fill-column 68))

;; gnus

(defun blc--gnus-switch-buffer (action)
  "Call ACTION on first desirable Gnus buffer found.
Return result of ACTION. See `blc-gnus' for a definition of
desirable."
  (and-let* ((buf (seq-some (lambda (var)
                              (and-let* (((boundp var))
                                         (buf (symbol-value var))
                                         (buf (get-buffer buf))
                                         ((buffer-live-p buf))
                                         ((> (buffer-size buf) 0)))
                                buf))
                            '(gnus-article-buffer
                              gnus-summary-buffer
                              gnus-group-buffer))))
    (funcall action buf)))

(defun blc-gnus ()
  "Display first desirable Gnus buffer found in selected window.
If none of the article, summary or group buffer are found, in
order of descending priority, start `gnus'."
  (interactive)
  (unless (blc--gnus-switch-buffer #'pop-to-buffer-same-window)
    (gnus)))

(defvar gnus-inhibit-startup-message)

(defun blc-gnus-other-window ()
  "Like `blc-gnus', but use another window."
  (interactive)
  (let (break) ; Infloop should never happen, but avoid famous last words
    (while (not (or (blc--gnus-switch-buffer #'pop-to-buffer)
                    break))
      (setq break t)
      (save-window-excursion
        (let ((gnus-inhibit-startup-message t))
          (gnus))))))

(defun blc-gnus-other-frame ()
  "Like `blc-gnus', but use another frame.
Suspending or exiting Gnus deletes that frame."
  (interactive)
  (blc-make-frame)
  (blc-gnus))

;; hi-lock

(defun blc-hi-lock-no-eof-nl ()
  "Highlight missing trailing EOF newlines."
  (hi-lock-set-pattern "^.+\\'" 'hi-red-b))

;; ibuffer

(defun blc-ibuffer-ffap ()
  "Like `ibuffer-find-file', but backed by `ffap-file-finder'."
  (interactive)
  (require 'ffap)
  (let* ((buffer            (ibuffer-current-buffer))
         (buffer            (if (buffer-live-p buffer) buffer (current-buffer)))
         (default-directory (buffer-local-value 'default-directory buffer)))
    (call-interactively ffap-file-finder)))

;; ielm

(defun blc-ielm-other-window ()
  "Call `ielm' in another window."
  (interactive)
  (let ((display-buffer-overriding-action '(() (inhibit-same-window . t))))
    (call-interactively #'ielm)))

(defun blc-info-read-buffer ()
  "Read the name, file and node of an Info buffer.
Return the name of the buffer as a string or `nil'."
  (if-let* ((bufs (blc-keep
                   (lambda (buf)
                     (and-let* ((name (buffer-name buf))
                                ((/= (aref name 0) ?\s))
                                (id   (buffer-local-value
                                       'mode-line-buffer-identification buf)))
                       (cons (concat name (substring-no-properties (cadr id)))
                             name)))
                   (blc-derived-buffers #'Info-mode)))
            ((cdr bufs)))
      (blc-get bufs (completing-read
                     "Info buffer: " (seq-sort-by #'car #'string-lessp bufs)))
    (cdar bufs)))

;; info

(defun blc-info (&optional buffer)
  "Call `info' on interactively completed BUFFER."
  (interactive (list (blc-info-read-buffer)))
  (info nil buffer))

(defun blc-info-other-window (&optional buffer)
  "Call `info-other-window' on interactively completed BUFFER."
  (interactive (list (blc-info-read-buffer)))
  (info-other-window nil buffer))

(defun blc-info-kill ()
  "Quit Info and kill its buffer."
  (interactive)
  (if Info-standalone
      (save-buffers-kill-emacs)
    (quit-window t)))

;; isearch

(defun blc-isearch-delight ()
  "Shorten lighter of `isearch-mode'."
  (setq isearch-mode "🔍"))

;; man

(defun blc--man-other-buffer (&optional prev)
  "Switch to next `man' buffer (previous if PREV is non-nil)."
  (let ((bufs (funcall (if prev #'nreverse #'identity)
                       (blc-derived-buffers #'Man-mode))))
    (pop-to-buffer-same-window (or (cadr (memq (current-buffer) bufs))
                                   (car bufs))
                               t)))

(defun blc-man-next-buffer ()
  "Switch to next `man' buffer."
  (interactive)
  (blc--man-other-buffer))

(defun blc-man-previous-buffer ()
  "Switch to previous `man' buffer."
  (interactive)
  (blc--man-other-buffer t))

;; message

(defun blc-message-set-msmtp-from ()
  "Replace From header with address read from `~/.msmtprc'."
  (interactive)
  (thread-last (completing-read "From address: " (blc-msmtp-addresses)
                                nil t nil nil user-mail-address)
    (message-make-from nil)
    (message-replace-header "From")))

(defun blc-message-confirm-attach ()
  "Allow user to quit sending on missing attachment detection."
  (or (save-excursion
        (message-goto-body)
        (not (re-search-forward "attach"
                                (save-excursion
                                  (message-goto-signature)
                                  (point))
                                t)))
      (map-some (lambda (_part params)
                  (assq 'disposition params))
                (mml-parse))
      (y-or-n-p "Mention of \"attach\" but no attachments; send anyway? ")
      (keyboard-quit)))

;; org

(defun blc--org-agenda-day-1 (n sign iter origin)
  "Subroutine of `blc--org-agenda-day'."
  (while (unless (zerop n)
           (when-let* ((day (funcall iter (funcall origin) 'org-date-line)))
             (goto-char day)
             (beginning-of-line)
             (setq n (- n sign))))))

(defun blc--org-agenda-day (n)
  "Move N agenda date lines forward (backward if N is negative)."
  (apply #'blc--org-agenda-day-1 n (cl-signum n)
         (if (natnump n)
             (list #'next-single-property-change #'line-end-position)
           (list #'previous-single-property-change #'line-beginning-position))))

(defun blc-org-agenda-day-backward (n)
  "Like `previous-line', but for `org' agenda date lines."
  (interactive "p")
  (blc--org-agenda-day (- n)))

(defun blc-org-agenda-day-forward (n)
  "Like `next-line', but for `org' agenda date lines."
  (interactive "p")
  (blc--org-agenda-day n))

(defun blc-org-read-file ()
  "Read `org' filename.
Defaults to `org-directory' and `org-default-notes-file'."
  ;; DEFAULT-FILENAME argument doesn't work with ivy
  (read-file-name "Org file: " org-directory nil nil
                  (file-name-nondirectory org-default-notes-file)))

(defun blc-org-find-file (&optional file)
  "Like `find-file', but defaults to `org-directory' files."
  (interactive (list (blc-org-read-file)))
  (find-file file))

(defun blc-org-find-file-other-window (&optional file)
  "Like `blc-org-find-file', but opens another window."
  (interactive (list (blc-org-read-file)))
  (find-file-other-window file))

(defun blc-org-prop-captured ()
  "Set inactive timestamp :captured: property on current entry."
  (org-set-property
   "captured" (format-time-string (org-time-stamp-format t t))))

;; pdf-tools

(defun blc-pdf-tools-undefer ()
  "Clean up remnants of deferred `pdf-tools' loading."
  (remove-hook 'pdf-view-mode-hook #'blc-pdf-tools-undefer)
  (setq auto-mode-alist (rassq-delete-all #'pdf-tools-install auto-mode-alist)))

;; project

(defun blc-project-switch ()
  "Complete a file from another project to visit."
  (interactive)
  (let ((proj (project-current t (and (require 'magit-repos nil t)
                                      (blc-dir (magit-read-repository))))))
    (setq this-command #'project-find-file)
    (project-find-file-in nil (project-roots proj) proj)))

;; python

(defun blc-python-pep-8-comments ()
  "Adapt `comment-inline-offset' to PEP-8 recommendations."
  (setq-local comment-inline-offset 2))

;; solar

(defun blc-solar-set-location (&optional location)
  "Reconcile solar calendar with LOCATION from `blc-locations'."
  (interactive (list (completing-read "Location: " blc-locations nil t nil ()
                                      (blc-system-location))))
  (pcase-let (((plist :country country :lat lat :long long)
               (blc-get blc-locations location)))
    (setq-default calendar-latitude      lat
                  calendar-longitude     long
                  calendar-location-name (format "%s, %s" location country))))

;; term

(defun blc-term (&optional non-ansi)
  "Complete `ansi-term' and `term' buffer to switch to.
Create a new `ansi-term' buffer if the special first candidate
\"New\" is selected.  With optional prefix argument NON-ANSI
non-nil, create a new `term' buffer instead."
  (interactive "P")
  (if-let* ((new   "New")
            (names (mapcar #'buffer-name (blc-derived-buffers #'term-mode)))
            (name  (completing-read "Term: " (cons new names)
                                    nil t nil 'blc-term-history names))
            ((not (string-equal name new))))
      (pop-to-buffer name)
    (blc-with-nonce switch-to-buffer :override #'pop-to-buffer
      (funcall (if non-ansi #'term #'ansi-term) explicit-shell-file-name))))

(defun blc-toggle-subterm-mode ()
  "Toggle between `term-char-mode' and `term-line-mode'."
  (interactive)
  (if (eq (current-local-map) term-raw-map)
      (term-line-mode)
    (term-char-mode)))

(defun blc-term-rename ()
  "Reflect `default-directory' changes in terminal buffer name.
Intended for `term-exec-hook'."
  (add-function
   :after (process-filter (get-buffer-process (current-buffer)))
   (let (dir)
     (lambda (proc _s)
       (when-let ((buf (process-buffer proc))
                  ((buffer-live-p buf)))
         (with-current-buffer buf
           (unless (equal dir default-directory)
             (setq dir default-directory)
             (rename-buffer (format "*%s %s*"
                                    term-ansi-buffer-base-name
                                    (abbreviate-file-name
                                     (directory-file-name dir)))
                            t))))))))

;; visual-fill-column

(defun blc-visual-auto-fill-column ()
  "Reconcile `visual-fill-column-mode' with `auto-fill-mode'.
Keep `visual-fill-column-width' larger than `fill-column' for
less jumpy auto-filling."
  (setq visual-fill-column-width (+ fill-column 20)))

;; webjump

(defun blc-webjump-browse-url (_name)
  "Wrap `browse-url-interactive-arg' for use in `webjump-sites'."
  (car (browse-url-interactive-arg "URL: ")))

;; xref-js2

(defun blc-xref-js2-install-backend ()
  "Locally install `xref-js2-xref-backend'."
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))

;;;; VARIABLES

(setq-default
 ;; buffer.c
 fill-column                            blc-chars-per-line
 indicate-buffer-boundaries             t
 indicate-empty-lines                   t
 mode-line-format                       (blc-sed-tree " +" " " mode-line-format)

 ;; callint.c
 mark-even-if-inactive                  nil

 ;; callproc.c
 shell-file-name                        "/bin/sh"

 ;; doc.c
 text-quoting-style                     'grave

 ;; frame.c
 frame-resize-pixelwise                 t

 ;; indent.c
 indent-tabs-mode                       nil

 ;; keyboard.c
 echo-keystrokes                        0.5

 ;; minibuffer.c
 enable-recursive-minibuffers           t

 ;; print.c
 print-circle                           t
 print-gensym                           t

 ;; terminal.c
 ring-bell-function                     #'ignore

 ;; window.c
 fast-but-imprecise-scrolling           t
 recenter-redisplay                     nil
 scroll-preserve-screen-position        t
 window-combination-resize              t

 ;; xdisp.c
 auto-hscroll-mode                      'current-line
 line-number-display-limit-width        (ash blc-chars-per-line 3)
 scroll-conservatively                  most-positive-fixnum
 scroll-margin                          1
 scroll-step                            1
 show-trailing-whitespace               t

 ;; xfns.c
 x-gtk-use-system-tooltips              nil

 ;; xterm.c
 x-wait-for-event-timeout               nil

 ;; abbrev
 abbrev-file-name                       (blc-file blc-index-dir "abbrevs.el")

 ;; ag
 ag-highlight-search                    t

 ;; alert
 alert-default-style                    'notifications

 ;; apt-utils
 apt-utils-show-all-versions            t

 ;; asm-mode
 asm-comment-char                       ?#

 ;; auth-source
 auth-source-cache-expiry               (blc-mins-to-secs 15)
 auth-source-debug                      'trivia
 auth-sources                           '(blc-pass)

 ;; avy
 avy-all-windows                        'all-frames
 avy-background                         t

 ;; battery
 battery-load-critical                  20
 battery-mode-line-format               "%L"

 ;; bbdb
 bbdb-complete-mail-allow-cycling       t
 bbdb-default-country                   nil
 bbdb-mua-summary-mark                  "✓"
 bbdb-name-format                       'last-first
 bbdb-phone-style                       nil
 bbdb-pop-up-window-size                t
 bbdb-read-name-format                  'first-last

 ;; bibtex
 bibtex-align-at-equal-sign             t
 bibtex-autokey-titleword-length        'infty
 bibtex-autokey-titlewords              1
 bibtex-autokey-titlewords-stretch      0
 bibtex-autokey-year-length             4
 bibtex-autokey-year-title-separator    ""
 bibtex-entry-kill-ring-max             kill-ring-max
 bibtex-field-kill-ring-max             kill-ring-max
 bibtex-maintain-sorted-entries         t
 bibtex-search-entry-globally           t

 ;; bindings
 mode-line-percent-position             '(-3 "%o")

 ;; bog
 bog-keymap-prefix                      "\C-cb"
 bog-root-directory                     (blc-parent-dir blc-bib-dir)

 ;; bookmark
 bookmark-default-file                  (blc-file blc-index-dir "bookmarks.el")
 bookmark-save-flag                     1
 bookmark-search-delay                  0

 ;; browse-url
 browse-url-browser-function            #'blc-browse-url
 browse-url-firefox-new-window-is-tab   t

 ;; calendar
 calendar-date-style                    'iso
 calendar-christian-all-holidays-flag   t
 calendar-islamic-all-holidays-flag     t

 ;; chess
 chess-images-default-size              blc-chars-per-line

 ;; comint
 comint-input-ignoredups                t

 ;; compile
 compilation-message-face               'default
 compilation-scroll-output              'first-error
 compile-command                        "make"

 ;; copyright
 copyright-names-regexp                 (regexp-quote user-full-name)

 ;; counsel
 counsel-describe-function-preselect    #'ivy-function-called-at-point
 counsel-git-grep-skip-counting-lines   t
 counsel-grep-base-command              "ag --nocolor %s %s" ; Smart case
 counsel-mode-map                       ()                   ; Control remaps
 counsel-org-goto-display-tags          t
 counsel-org-goto-display-todo          t
 counsel-org-goto-face-style            'verbatim
 counsel-yank-pop-filter                #'identity

 ;; csv-mode
 csv-align-style                        'auto

 ;; cus-edit
 custom-unlispify-menu-entries          nil
 custom-unlispify-tag-names             nil

 ;; custom
 custom-theme-directory                 (blc-dir user-emacs-directory "lisp")

 ;; debbugs
 debbugs-gnu-emacs-current-release      "26.1"
 debbugs-gnu-send-mail-function         #'message-send-mail-with-sendmail
 debbugs-gnu-trunk-directory            source-directory

 ;; diary-lib
 diary-comment-start                    ";"
 diary-number-of-entries                3

 ;; diff-mode
 diff-font-lock-prettify                t

 ;; dired
 dired-auto-revert-buffer               t
 dired-dwim-target                      t
 dired-listing-switches                 (string-join
                                         '("--almost-all"
                                           "--classify"
                                           "--group-directories-first"
                                           "--human-readable"
                                           "-l")
                                         " ")
 dired-recursive-copies                 'always

 ;; dired-aux
 dired-create-destination-dirs          'ask

 ;; disaster
 disaster-objdump
 "objdump -D -M att -Sl --no-show-raw-insn"

 ;; doc-view
 doc-view-resolution                    150

 ;; dropbox
 dropbox-locale                         "en_IE"
 dropbox-verbose                        t

 ;; emms
 emms-volume-change-function            #'emms-volume-pulse-change

 ;; enwc
 enwc-ask-to-save-interfaces            nil
 enwc-default-backend                   'nm

 ;; epg-config
 epg-pinentry-mode                      'loopback

 ;; ess
 ess-default-style                      'DEFAULT
 ess-indent-from-lhs                    nil

 ;; eww
 eww-bookmarks-directory                blc-index-dir
 eww-search-prefix
 "https://encrypted.google.com/search?ie=utf-8&oe=utf-8&q="

 ;; ffap
 dired-at-point-require-prefix          t
 ffap-file-finder                       #'blc-counsel-find-file
 ffap-require-prefix                    t
 ffap-rfc-path                          "https://ietf.org/rfc/rfc%s.txt"

 ;; files
 auto-save-visited-interval             auto-save-timeout
 backup-by-copying                      t
 backup-directory-alist                 '(("" . "~/.backup/"))
 delete-old-versions                    t
 directory-free-space-args              "-hP"
 find-file-visit-truename               t
 kept-new-versions                      4
 kept-old-versions                      2
 mode-require-final-newline             nil
 version-control                        t
 view-read-only                         t

 ;; footnote
 footnote-body-tag-spacing              1
 footnote-mode-line-string              ""
 footnote-section-tag                   ""
 footnote-spaced-footnotes              nil

 ;; frame
 window-divider-default-right-width     2

 ;; generic-x
 generic-use-find-file-hook             nil

 ;; ggtags
 ggtags-enable-navigation-keys          nil

 ;; git-annex
 git-annex-commit                       nil

 ;; git-commit
 git-commit-summary-max-length          50
 global-git-commit-mode                 nil

 ;; gnus
 gnus-home-directory                    user-emacs-directory
 gnus-init-file                         (expand-file-name
                                         "gnus" gnus-home-directory)

 ;; gnus-desktop-notify
 gnus-desktop-notify-groups             'gnus-desktop-notify-explicit
 gnus-desktop-notify-format             "%3n: %G"
 gnus-desktop-notify-function           #'gnus-desktop-notify-send

 ;; gnutls
 gnutls-min-prime-bits                  nil

 ;; hacker-typer
 hacker-typer-show-hackerman            t

 ;; haskell-mode
 haskell-completing-read-function       #'completing-read
 haskell-indent-offset                  2
 haskell-notify-p                       t
 haskell-process-log                    t
 haskell-process-suggest-hoogle-imports t
 haskell-process-suggest-remove-import-lines
 t

 ;; helm-make
 helm-make-cache-targets                t
 helm-make-completion-method            'ivy
 helm-make-list-target-method           'qp
 helm-make-require-match                nil

 ;; help
 help-window-select                     'other

 ;; holidays
 holiday-bahai-holidays                 ()
 holiday-oriental-holidays              ()

 ;; htmlize
 htmlize-css-name-prefix                "htmlize-"
 htmlize-html-charset                   "utf-8"
 htmlize-html-major-mode                #'mhtml-mode

 ;; ibuf-ext
 ibuffer-filter-groups
 '(("Book   " (or (derived-mode . bookmark-bmenu-mode)
                  (derived-mode . bookmark-edit-annotation-mode)
                  (and (name . "Bookmark Annotation")
                       (starred-name))))
   ("Code   " (and (or (derived-mode . prog-mode)
                       (derived-mode . conf-mode))
                   (not (saved . "package"))
                   (not (saved . "REPL"))))
   ("Custom " (derived-mode . Custom-mode))
   ("Dired  " (derived-mode . dired-mode))
   ("Git    "  (or (derived-mode . magit-mode)
                   (derived-mode . magit-repolist-mode)))
   ("Gnus   " (or (saved . "gnus")
                  (derived-mode . gnus-server-mode)
                  (predicate . (equal (bound-and-true-p gnus-dribble-buffer)
                                      (buffer-name)))))
   ("Help   " (or (predicate . (apply #'derived-mode-p
                                      ibuffer-help-buffer-modes))
                  (and (name . "Ivy Help")
                       (starred-name))))
   ("Image  " (derived-mode . image-mode))
   ("IRC    " (or (derived-mode . erc-mode)
                  (and (name . "erc-protocol")
                       (starred-name))))
   ("Log    " (or (derived-mode . TeX-output-mode)
                  (derived-mode . compilation-mode)
                  (derived-mode . ivy-occur-mode)
                  (derived-mode . messages-buffer-mode)
                  (derived-mode . tags-table-mode)
                  (predicate . (seq-some
                                (apply-partially #'equal (buffer-name))
                                (append blc-gnus-log-buffers
                                        (blc-as-list
                                         (bound-and-true-p dired-log-buffer)))))
                  (and (starred-name)
                       (or (name . "Backtrace")
                           (name . "Warnings")
                           (name . "WoMan-Log")
                           (name . "dropbox")
                           (name . "mbsync")))))
   ("PDF    " (derived-mode . pdf-view-mode))
   ("Package" (and (saved . "package")
                   (not (saved . "REPL"))))
   ("Process" (and (name . "Async Shell Command")
                   (starred-name)))
   ("REPL   " (saved . "REPL"))
   ("SX     " (or (derived-mode . sx-compose-mode)
                  (derived-mode . sx-question-list-mode)
                  (derived-mode . sx-question-mode)
                  (and (name . "sx temp buffer")
                       (starred-name))))
   ("TeX    " (saved . "TeX"))
   ("Text   " (saved . "text document"))
   ("Web    " (saved . "web")))
 ibuffer-old-time                       12
 ibuffer-show-empty-filter-groups       nil

 ;; ibuffer
 ibuffer-always-compile-formats         t
 ibuffer-default-sorting-mode           'alphabetic
 ibuffer-jump-offer-only-visible-buffers
 t
 ibuffer-use-other-window               t

 ;; ido
 ido-enable-flex-matching               t

 ;; ielm
 ielm-noisy                             nil
 ielm-prompt                            "(>) "

 ;; indent
 indent-line-function                   #'insert-tab

 ;; irony-eldoc
 irony-eldoc-use-unicode                t

 ;; isearch
 isearch-allow-scroll                   t
 search-exit-option                     nil

 ;; ivy
 ivy-action-wrap                        t
 ivy-count-format                       "(%d/%d) "
 ivy-extra-directories                  ()
 ivy-format-function                    #'ivy-format-function-arrow
 ivy-on-del-error-function              #'ignore

 ;; ivy-bibtex
 bibtex-completion-display-formats
 '((t . "\
${author:30} ${date:4} ${title:*} ${=has-pdf=:1}${=has-note=:1} ${=type=:14}"))

 ;; jit-lock
 jit-lock-stealth-load                  60
 jit-lock-stealth-time                   4

 ;; js
 js-indent-level                        4
 js-switch-indent-offset                js-indent-level

 ;; js-2
 js2-allow-rhino-new-expr-initializer   nil
 js2-bounce-indent-p                    t
 js2-global-externs                     '("define" "location")
 js2-highlight-level                    3
 js2-include-node-externs               t
 js2-mode-assume-strict                 t
 js2-skip-preprocessor-directives       t

 ;; ledger-mode
 ledger-post-amount-alignment-at        :decimal
 ledger-report-auto-refresh-sticky-cursor
 t
 ledger-report-use-header-line          t
 ledger-use-iso-dates                   t

 ;; lpr
 lpr-add-switches                       nil
 lpr-command                            "hp-print"

 ;; magit-branch
 magit-branch-popup-show-variables      t

 ;; magit-diff
 magit-diff-adjust-tab-width            t
 magit-diff-refine-hunk                 t
 magit-revision-use-hash-sections       'quickest

 ;; magit-git
 magit-list-refs-sortby                 "-creatordate"
 magit-prefer-remote-upstream           t

 ;; magit-mode
 magit-display-buffer-function
 #'magit-display-buffer-same-window-except-diff-v1

 ;; magit-process
 magit-process-finish-apply-ansi-colors t

 ;; magit-refs
 magit-refs-pad-commit-counts           t

 ;; magit-remote
 magit-remote-add-set-remote.pushDefault
 'ask

 ;; magit-repos
 magit-repository-directories           `((,blc-index-dir . 2))

 ;; magit-utils
 magit-view-git-manual-method           'man

 ;; make-mode
 makefile-macro-assign                  " := "
 makefile-pickup-everything-picks-up-filenames-p
 t
 makefile-tab-after-target-colon        nil

 ;; man
 Man-notify-method                      'aggressive

 ;; map-ynp
 read-answer-short                      t

 ;; markdown-mode
 markdown-fontify-code-blocks-natively  t
 markdown-header-scaling                t

 ;; message
 message-confirm-send                   t
 message-forward-before-signature       nil
 message-make-forward-subject-function  #'message-forward-subject-fwd
 message-mail-alias-type                nil
 message-send-mail-function             #'message-send-mail-with-sendmail
 message-sendmail-envelope-from         'header
 message-signature                      (car (split-string user-full-name))

 ;; mines
 mines-empty-cell-char                  ?\s
 mines-flagged-cell-char                ?⚑

 ;; minibuffer
 completing-read-function               #'ivy-completing-read
 completion-in-region-function          #'ivy-completion-in-region

 ;; minimap
 minimap-highlight-line                 nil
 minimap-recenter-type                  'relative
 minimap-width-fraction                 0.05
 minimap-window-location                'right

 ;; mm-decode
 mm-decrypt-option                      'ask
 mm-external-terminal-program           "x-terminal-emulator"
 mm-inline-large-images                 'resize
 mm-sign-option                         'guided
 mm-verify-option                       'always

 ;; mml-sec
 mml-secure-verbose                     t

 ;; mpc
 mpc-frame-alist                        '((tool-bar-lines . 1))

 ;; mule-cmds
 default-input-method                   "greek"

 ;; mwheel
 mwheel-tilt-scroll-p                   t

 ;; newsticker
 newsticker-date-format                 "(%F %a %R %:::z)"
 newsticker-treeview-date-format        "%F %R %:::z "
 newsticker-treeview-own-frame          t
 newsticker-url-list
 '(("HN"
    "https://hnrss.org/frontpage")
   ("LWN"
    "https://lwn.net/headlines/rss")
   ("Nautilus"
    "http://nautil.us/rss/all")
   ("NYT"
    "https://nytimes.com/services/xml/rss/userland/HomePage.xml")
   ("NYT Tech"
    "https://nytimes.com/services/xml/rss/userland/Technology.xml")
   ("QotD"
    "https://feeds.feedburner.com/quotationspage/qotd")
   ("Spiegel"
    "http://spiegel.de/schlagzeilen/tops/index.rss")
   ("Spiegel Net"
    "http://spiegel.de/netzwelt/index.rss")
   ("Spiegel Sci"
    "http://spiegel.de/wissenschaft/index.rss")
   ("Wired"
    "https://wired.com/feed/rss"))
 newsticker-url-list-defaults           ()

 ;; novice
 disabled-command-function              nil

 ;; nsm
 nsm-settings-file                      (blc-file blc-index-dir
                                                  "network-security.el")

 ;; ob-python
 org-babel-python-command               "python3"

 ;; org
 org-directory                          (blc-dir user-emacs-directory "org")
 org-default-notes-file                 (blc-file org-directory "notes.org")

 org-M-RET-may-split-line               nil
 org-agenda-files                       (expand-file-name "agenda-index"
                                                          org-directory)
 org-archive-location                   (format "%s::" (blc-file org-directory
                                                                 "archive.org"))
 org-catch-invisible-edits              'smart
 org-columns-default-format             "%ITEM %TODO %LOCATION %TAGS"
 org-ctrl-k-protect-subtree             t
 org-fontify-done-headline              t
 org-goto-interface                     'outline-path-completion
 org-goto-max-level                     10
 org-hierarchical-todo-statistics       nil
 org-highlight-latex-and-related        '(entities latex script)
 org-log-done                           'note
 org-log-into-drawer                    t
 org-log-redeadline                     'note
 org-log-reschedule                     'note
 org-modules                            '(org-bibtex
                                          org-bookmark
                                          org-docview
                                          org-eshell
                                          org-eww
                                          org-gnus
                                          org-id
                                          org-info
                                          org-man)
 org-outline-path-complete-in-steps     nil
 org-property-format                    "%-20s %s"
 org-refile-allow-creating-parent-nodes 'confirm
 org-refile-targets                     `((org-agenda-files
                                           . (:maxlevel . ,org-goto-max-level)))
 org-refile-use-outline-path            'file
 org-reverse-note-order                 t
 org-special-ctrl-a/e                   t
 org-startup-indented                   t
 org-todo-keywords
 '((type "NEXT(n)" "TODO(t)" "PROJ(p)" "EXEC(e)" "MEET(m)" "WAIT(w)" "BALK(b)"
         "|" "DONE(d!)" "VOID(v@)"))
 org-treat-S-cursor-todo-selection-as-state-change
 nil
 org-use-speed-commands                 t

 ;; org-agenda
 org-agenda-category-icon-alist         '(("" (space :width (16))))
 org-agenda-deadline-leaders            '("" "%3dd +" "%3dd -")
 org-agenda-scheduled-leaders           '("" "%3dd -")
 org-agenda-timerange-leaders           '("" "(%d/%d)")
 org-agenda-todo-ignore-with-date       t
 org-agenda-todo-list-sublevels         nil
 org-agenda-window-setup                'other-frame

 ;; org-capture
 org-capture-bookmark                   nil

 ;; org-clock
 org-clock-idle-time                    10
 org-clock-persist                      'history
 org-clock-persist-file                 (blc-file blc-index-dir
                                                  "org-clock-save.el")

 ;; org-footnote
 org-footnote-section                   nil

 ;; org-list
 org-checkbox-hierarchical-statistics   nil
 org-list-demote-modify-bullet          '(("+" . "-") ("-" . "+"))
 org-list-use-circular-motion           t

 ;; ox
 org-export-coding-system               'utf-8

 ;; ox-html
 org-html-checkbox-type                 'html
 org-html-doctype                       "html5"
 org-html-html5-fancy                   t
 org-html-htmlize-output-type           'css
 org-html-metadata-timestamp-format     "%F %a %R %Z"
 org-html-validation-link               ""

 ;; ox-publish
 org-publish-use-timestamps-flag        nil

 ;; paragraphs
 sentence-end-double-space              nil

 ;; password-cache
 password-cache                         nil

 ;; pcomplete
 pcomplete-ignore-case                  t

 ;; pdf-misc
 pdf-misc-print-programm                lpr-command

 ;; pdf-view
 pdf-view-display-size                  'fit-page

 ;; proced
 proced-auto-update-flag                t

 ;; prolog
 prolog-system                          'swi

 ;; reftex
 reftex-cite-format                     'biblatex
 reftex-comment-citations               t
 reftex-plug-into-AUCTeX                t
 reftex-revisit-to-follow               t

 ;; saveplace
 save-place-file                        (blc-file blc-index-dir "places.el")

 ;; sendmail
 sendmail-program                       "msmtp"

 ;; sh-script
 sh-basic-offset                        2
 sh-indentation                         2

 ;; shell
 explicit-shell-file-name               (or (getenv "ESHELL")
                                            (getenv "SHELL")
                                            "/bin/bash")

 ;; shr
 shr-bullet                             "• "
 shr-hr-line                            ?─
 shr-width                              blc-chars-per-line

 ;; simple
 async-shell-command-display-buffer     nil
 kill-do-not-save-duplicates            t
 kill-whole-line                        t
 mail-user-agent                        'gnus-user-agent
 next-error-recenter                    '(4)
 read-mail-command                      'gnus
 save-interprogram-paste-before-kill    t
 set-mark-command-repeat-pop            t
 yank-pop-change-selection              t

 ;; solar
 calendar-time-display-form
 '(24-hours ":" minutes (and time-zone (concat " (" time-zone ")")))

 ;; solarized
 solarized-distinct-doc-face            t
 solarized-distinct-fringe-background   t
 solarized-use-variable-pitch           nil

 ;; speedbar
 speedbar-show-unknown-files            t
 speedbar-update-flag                   nil
 speedbar-use-images                    t
 speedbar-vc-do-check                   nil

 ;; startup
 inhibit-default-init                   t
 inhibit-startup-screen                 t
 initial-scratch-message
 (or (blc-with-contents (or (getenv "COWTUNE_FILE") "~/.cowtune")
       (let ((pmin (point-min-marker))
             (pmax (point-max-marker))
             (comment-start ";;")
             (comment-empty-lines t)
             delete-trailing-lines)
         (comment-region             pmin pmax)
         (delete-trailing-whitespace pmin pmax)
         (buffer-substring           pmin pmax)))
     initial-scratch-message)
 user-mail-address                      (or (car (blc-msmtp-addresses))
                                            user-mail-address)

 ;; swiper
 swiper-goto-start-of-match             t

 ;; sx
 sx-question-mode-comments-format       "%s:\n   %s\n"

 ;; term
 term-ansi-buffer-base-name             "term"
 term-buffer-maximum-size               4096
 term-suppress-hard-newline             t

 ;; tex
 TeX-auto-save                          t
 TeX-engine                             'xetex
 TeX-parse-self                         t
 TeX-PDF-mode                           t

 ;; tex-style
 LaTeX-csquotes-open-quote              "\\enquote{"
 LaTeX-csquotes-close-quote             "}"

 ;; time
 display-time-format                    ""
 display-time-default-load-average      nil
 display-time-use-mail-icon             t
 display-time-world-list
 (map-apply (lambda (loc props)
              (list (apply #'blc--location-to-tz loc props) loc))
            blc-locations)
 display-time-world-time-format         "%a %d %b %R %:::z"

 ;; tls
 tls-checktrust                         'ask

 ;; tooltip
 tooltip-resize-echo-area               t

 ;; tramp
 tramp-default-method                   "rsync"

 ;; tramp-cache
 tramp-persistency-file-name            (blc-file blc-index-dir "tramp")

 ;; type-break
 type-break-demo-boring-stats           t
 type-break-file-name                   nil
 type-break-good-rest-interval          (blc-mins-to-secs  5)
 type-break-interval                    (blc-mins-to-secs 20)
 type-break-keystroke-threshold         '(nil)
 type-break-keystroke-warning-intervals ()
 type-break-terse-messages              t
 type-break-time-stamp-format           "[type-break %R] "
 type-break-time-warning-intervals      ()

 ;; uniquify
 uniquify-after-kill-buffer-p           nil
 uniquify-buffer-name-style             'forward
 uniquify-min-dir-content               1
 uniquify-trailing-separator-p          t

 ;; vc-hooks
 vc-handled-backends                    '(Git)

 ;; warnings
 warning-minimum-log-level              :debug

 ;; wdired
 wdired-allow-to-change-permissions     t
 wdired-use-dired-vertical-movement     t

 ;; webjump
 webjump-sites
 '(("Book Depository"
    . [simple-query "https://bookdepository.com/"
                    "https://bookdepository.com/search?searchTerm=hi"
                    ""])
   ("Browse URL"
    .               blc-webjump-browse-url)
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
   ("GitLab"
    . [mirrors      "https://gitlab.com/"
                    "https://gitlab.com/basil-conto"
                    "https://gitlab.com/dashboard"
                    "https://gitlab.com/dashboard/issues"
                    "https://gitlab.com/dashboard/merge_requests"
                    "https://gitlab.com/dashboard/todos"
                    "https://gitlab.com/search"])
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
                    ""]))

 ;; whitespace
 whitespace-style                       '(face lines-tail tab-mark)

 ;; windmove
 windmove-window-distance-delta         2
 windmove-wrap-around                   t

 ;; window
 frame-auto-hide-function               #'blc-delete-spare-frame
 pop-up-frames                          'graphic-only
 scroll-error-top-bottom                t
 split-window-keep-point                nil

 ;; wttrin
 wttrin-default-accept-language         '("Accept-Language" . "el,en,*")
 wttrin-default-cities
 (cons "Moon" (map-apply
               (pcase-lambda (loc (app (apply #'blc--country-xref) country))
                 (format "%s, %s" loc (plist-get country :name)))
               blc-locations)))

;;;; HOOKS

(blc-hook
  ;; auctex
  (:hooks TeX-after-compilation-finished-functions
          :fns TeX-revert-document-buffer)
  (:hooks LaTeX-mode-hook :fns (blc-LaTeX-command-default
                                turn-on-reftex))

  ;; auth-source
  (:hooks auth-source-backend-parser-functions :fns blc-pass-backend-parse)

  ;; bbdb
  (:hooks gnus-started-hook :fns blc-bbdb-set-gnus-summary-line-format)
  (:hooks gnus-startup-hook :fns bbdb-insinuate-gnus)

  ;; blc-lib
  (:hooks after-init-hook :fns blc-dropbox-mode)
  (:fns blc-turn-off-trailing-whitespace :hooks (Info-mode-hook
                                                 calendar-mode-hook
                                                 eww-mode-hook
                                                 help-mode-hook
                                                 magit-popup-mode-hook
                                                 message-mode-hook
                                                 minibuffer-setup-hook
                                                 term-mode-hook
                                                 youtube-dl-list-mode-hook))

  ;; bog
  (:hooks org-mode-hook :fns bog-mode)

  ;; bug-reference
  (:hooks prog-mode-hook :fns bug-reference-prog-mode)
  (:hooks text-mode-hook :fns bug-reference-mode)

  ;; cc-mode
  (:hooks c-mode-common-hook :fns (blc-turn-on-c++-comments
                                   hs-minor-mode))

  ;; csv-mode
  (:hooks csv-mode-hook :fns blc-csv-align-all-fields)

  ;; dafny-mode
  (:hooks dafny-mode-hook :fns blc-turn-off-prettify-symbols)

  ;; electric
  (:fns blc-turn-off-electric-indent-local :hooks (apt-sources-list-mode-hook
                                                   bibtex-mode-hook
                                                   conf-mode-hook
                                                   dafny-mode-hook
                                                   haskell-cabal-mode-hook
                                                   haskell-mode-hook
                                                   js2-mode-hook
                                                   lisp-mode-hook
                                                   restclient-mode-hook
                                                   text-mode-hook))

  ;; elisp-mode
  (:hooks emacs-lisp-mode-hook :fns (blc-rainbow-mode
                                     blc-turn-on-lexical-binding))

  ;; eshell
  (:hooks eshell-output-filter-functions :fns eshell-truncate-buffer)

  ;; executable
  (:hooks after-save-hook
          :fns executable-make-buffer-file-executable-if-script-p)

  ;; ffap
  (:hooks ffap-gnus-hook :hooks (gnus-article-mode-hook
                                 gnus-summary-mode-hook))

  ;; flycheck
  (:hooks flycheck-mode-hook :fns blc-turn-off-flycheck)

  ;; footnote
  (:hooks message-setup-hook :fns footnote-mode)

  ;; git-commit
  (:hooks git-commit-setup-hook :fns (blc-git-commit-set-fill-column
                                      blc-turn-on-double-space-sentence-ends
                                      bug-reference-mode))

  ;; gitconfig-mode
  (:hooks gitconfig-mode-hook :fns blc-turn-off-indent-tabs)

  ;; gnus
  (:hooks gnus-load-hook    :fns blc-gc-thresh-maximise)
  (:hooks gnus-started-hook :fns blc-gc-thresh-restore )

  ;; gnus-dired
  (:hooks dired-mode-hook :fns turn-on-gnus-dired-mode)

  ;; haskell-mode
  (:hooks haskell-mode-hook :fns haskell-indent-mode)

  ;; hi-lock
  (:hooks hi-lock-mode-hook :fns blc-hi-lock-no-eof-nl)

  ;; hl-line
  (:fns hl-line-mode :hooks (dired-mode-hook
                             finder-mode-hook
                             git-rebase-mode-hook
                             gnus-group-mode-hook
                             hackernews-mode-hook
                             ibuffer-mode-hook
                             ivy-occur-mode-hook
                             ledger-report-mode-hook
                             newsticker-treeview-list-mode
                             org-agenda-mode-hook
                             tabulated-list-mode-hook))

  ;; ielm
  (:hooks ielm-mode-hook :fns blc-turn-on-lexical-binding)

  ;; irony
  (:hooks irony-mode-hook :fns (irony-cdb-autosetup-compile-options
                                irony-eldoc))

  ;; isearch
  (:hooks isearch-mode-hook :fns blc-isearch-delight)

  ;; js2-mode
  (:hooks js2-mode-hook :fns (js2-highlight-unused-variables-mode
                              js2-refactor-mode
                              blc-xref-js2-install-backend))

  ;; markdown-mode
  (:hooks markdown-mode-hook :fns visual-line-mode)

  ;; message
  (:hooks message-mode-hook :fns blc-turn-on-double-space-sentence-ends)
  (:hooks message-send-hook :fns blc-message-confirm-attach)
  (:hooks message-subscribed-address-functions
          :fns gnus-find-subscribed-addresses)

  ;; mm-decode
  (:hooks mm-file-name-rewrite-functions :hooks mm-file-name-replace-whitespace)

  ;; org
  (:hooks org-capture-before-finalize-hook :fns blc-org-prop-captured)

  ;; pascal
  (:hooks pascal-mode-hook :fns blc-turn-on-c++-comments)

  ;; pdf-view
  (:hooks pdf-view-mode-hook :fns blc-pdf-tools-undefer)

  ;; python
  (:hooks python-mode-hook :fns blc-python-pep-8-comments)

  ;; sass-mode
  (:hooks sass-mode-hook :fns blc-turn-on-c++-comments)

  ;; simple
  (:fns display-line-numbers-mode :hooks visual-line-mode-hook)
  (:fns turn-on-auto-fill         :hooks (bookmark-edit-annotation-mode
                                          LaTeX-mode-hook
                                          org-mode-hook))

  ;; startup
  (:hooks window-setup-hook :append t :fns (blc-report-init-time
                                            blc-gc-thresh-restore))

  ;; term
  (:hooks term-exec-hook :fns blc-term-rename)

  ;; text-mode
  (:hooks text-mode-hook :fns blc-indent-relative-first-indent-point)

  ;; writeroom-mode
  (:hooks writeroom-mode-hook :fns blc-visual-auto-fill-column))

;;;; BINDINGS

(define-prefix-command 'blc-jump-map)
(define-prefix-command 'blc-org-map)

(fmakunbound 'sx-switchto-map)
(autoload 'sx-switchto-map "sx-switchto" nil t 'keymap)

(blc-define-keys
  ((current-global-map)
   ([?\C-\S-v]                            . #'scroll-other-window)
   ([S-next]                              . #'blc-small-scroll-up)
   ([S-prior]                             . #'blc-small-scroll-down)
   ([S-up]                                . #'windmove-up)
   ([S-down]                              . #'windmove-down)
   ([S-left]                              . #'windmove-left)
   ([S-right]                             . #'windmove-right)
   ([f5]                                  . #'blc-revert-buffer)
   ([remap bookmark-jump]                 . #'counsel-bookmark)
   ([remap capitalize-word]               . #'capitalize-dwim)
   ([remap count-lines-page]              . #'blc-count-lines)
   ([remap describe-bindings]             . #'counsel-descbinds)
   ([remap describe-function]             . #'counsel-describe-function)
   ([remap describe-variable]             . #'counsel-describe-variable)
   ([remap dired]                         . #'dired-at-point)
   ([remap dired-other-window]            . #'ffap-dired-other-window)
   ([remap dired-other-frame]             . #'ffap-dired-other-frame)
   ([remap downcase-word]                 . #'downcase-dwim)
   ([remap execute-extended-command]      . #'counsel-M-x)
   ([remap fill-paragraph]                . #'unfill-toggle)
   ([remap find-file]                     . #'find-file-at-point)
   ([remap find-file-other-window]        . #'ffap-other-window)
   ([remap find-file-other-frame]         . #'ffap-other-frame)
   ([remap imenu]                         . #'counsel-imenu)
   ([remap info]                          . #'blc-info)
   ([remap info-other-window]             . #'blc-info-other-window)
   ([remap info-lookup-symbol]            . #'counsel-info-lookup-symbol)
   ([remap just-one-space]                . #'cycle-spacing)
   ([remap list-buffers]                  . #'ibuffer)
   ([remap load-library]                  . #'counsel-load-library)
   ([remap load-theme]                    . #'counsel-load-theme)
   ([remap menu-bar-open]                 . #'counsel-tmm)
   ([remap org-goto]                      . #'counsel-org-goto)
   ([remap org-set-tags-command]          . #'counsel-org-tag)
   ([remap pop-global-mark]               . #'counsel-mark-ring)
   ([remap save-buffers-kill-terminal]    . #'save-buffers-kill-emacs)
   ([remap switch-to-buffer]              . #'ivy-switch-buffer)
   ([remap switch-to-buffer-other-window] . #'ivy-switch-buffer-other-window)
   ([remap upcase-word]                   . #'upcase-dwim)
   ([remap yank-pop]                      . #'counsel-yank-pop)
   ([remap zap-to-char]                   . #'zap-up-to-char))

  (mode-specific-map
   ([?\M-g]                               . #'magit-file-popup)
   ([?\M-n]                               . #'next-logical-line)
   ([?\M-p]                               . #'previous-logical-line)
   ("/"                                   . #'define-word-at-point)
   ("["                                   . #'raise-sexp)
   ("]"                                   . #'delete-pair)
   ("C"                                   . #'copy-from-above-command)
   ("M"                                   . #'blc-mbsync-all)
   ("b"                                   . #'bog-command-map)
   ("e"                                   . #'ielm)
   ("4e"                                  . #'blc-ielm-other-window)
   ("g"                                   . #'blc-github-notifications)
   ("i"                                   . #'blc-indent-relative)
   ("j"                                   . #'blc-jump-map)
   ("m"                                   . #'blc-mbsync)
   ("n"                                   . #'blc-rename-buffer)
   ("o"                                   . #'blc-org-map)
   ("s"                                   . #'sx-switchto-map)
   ("u"                                   . #'counsel-unicode-char))

  (blc-jump-map
   ("b"                                   . #'ibuffer-jump)
   ("d"                                   . #'counsel-dired-jump)
   ("e"                                   . #'eww-open-in-new-buffer)
   ("f"                                   . #'project-find-file)
   ("l"                                   . #'counsel-locate)
   ("m"                                   . #'magit-find-file)
   ("4m"                                  . #'magit-find-file-other-window)
   ("p"                                   . #'blc-project-switch)
   ("r"                                   . #'ivy-resume)
   ("s"                                   . #'blc-scratch)
   ("t"                                   . #'blc-term)
   ("w"                                   . #'webjump))

  (blc-org-map
   ("a"                                   . #'org-agenda)
   ("c"                                   . #'org-capture)
   ("f"                                   . #'blc-org-find-file)
   ("4f"                                  . #'blc-org-find-file-other-window)
   ("l"                                   . #'org-store-link))

  (ctl-x-map
   ("\C-j"                                . #'dired-jump)
   ("\C-n"                                . #'blc-open-next-line)
   ("\C-p"                                . #'blc-open-previous-line)
   ([?\M-g]                               . #'magit-dispatch-popup)
   ("/"                                   . #'engine-mode-prefixed-map)
   ("7"                                   . #'blc-transpose-split)
   ("B"                                   . #'blc-bury-buffer)
   ("M"                                   . #'blc-gnus)
   ("g"                                   . #'magit-status))

  (ctl-x-4-map
   ("\C-j"                                . #'dired-jump-other-window)
   ("M"                                   . #'blc-gnus-other-window))

  (ctl-x-5-map
   ("3"                                   . #'blc-make-graphic-display)
   ("M"                                   . #'blc-gnus-other-frame))

  (ctl-x-r-map
   ("4b"                                  . #'bookmark-jump-other-window))

  (esc-map
   ("#"                                   . #'avy-goto-word-or-subword-1)
   ("+"                                   . #'er/expand-region)
   ("R"                                   . #'redraw-display)
   ("V"                                   . #'scroll-other-window-down)
   ("o"                                   . #'ace-window))

  (goto-map
   ("\t"                                  . #'blc-move-to-column)
   ("e"                                   . #'first-error)
   ("f"                                   . #'avy-goto-line))

  (help-map
   ("\C-m"                                . #'discover-my-major)
   ("\C-f"                                . #'find-function)
   ("4f"                                  . #'find-function-other-window)
   ("4\C-f"                               . #'find-function-other-window)
   ("5f"                                  . #'find-function-other-frame)
   ("5\C-f"                               . #'find-function-other-frame)
   ("\C-j"                                . #'find-face-definition)
   ("j"                                   . #'counsel-faces)
   ("\C-k"                                . #'find-function-on-key)
   ("4k"                                  . #'find-function-on-key-other-window)
   ("4\C-k"                               . #'find-function-on-key-other-window)
   ("5k"                                  . #'find-function-on-key-other-frame)
   ("5\C-k"                               . #'find-function-on-key-other-frame)
   ("\C-l"                                . #'find-library)
   ("4l"                                  . #'find-library-other-window)
   ("4\C-l"                               . #'find-library-other-window)
   ("5l"                                  . #'find-library-other-frame)
   ("5\C-l"                               . #'find-library-other-frame)
   ("\C-v"                                . #'find-variable)
   ("4\C-v"                               . #'find-variable-other-window)
   ("5\C-v"                               . #'find-variable-other-frame))

  (isearch-mode-map
   ([?\C-']                               . #'avy-isearch)
   ([up]                                  . #'isearch-repeat-backward)
   ([down]                                . #'isearch-repeat-forward))

  (search-map
   ("a"                                   . #'counsel-ag)
   ("g"                                   . #'counsel-git-grep)
   ("r"                                   . #'counsel-rg)
   ("s"                                   . #'counsel-grep-or-swiper)))


;;;; PACKAGES

;; ace-window

(with-eval-after-load 'ace-window
  (ace-window-display-mode))

;; ag

(with-eval-after-load 'ag
  (add-to-list 'ag-arguments "--context=5"))

;; apt-sources-list

(add-to-list 'auto-mode-alist
             (cons (rx (| ".sources"
                          (: "sources" (? ".list.d/" (+ anything)) ".list"))
                       eos)
                   #'apt-sources-list-mode))

;; atomic-chrome

(with-eval-after-load 'atomic-chrome
  (setq-default atomic-chrome-extension-type-list '(ghost-text))
  (blc-put* atomic-chrome-url-major-mode-alist "github\\.com" #'gfm-mode))

;; auctex

(with-eval-after-load 'latex
  (dolist (suffix '("fdb_latexmk" "vrb"))
    (add-to-list 'LaTeX-clean-intermediate-suffixes (blc-rx `(: ?. ,suffix)))))

(with-eval-after-load 'tex
  (define-key TeX-mode-map [remap TeX-documentation-texdoc] #'blc-TeX-doc)

  ;; Set priority of pre-configured PDF viewers
  (dolist (viewer '("Zathura" "PDF Tools"))
    (when (assoc viewer TeX-view-program-list-builtin)
      (push (list 'output-pdf viewer) TeX-view-program-selection)))

  ;; Configure latexmk commands
  (let* ((exe "latexmk")
         (nom (setq-default TeX-command-default (capitalize exe))))

    (dolist (pvc '(nil t))
      (let* ((nom (format "%s%s"     nom (if pvc " PVC" "")))
             (cmd (format "%s%s %%t" exe (if pvc "-pvc -view=none" "")))
             (dsc (format "Run %s"   nom)))
        (blc-put* TeX-command-list nom           ; Name
                  (list cmd                      ; Non-expanded command
                        #'TeX-run-command        ; Process handler
                        nil                      ; Confirm expanded command
                        '(latex-mode LaTeX-mode) ; Applicable modes
                        :help dsc))))))          ; Command

;; auth-source

(with-eval-after-load 'auth-source
  (blc-put auth-source-protocols 'smtp '("smtp" "smtps" "25" "465" "587")))

;; battery

(display-battery-mode)

;; bbdb

(with-eval-after-load 'bbdb
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

;; biblio-download

(with-eval-after-load 'biblio-download
  (when (require 'bog nil t)
    (setq-default biblio-download-directory bog-stage-directory)))

;; bibtex

(with-eval-after-load 'bibtex
  (bibtex-set-dialect reftex-cite-format)
  (map-do #'add-to-list
          '((bibtex-entry-format . realign)
            (bibtex-files        . bibtex-file-path))))

;; cc-mode

(with-eval-after-load 'cc-mode
  (let ((name "blc"))
    (c-add-style name '("linux"
                        (c-basic-offset . 2 )
                        (c-offsets-alist
                         (access-label      . / )
                         (arglist-close     . 0 )
                         (case-label        . + )
                         (inher-intro       . ++)
                         (inline-open       . 0 )
                         (innamespace       . 0 )
                         (member-init-intro . ++))))

    (blc-put c-default-style 'other name)))

;; chess

(with-eval-after-load 'chess
  (setq-default chess-images-directory
                (blc-dir (blc-package-dir 'chess) "pieces" "xboard")))

;; comint

(with-eval-after-load 'comint
  (define-key comint-mode-map "\C-c\C-r" nil))

;; conf-mode

(add-to-list 'auto-mode-alist (cons (rx ".dirs" eos) #'conf-unix-mode))

;; counsel

(with-eval-after-load 'counsel
  (ivy-add-actions #'counsel-M-x
                   `(("j" ,#'blc-counsel-M-x-other-window "other window"))))

;; cus-edit

(when (file-exists-p
       (setq-default custom-file
                     (expand-file-name "custom.el" user-emacs-directory)))
  (lwarn 'blc :warning "Custom file %s exists but not loaded" custom-file))

;; custom

(load-theme 'blc-light t)

;; dash

(with-eval-after-load 'dash
  (dash-enable-font-lock))

;; deb-view

(add-to-list 'auto-mode-alist (cons (rx ".deb" eos) #'deb-view-mode))
(with-eval-after-load 'dired
  (require 'deb-view nil t)
  ;; Dired Isearch support is not worth overriding `search-map'
  (define-key dired-mode-map (where-is-internal search-map nil t) nil))

;; delight

(delight
 '(;; abbrev
   (abbrev-mode nil abbrev)

   ;; autorevert
   (auto-revert-mode "↻" autorevert)

   ;; conf-mode
   (    conf-colon-mode "🔧[:]"  :major)
   (  conf-desktop-mode "🔧[🗔]"  :major)
   (    conf-space-mode "🔧[ ]"  :major)
   (     conf-unix-mode "🔧[🐧]" :major)
   (conf-xdefaults-mode "🔧[X]"  :major)

   ;; counsel
   (counsel-mode nil counsel)

   ;; eldoc
   (eldoc-mode nil eldoc)

   ;; elisp-mode
   (      emacs-lisp-mode "(ε)" :major)
   (lisp-interaction-mode "(ι)" :major)

   ;; eww
   (eww-mode "🕸" :major)

   ;; haskell-mode
   (haskell-indent-mode      nil haskell-indent)
   (haskell-mode
    (:eval (if (bound-and-true-p interactive-haskell-mode) "λ>" "λ"))
    :major)
   (interactive-haskell-mode nil haskell)

   ;; ielm
   (inferior-emacs-lisp-mode "(>)" :major)

   ;; info
   (Info-mode "📘" :major)

   ;; irony
   (irony-mode "🜜" irony)

   ;; ivy
   (ivy-mode nil ivy)

   ;; js2-mode
   (js2-mode "jsⅡ" :major)

   ;; js2-refactor
   (js2-refactor-mode nil js2-refactor)

   ;; magit
   (git-rebase-mode                 "±𝄢"  :major     )
   (magit-blame-mode                "🖜"   magit-blame)
   (magit-cherry-mode               "±🍒" :major     )
   (magit-diff-mode                 "±±"  :major     )
   (magit-log-mode                  "±㏒" :major     )
   (magit-log-select-mode           "±㏒" :major     )
   (magit-merge-preview-mode        "±⛙"  :major     )
   (magit-mode                      "±"   :major     )
   (magit-process-mode              "±👷" :major     )
   (magit-reflog-mode               "±🚑" :major     )
   (magit-refs-mode                 "±⚖"  :major     )
   (magit-repolist-mode             "±🖧"  :major     )
   (magit-revision-mode             "±¶"  :major     )
   (magit-stash-mode                "±︷" :major     )
   (magit-stashes-mode              "±︷" :major     )
   (magit-status-mode               "±"   :major     )
   (magit-submodule-list-mode       "±%"  :major     )
   (magit-wip-after-apply-mode      ""    magit-wip  )
   (magit-wip-after-save-local-mode ""    magit-wip  )
   (magit-wip-before-change-mode    ""    magit-wip  )

   ;; make-mode
   (makefile-automake-mode "⛏.am" :major)
   (makefile-mode          "⛏"    :major)
   (makefile-gmake-mode    "⛏GNU" :major)

   ;; markdown-mode
   (     gfm-mode "🐙" :major)
   (markdown-mode "🡇"  :major)

   ;; message
   (message-mode "🖹" :major)

   ;; rainbow-mode
   (rainbow-mode "🌈" rainbow-mode)

   ;; subword
   (subword-mode nil subword)

   ;; view
   (view-mode "👓" view)

   ;; whitespace
   (global-whitespace-mode nil whitespace)

   ;; wrap-region
   (wrap-region-mode nil wrap-region)))

;; delsel

(delete-selection-mode)

;; dired-aux

(with-eval-after-load 'dired-aux
  (mapc (apply-partially #'add-to-list 'dired-compress-files-alist)
        '(("\\.tar\\.7z\\'" . "tar -c %i | 7zr a -si %o")
          ("\\.7z\\'"       . "7zr a %o %i"))))

;; dired-x

(with-eval-after-load 'dired
  (require 'dired-x)

  (setq-default dired-omit-files (blc-rx `(| (: bos ?. (not (in ?.)))
                                             (regexp ,dired-omit-files))))

  (map-do (lambda (cmds suffs)
            (blc-put* dired-guess-shell-alist-user
                      (blc-rx `(: ?. (| ,@suffs) eos))
                      cmds))
          '((("jpegoptim")      "jpg" "jpeg")
            (("localc")         "ods" "xls" "xlsx")
            (("lowriter")       "doc" "docx" "odt")
            (("mpv")            "mkv" "mp4" "webm")
            (("mpv" "opusinfo") "opus")
            (("optipng")        "png")
            (("pdf")            "pdf"))))

;; disaster

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map "\C-cd" #'disaster))

;; engine-mode

(with-eval-after-load 'engine-mode
  ;; No eager autoloaded macro expansion
  (eval
   '(progn
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
        :keybinding "w"))
   lexical-binding)

  (engine-mode))

;; eww

(with-eval-after-load 'eww
  (blc-define-keys
    (eww-bookmark-mode-map
     ("T" . #'blc-eww-toggle-images)
     ("n" . #'next-line)
     ("p" . #'previous-line)
     ("w" . #'blc-eww-bookmark-save))))

;; exec-path-from-shell

(with-eval-after-load 'exec-path-from-shell
  (mapc (apply-partially #'add-to-list 'exec-path-from-shell-variables)
        '("SSH_AGENT_PID" "SSH_AUTH_SOCK")))

;; ffap

(with-eval-after-load 'ffap
  (add-to-list 'ffap-rfc-directories (blc-dir user-emacs-directory "rfc")))

;; files

(add-to-list 'safe-local-variable-values
             '(eval . (when buffer-file-name (view-mode))))
(auto-save-visited-mode)

;; find-func

(with-eval-after-load 'find-func
  (setq-default find-function-source-path (blc-src-path)))

;; flex-mode

(add-to-list 'auto-mode-alist (cons (rx ".lex" eos) #'flex-mode))

;; frame

(window-divider-mode)

;; ggtags

(with-eval-after-load 'ggtags
  (blc-define-keys
    (ggtags-mode-map
     ([?\M-\]]) ; `ggtags-find-reference'
     ([?\M-F]  . #'ggtags-find-reference))))

;; git-annex

(with-eval-after-load 'dired
  (require 'git-annex nil t))

;; git-commit

(when (boundp 'git-commit-filename-regexp)
  (add-to-list 'auto-mode-alist
               (cons git-commit-filename-regexp #'git-commit-setup)))

(with-eval-after-load 'git-commit
  (add-to-list 'git-commit-style-convention-checks 'overlong-summary-line))

;; gscholar-bibtex

(with-eval-after-load 'gscholar-bibtex
  (when-let ((src (assoc "Google Scholar" gscholar-bibtex-available-sources)))
    (setq-default gscholar-bibtex-default-source (car src))))

;; hacker-typer

(with-eval-after-load 'hacker-typer
  (require 'mm-util)
  (setq-default
   hacker-typer-files
   (mapcar (apply-partially #'concat "file://")
           (directory-files (blc-dir source-directory "src") t (rx ".c" eos) t))
   hacker-typer-random-range
   (mapcar (apply-partially #'* 2)
           hacker-typer-random-range)))

;; haskell-mode

(with-eval-after-load 'haskell-mode
  (define-key haskell-mode-map [remap haskell-hoogle] #'haskell-hayoo))

;; hi-lock

(with-eval-after-load 'hi-lock
  (mapc (apply-partially #'add-to-list 'hi-lock-exclude-modes)
        '(comint-mode
          completion-list-mode
          display-time-world-mode
          erc-mode
          eshell-mode
          lyrics-show-mode
          newsticker-treeview-item-mode
          term-mode))

  (when-let ((cell (assq 'hi-lock-mode minor-mode-alist)))
    (setcdr cell (blc-sed-tree " .+" "⛯" (cdr cell)))))

(global-hi-lock-mode)

;; hideshow

(with-eval-after-load 'hideshow
  (define-key hs-minor-mode-map "\C-c\t" #'hs-toggle-hiding))

;; highlight-escape-sequences

(turn-on-hes-mode)

;; holidays

(with-eval-after-load 'holidays
  ;; Remove redundant full-stops
  (dolist (sym '(calendar-holidays holiday-general-holidays))
    (set-default sym (blc-sed-tree "St\\(\\.\\)" "" (symbol-value sym) t t 1)))

  ;; Maximise Unicode usage
  (let ((lut '(("Christmas"        . "🎄")
               ("Good Friday"      . "🞤" )
               ("Halloween"        . "👻")
               ("Hanukkah"         . "🕎")
               ("Palm Sunday"      . "🌴")
               ("St Patrick's Day" . "☘" )
               ("Valentine's Day"  . "❤" ))))
    (setq-default calendar-holidays
                  (blc-sed-tree (regexp-opt (map-keys lut))
                                (lambda (day)
                                  (cdr (assoc-string day lut t)))
                                calendar-holidays))))

;; ibuf-ext

(with-eval-after-load 'ibuf-ext
  (mapc (apply-partially #'add-to-list 'ibuffer-saved-filters)
        `(("package" (directory . ,(regexp-opt
                                    (mapcar
                                     #'expand-file-name
                                     (list blc-dataroot-dir
                                           source-directory
                                           package-user-dir)))))
          ("REPL"    (or (derived-mode . eshell-mode)
                         (derived-mode . inferior-emacs-lisp-mode)
                         (derived-mode . lisp-interaction-mode))))))

;; ibuffer

(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map [remap ibuffer-find-file] #'blc-ibuffer-ffap)

  (mapc (apply-partially #'add-to-list 'ibuffer-help-buffer-modes)
        '(Man-mode woman-mode)))

;; info

(with-eval-after-load 'info
  (define-key Info-mode-map "k" #'blc-info-kill))

;; irfc

(add-to-list 'auto-mode-alist
             (cons (blc-rx `(: ,@(mapcar (lambda (c) `(in ,(upcase c) ,c))
                                         "rfc")
                               (group (+ digit)) ".txt" eos))
                   #'irfc-mode))

(with-eval-after-load 'irfc
  (require 'ffap)

  (blc-define-keys
    (irfc-mode-map
     ([remap scroll-down] . #'scroll-down-command)
     ([remap scroll-up]   . #'scroll-up-command)
     ("\C-?"              . #'scroll-down-command)))

  (setq-default
   irfc-directory         (seq-find #'identity ffap-rfc-directories)
   irfc-download-base-url (url-file-directory  ffap-rfc-path)))

;; ivy

(with-eval-after-load 'ivy
  ;; Keys
  (blc-define-keys
    (ivy-minibuffer-map
     ([?\S-\s]))
    (ivy-occur-mode-map
     ("n" . #'ivy-occur-next-line)
     ("p" . #'ivy-occur-previous-line)))

  (mapc (pcase-lambda (`[,olddef ,newdef ,oldmap])
          (substitute-key-definition olddef newdef ivy-minibuffer-map oldmap))
        `([,#'narrow-to-region ,#'ivy-restrict-to-matches ,(current-global-map)]
          [,#'isearch-yank-word-or-char ,#'ivy-yank-word ,isearch-mode-map]))

  ;; Default matching behaviour
  (blc-put ivy-re-builders-alist t #'ivy--regex-ignore-order)

  ;; Fix ordering
  (map-do (lambda (sort callers)
            (dolist (caller callers)
              (blc-put ivy-sort-functions-alist caller sort)))
          `((nil                 t)
            (,#'blc-path-lessp   ,#'project-find-file)
            (,#'blc-sort-reverse ,#'Info-complete-menu-item)
            (,#'string-lessp     ,#'blc-scratch
                                 ,#'counsel-M-x
                                 ,#'counsel-apropos
                                 ,#'counsel-describe-face
                                 ,#'counsel-describe-function
                                 ,#'counsel-describe-variable
                                 ,#'counsel-faces
                                 ,#'elisp-completion-at-point
                                 ,#'find-face-definition)))

  (dolist (caller (list #'Info-menu #'webjump))
    (setq ivy-completing-read-handlers-alist
          (map-delete ivy-completing-read-handlers-alist caller)))

  ;; Recursive minibuffers
  (minibuffer-depth-indicate-mode)

   ;; Do not match start of input for counsel or org commands
  (setq-default
   ivy-initial-inputs-alist
   (map-remove (lambda (cmd _)
                 (string-match-p (rx bos (| "org" "counsel") ?-)
                                 (symbol-name cmd)))
               ivy-initial-inputs-alist)))

;; ivy-bibtex

(with-eval-after-load 'ivy-bibtex
  (add-to-list 'bibtex-completion-additional-search-fields "date")
  (mapc (apply-partially #'add-to-list 'bibtex-completion-bibliography)
        (blc-bib-files))

  (when (require 'bog nil t)
    (setq-default bibtex-completion-library-path bog-file-directory
                  bibtex-completion-notes-path   bog-note-directory)))

;; jq-mode

(with-eval-after-load 'json-mode
  (define-key json-mode-map "\C-c\C-q" #'jq-interactively))

;; js

(with-eval-after-load 'js
  (setq-default js-enabled-frameworks
                (seq-intersection '(dojo javascript prototype)
                                  js-enabled-frameworks)))

;; js2-mode

(map-do (lambda (alist re)
          (add-to-list alist (cons re #'js2-mode)))
        `((auto-mode-alist        . ,(rx ".js" eos))
          (interpreter-mode-alist . ,(rx (| "node" "nodejs")))))

(with-eval-after-load 'js2-mode
  (blc-define-keys
    (js2-mode-map
     ("\r" . #'js2-line-break)
     ([?\M-.]))))

;; js2-refactor

(with-eval-after-load 'js2-refactor
  (js2r-add-keybindings-with-prefix "\C-c\C-m"))

;; ledger-mode

(add-to-list 'auto-mode-alist (cons (rx ".ledger" eos) #'ledger-mode))

(with-eval-after-load 'ledger-mode
  (setq-default ledger-default-date-format ledger-iso-date-format)

  (ledger-reports-add "blc-account"
                      (string-join '("%(binary)"
                                     "--dc"
                                     "--file %(ledger-file)"
                                     "register %(account)")
                                   " ")))

;; lunar

(with-eval-after-load 'lunar
  (setq-default
   lunar-phase-names
   (mapcar (lambda (name)
             (char-to-string (char-from-name (concat name " symbol") t)))
           lunar-phase-names)))

;; magit

(with-eval-after-load 'magit
  (require 'blc-magit))

;; magit-extras

(with-eval-after-load 'magit-extras
  ;; Adapted from URL `https://github.com/npostavs/emacs.d'
  (setq-default
   magit-pop-revision-stack-format
   (list "format:[%N] "
         "\
[%N]: %s
  %ci
  https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=%H\n"
         (caddr magit-pop-revision-stack-format))))

;; magit-log

(with-eval-after-load 'magit-log
  (let ((age (cdr magit-log-margin)))
    (setcar age 'age-abbreviated)
    (setcar (cdddr age) 16)))

;; make-mode

(with-eval-after-load 'make-mode
  (define-key makefile-mode-map "\C-c$" #'makefile-insert-macro-ref)

  ;; Expand GNU functions
  (map-do (lambda (fn args)
            (unless (assoc-string fn makefile-gnumake-functions-alist)
              (blc-put* makefile-gnumake-functions-alist fn args)))
          '(("abspath"  "Names")
            ("error"    "Text")
            ("flavor"   "Variable")
            ("info"     "Text")
            ("lastword" "Text")
            ("realpath" "Names")
            ("value"    "Variable")
            ("warning"  "Text")
            ("wordlist" "Start index" "End index" "Text")))

  (setq-default makefile-gnumake-functions-alist
                (seq-sort-by #'car #'string-lessp
                             makefile-gnumake-functions-alist))

  ;; Expand special targets
  (let ((targets 'makefile-special-targets-list))
    ;; Remove old-fashioned suffix rules
    (set targets (seq-remove (apply-partially #'string-match-p (rx "."))
                             (symbol-value targets)))

    (mapc (lambda (target)
            (add-to-list targets target))
          '("DEFAULT_GOAL"
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

;; man

(with-eval-after-load 'man
  (blc-define-keys
    (Man-mode-map
     ("]" . #'blc-man-next-buffer)
     ("[" . #'blc-man-previous-buffer))))

;; message

(with-eval-after-load 'message
  (define-key message-mode-map "\C-c\C-ff" #'blc-message-set-msmtp-from)

  (add-to-list 'message-required-mail-headers 'To)

  (when-let* ((addresses (blc-msmtp-addresses)))
    (setq-default message-alternative-emails (regexp-opt addresses)))

  (setq-default message-expand-name-databases
                (delq 'eudc message-expand-name-databases)))

;; mm-decode

(with-eval-after-load 'mm-decode
  (setq-default mm-default-directory (blc-user-dir "DOWNLOAD")))

;; mpc

(with-eval-after-load 'mpc
  (setq mpc-frame-alist (map-delete mpc-frame-alist 'font)))

;; org

(with-eval-after-load 'org
  (require 'dom)

  (dolist (cmd '(org-clock-in-last org-clock-out))
    (global-set-key (where-is-internal cmd org-mode-map t) cmd))

  (mapc (lambda (lang)
          (push (cons lang t) org-babel-load-languages))
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
   org-global-properties
   `(("Effort_ALL"
      . ,(concat
          "0 " (mapconcat #'org-duration-from-minutes
                          (mapcan (lambda (step)
                                    (number-sequence step (* step 3) step))
                                  '(15 60))
                          " ")))))

  (org-clock-persistence-insinuate))

;; org-agenda

(with-eval-after-load 'org-agenda
  (blc-define-keys
    (org-agenda-mode-map
     ([?\M-n] . #'blc-org-agenda-day-forward)
     ([?\M-p] . #'blc-org-agenda-day-backward)))

  (if-let* ((key "n")
            (cmd (seq-take (blc-get org-agenda-custom-commands key) 2))
            ((= (length cmd) 2)))
      (blc-put* org-agenda-custom-commands key
                `(,@cmd () ,(blc-file org-directory "agenda.html")))
    (lwarn 'blc :error "Could not hijack `org-agenda-custom-commands'"))

  (mapc (lambda (icon)
          (blc-put* org-agenda-category-icon-alist
                    (blc-rx `(: bos ,(file-name-base icon) eos))
                    (list (file-truename icon) nil nil :ascent 'center)))
        (and-let* ((dir (blc-dir org-directory "icons"))
                   ((file-directory-p dir)))
          (nreverse
           (directory-files dir t directory-files-no-dot-files-regexp t)))))

;; org-capture

(with-eval-after-load 'org-capture
  (setq-default
   org-capture-templates
   `(("t" . ("Task" entry (file+olp "" "Inbox")
             ,(string-join '("* %?"       ; Final point
                             "%i")        ; Active region contents
                           "\n")
             :prepend t :unnarrowed t))
     ("s" . ("Show" entry (file+olp "" "Show")
             ,(string-join '("* %?"
                             "%x"         ; X clipboard contents
                             "%i")
                           "\n")
             :prepend t :unnarrowed t))
     ("b" . ("Book" entry (file "books.org")
             ,(apply #'concat "* %? %^g"  ; Genre tag prompt
                     (mapcar (apply-partially #'format "%%^{%s}p")
                             '(title      ; Property prompts
                               author
                               publisher
                               published
                               published_orig
                               language
                               format
                               pages
                               price
                               discount)))
             :prepend t :unnarrowed t))
     ("e" . ("Entertainment" entry (file+olp "ents.org" "Inbox")
             "* %?"
             :prepend t :unnarrowed t))
     ("p" . ("Playlist" entry (file+olp "ents.org" "Playlist")
             "* %?"
             :prepend t :unnarrowed t)))))

;; org-pdfview

(with-eval-after-load 'org
  (require 'org-pdfview))
(with-eval-after-load 'pdf-view
  (require 'org-pdfview))

;; org-pomodoro

(with-eval-after-load 'org-pomodoro
  (setq-default org-pomodoro-format
                (blc-sed-tree "pomodoro" "🍅" org-pomodoro-format nil t)))

;; outline

(with-eval-after-load 'outline
  (define-key outline-minor-mode-map "\C-c\t" #'outline-toggle-children))

;; ox-html

(with-eval-after-load 'ox-html
  (setq-default
   org-html-footnotes-section
   (blc-dom-to-xml 'div
                   '((id . footnotes))
                   (dom-node 'h3  '((class .      footnotes)) "%s")
                   (dom-node 'div '((id    . text-footnotes)) "%s")))

  (blc-put org-html-checkbox-types
           'html
           (let ((checkbox '((type     . checkbox)
                             (disabled . ""))))
             (map-apply
              (lambda (state checked)
                (cons state (blc-dom-to-xml 'input (append checkbox checked))))
              '((on    . ((checked . "")))
                (off   . ())
                (trans . ())))))

  (blc-put* org-html-postamble-format
            org-export-default-language
            (list (blc-dom-to-xml 'p '((class . modification)) "Updated: %C"))))

;; ox-publish

(with-eval-after-load 'ox-publish
  (setq-default
   org-publish-project-alist
   (mapcar (lambda (proj)
             (let ((pubdir (blc-dir (blc-user-dir "PUBLICSHARE") proj)))
               (list proj
                     :base-directory       (blc-dir org-directory proj)
                     :publishing-directory pubdir
                     :publishing-function  #'org-html-publish-to-html
                     :recursive            t
                     :with-author          nil
                     :with-toc             nil
                     :html-postamble       t
                     :html-home/up-format
                     (blc-dom-to-xml 'div
                                     '((id . org-div-home-and-up))
                                     (dom-node 'a
                                               '((accesskey . h)
                                                 (href      . "%s"))
                                               "&uarr;"))
                     :completion-function
                     (list
                      (lambda (&rest _)
                        (dolist (file (directory-files-recursively pubdir "" t))
                          (set-file-modes file (pcase file
                                                 ((pred file-regular-p)   #o640)
                                                 ((pred file-directory-p) #o750)
                                                 (_ (file-modes file))))))))))
           '("daft" "recipes"))))

;; paren

(show-paren-mode)

;; paren-face

(global-paren-face-mode)

;; pdf-tools

(add-to-list 'auto-mode-alist (cons (rx ".pdf" eos) #'pdf-tools-install))

;; perl-mode

(add-to-list 'auto-mode-alist (cons (rx ".latexmkrc" eos) #'perl-mode))

;; prolog

(add-to-list 'auto-mode-alist (cons (rx ".pl" eos) #'prolog-mode))

;; python

(with-eval-after-load 'python
  (map-do
   (lambda (var cmds)
     (when-let* ((cmd (seq-some #'executable-find cmds)))
       (set-default var cmd)))
   '((python-check-command     "epylint3" "epylint" "pyflakes")
     (python-shell-interpreter "ipython3" "python3" "ipython" "python"))))

;; reftex

(with-eval-after-load 'reftex
  (mapc (apply-partially #'add-to-list 'reftex-default-bibliography)
        (blc-bib-files)))

;; saveplace

(save-place-mode)

;; simple

(column-number-mode)

;; solar

(with-eval-after-load 'solar
  (when-let* ((loc (blc-system-location)))
    (blc-solar-set-location loc)))

;; subword

(global-subword-mode)

;; sx

(with-eval-after-load 'sx-question-list
  (define-key sx-question-list-mode-map
    [remap sx-question-list-hide] #'describe-mode))

;; term

(with-eval-after-load 'term
  (dolist (mapsym '(term-mode-map term-raw-map))
    (dolist (fn '(term-char-mode term-line-mode))
      (define-key (symbol-value mapsym)
        (vector 'remap fn) #'blc-toggle-subterm-mode))))

;; time

(display-time-mode)

;; url

(with-eval-after-load 'url-cookie
  (add-to-list 'url-cookie-untrusted-urls "economist\\.com"))

;; web-mode

(add-to-list 'auto-mode-alist (cons (rx ".mustache" eos) #'web-mode))

;; whitespace

(global-whitespace-mode)

;; winner

(winner-mode)

;; writeroom-mode

(with-eval-after-load 'writeroom-mode
  (blc-define-keys
    (writeroom-mode-map
     ([?\C-\M-<] . #'writeroom-decrease-width)
     ([?\C-\M->] . #'writeroom-increase-width)
     ([?\C-\M-=] . #'writeroom-adjust-width))))

;; xt-mouse

(blc-with-every-frame #'blc-turn-on-xterm-mouse)

;; youtube-dl

(with-eval-after-load 'youtube-dl
  (setq-default youtube-dl-directory (blc-user-dir "VIDEOS")))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; init.el ends here
