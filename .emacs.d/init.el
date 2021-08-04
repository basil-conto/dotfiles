;;; init.el --- init file for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://gitlab.com/basil-conto/dotfiles

;;; Code:

;;; Bootstrapping

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
(require 'xdg)
(eval-when-compile
  (require 'subr-x))

(autoload 'blc-backup                       "blc-backup" nil t)
(autoload 'blc-mbsync                       "blc-mbsync" nil t)
(autoload 'blc-mbsync-all                   "blc-mbsync" nil t)
(autoload 'blc-mbsync-deduplicate           "blc-mbsync" nil t)
(autoload 'blc-mbsync-maximise-uid          "blc-mbsync" nil t)
(autoload 'blc-pass-backend-parse           "blc-pass")
(autoload 'debbugs-gnu-apply-patch          "debbugs-gnu" nil t)
(autoload 'debbugs-gnu-find-contributor     "debbugs-gnu" nil t)
(autoload 'debbugs-gnu-make-control-message "debbugs-gnu" nil t)
(autoload 'engine-mode-prefixed-map         "engine-mode" nil t 'keymap)
(autoload 'ffap-gnus-hook                   "ffap")
(autoload 'fileloop-continue                "fileloop" nil t)
(autoload 'flex-mode                        "flex-mode" nil t)
(autoload 'samba-generic-mode               "generic-x" nil t)
(autoload 'gnus-find-subscribed-addresses   "gnus")
(autoload 'ivy-completion-in-region         "ivy")
(autoload 'mailcap-file-name-to-mime-type   "mailcap")
(autoload 'meme                             "meme" nil t)
(autoload 'meme-file                        "meme" nil t)
(autoload 'notifications-notify             "notifications")
(autoload 'pdf-view-mode                    "pdf-view" nil t)
(autoload 'TeX-doc                          "tex" nil t)
(autoload 'youtube-dl                       "youtube-dl" nil t)
(autoload 'youtube-dl-list                  "youtube-dl" nil t)
(autoload 'youtube-dl-playlist              "youtube-dl" nil t)

;;; Advice

;;;; battery

(defvar blc-battery-id nil
  "ID of last battery notification or nil.")

(defun blc-battery-notify (alist &optional id)
  "Send a low battery notification optionally replacing ID.
ALIST is like that returned by `battery-status-function'."
  (notifications-notify :title "Low Battery"
                        :body (format "%s%% (%s mins) remaining"
                                      (alist-get ?p alist)
                                      (alist-get ?m alist))
                        :replaces-id id
                        :urgency 'critical
                        :image-path "battery-caution"))

(define-advice battery-upower (:filter-return (alist) blc-notify-critical)
  "Send a notification if battery load percentage is critical.
Also transcribe AC line status in ALIST to Unicode."
  (let* ((load (read (alist-get ?p alist)))
         (line (assq ?L alist))
         (ac   (string-equal (cdr line) "on-line")))
    (setcdr line (if ac "🔌" "🔋"))
    (cond ((and (not ac) (numberp load) (<= load battery-load-critical))
           (let ((new (blc-battery-notify alist blc-battery-id)))
             (or blc-battery-id (setq blc-battery-id new))))
          (blc-battery-id
           (notifications-close-notification
            (prog1 blc-battery-id (setq blc-battery-id nil))))))
  alist)

;;;; browse-url

(define-advice browse-url-firefox (:around (fn &rest args) blc-no-wait)
  "Detach from spawned subprocess."
  (blc-with-nonce start-process :override
                  (lambda (_name _buf prog &rest args)
                    (apply #'call-process prog nil 0 nil args))
    (apply fn args)))

;;;; eldoc

(define-advice eldoc--format-doc-buffer (:filter-return (buf) blc-nbsp)
  "Ignore non-ASCII space and hyphen chars in ElDoc buffer."
  (with-current-buffer buf
    (setq-local nobreak-char-display nil)
    (blc-turn-off-trailing-whitespace))
  buf)

;;;; elisp-mode

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

;;;; find-func

(define-advice find-function-search-for-symbol
    (:around (search sym type lib) blc-dataroot-to-src)
  "Pass LIB through `blc-dataroot-to-src'."
  (funcall search sym type (blc-dataroot-to-src lib)))

;;;; gnus-sum

(define-advice gnus-summary-exit (:after (&rest _) blc-gnus-single-group-frame)
  "Allow only the selected frame to display `gnus-group-buffer'."
  (dolist (win (cdr (get-buffer-window-list gnus-group-buffer nil 'visible)))
    (when (frame-root-window-p win)
      (blc-delete-spare-frame (window-frame win)))))

;;;; gnus-win

(define-advice gnus-configure-frame (:before (&rest _) blc-no-gnus-frames)
  "Empty lists of frames kept by Gnus.
Emptying `gnus-frame-list' undedicates Gnus frames from their
initial purpose, so their window configuration, if since changed,
is left alone.
Emptying `gnus-created-frames' protects against deletion of the
last visible Emacs client frame."
  (setq gnus-frame-list     ())
  (setq gnus-created-frames ()))

;;;; help

(define-advice view-echo-area-messages (:filter-return (win) blc-select-window)
  "Pop to `messages-buffer'."
  (when-let (frame (and win (window-frame win)))
    (select-frame-set-input-focus frame)
    (select-window win))
  win)

;;;; help-fns

(define-advice help-fns-short-filename (:around (abbr file) blc-src-load-path)
  "Dynamically bind `load-path' with `blc-src-path'."
  (let ((load-path (blc-src-path)))
    (funcall abbr file)))

(advice-add #'find-lisp-object-file-name :filter-return #'blc-dataroot-to-src)

;;;; ivy-bibtex

(define-advice bibtex-completion-format-entry
    (:around (fmt entry width) blc-narrow)
  "Decrease `ivy-bibtex' entry width due to other formatting.
`ivy-bibtex-default-action' only considers `frame-width', which
for example excludes the effect of `ivy-format-functions-alist'."
  (let ((str (funcall (ivy-alist-setting ivy-format-functions-alist) '(""))))
    (funcall fmt entry (apply #'- width (string-width str)
                              (mapcar #'fringe-columns '(left right))))))

;;;; magit-diff

(define-advice magit-diff-show-or-scroll
    (:around (&rest args) blc-visible-frames)
  "Show and scroll Magit diff buffer across frames."
  (blc-with-nonce get-buffer-window :around
                  (lambda (get &optional buf _frames)
                    (funcall get buf 'visible))
    (apply args)))

;;;; magit-extras

(define-advice magit-pop-revision-stack
    (:around (&rest args) blc-message-narrow-to-body)
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
      (apply args))
    (save-excursion
      (and mail
           (message-goto-signature)
           (re-search-backward message-signature-separator)
           (or (= (line-beginning-position 0) (line-end-position 0))
               (insert ?\n))))))

;;;; magit-log

(define-advice magit-log-maybe-update-revision-buffer
    (:around (&rest args) blc-all-frames)
  "Update Magit log buffer across frames."
  (blc-with-nonce magit-get-mode-buffer :filter-args #'butlast
    (apply args)))

;;;; magit-mode

(define-advice magit-display-buffer-same-window-except-diff-v1
    (:around (&rest args) blc-visible-frames)
  "Display Magit diff buffers across frames."
  (blc-with-nonce display-buffer :around
                  (lambda (display buf &optional action _frame)
                    (funcall display buf action 'visible))
    (apply args)))

;;;; magit-remote

(define-advice magit-clone-internal
    (:around (clone repo dir &rest args) blc-git-clone-subdir)
  "Clone into subdirectory of DIR if non-empty."
  (and (file-directory-p dir)
       (directory-files dir nil directory-files-no-dot-files-regexp t 1)
       (let* ((re  (rx (group (+? (not (in ?/ ?:)))) (? ".git") eos))
              (sub (blc-dir dir (and (string-match re repo)
                                     (match-string 1 repo)))))
         (or (file-directory-p sub)
             (setq dir sub))))
  (apply clone repo dir args))

;;;; mail-extr

(define-advice mail-extract-address-components
    (:before-until (address &optional all) blc-delegate-gnus)
  "Try to cut corners with `gnus-extract-address-components'.
This is much less accurate but also much more performant than
`mail-extract-address-components'."
  (and (not all)
       (stringp address)
       (gnus-extract-address-components address)))

;;;; make-mode

(defun blc-delete-hspace-backward--advice (&rest _)
  "Delete horizontal whitespace before point."
  (delete-horizontal-space t))

(dolist (fn (list #'makefile-insert-gmake-function
                  #'makefile-insert-target-ref))
  (advice-add fn :after #'blc-delete-hspace-backward--advice))

;;;; mpc

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

;;;; newst-treeview

(define-advice newsticker--treeview-frame-init
    (:around (&rest args) blc-anonymous-frame)
  "Create frame sans `name' parameter."
  (blc-with-nonce make-frame :filter-args #'ignore
    (apply args)))

;;;; octave

(advice-add #'octave-maybe-mode :override #'octave-mode)

;;;; org

(define-advice org-read-date (:around (&rest args) blc-avoid-frames)
  "Temporarily disable `pop-up-frames'."
  (let (pop-up-frames)
    (apply args)))

;;;; org-agenda

(defvar org-agenda-window-setup)

(define-advice org-agenda--quit (:around (&rest args) blc-spare-frame)
  "Do not delete Org Agenda frame on exit."
  (let (org-agenda-window-setup)
    (apply args)))

;;;; org-capture

(define-advice org-capture-refile (:after (&rest _) blc-org-save)
  "Save target buffer of `org-capture-refile'."
  (save-window-excursion
    (org-capture-goto-last-stored)
    (save-buffer)))

;;;; org-pcomplete

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

;;;; project

(defvar-local blc-project 'unset
  "Per-buffer cached `project-current' or `unset'.")

(defvar blc--project-death-row ()
  "List of buffers `project-kill-buffers' is about to kill.")

(defun blc--project-death-row ()
  "Print `blc--project-death-row' to `standard-output'."
  (dolist (buf (prog1 blc--project-death-row
                 (setq blc--project-death-row ())))
    (princ buf)
    (terpri)))

(define-advice project--buffers-to-kill (:filter-return (bufs) blc-store)
  "Store buffers about to be killed."
  (setq blc--project-death-row bufs))

(define-advice project-kill-buffers (:around (&rest args) blc-list)
  "List buffers `project-kill-buffers' is about to kill."
  (let* ((buf (get-buffer-create " *Project Buffers*" t))
         ;; See `dired-mark-pop-up'.
         (win (display-buffer buf `(display-buffer-below-selected
                                    (window-height . fit-window-to-buffer)
                                    (preserve-size . (nil . t)))))
         (standard-output buf))
    (blc-with-nonce yes-or-no-p :around
                    (lambda (&rest args)
                      (unwind-protect
                          (progn (blc--project-death-row)
                                 (fit-window-to-buffer win)
                                 (apply args))
                        (when (window-live-p win)
                          (quit-restore-window win 'kill))))
      (apply args))))

;;;; python

(define-advice python-shell-make-comint (:around (&rest args) blc-dumb-term)
  "Start inferior Python with TERM=dumb."
  (defvar comint-terminfo-terminal)
  (let ((comint-terminfo-terminal "dumb"))
    (apply args)))

;;;; whitespace

(defun blc-whitespace-enable--advice ()
  "Advice intended for predicate `whitespace-enable-predicate'."
  (not (equal (buffer-name) "*scratch*")))

(with-eval-after-load 'whitespace
  (add-function :after-while whitespace-enable-predicate
                #'blc-whitespace-enable--advice))

;;; Definitions

;;;; fontset.c

(defun blc-turn-on-emoji-font (&optional _frame)
  "Add an emoji font to the default fontset.
Do this only once in the first non-daemon initial frame."
  (when (fboundp 'set-fontset-font)
    (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append))
  (dolist (hook '(after-make-frame-functions window-setup-hook))
    (remove-hook hook #'blc-turn-on-emoji-font)))

(add-hook (if (daemonp) 'after-make-frame-functions 'window-setup-hook)
          #'blc-turn-on-emoji-font)

;;;; auctex

(defun blc-TeX-command-default ()
  "Set `TeX-command-default' based on `TeX-command-list'."
  (setq TeX-command-default (caar TeX-command-list)))

(defun blc-TeX-doc (arg)
  "Prompt for and display TeX documentation.
Like `TeX-doc', but with prefix ARG pass it to
`TeX-documentation-texdoc' instead."
  (interactive "P")
  (if arg
      (TeX-documentation-texdoc arg)
    (call-interactively #'TeX-doc)))

;;;; autorevert

(defun blc-turn-on-silent-auto-revert ()
  "Locally enable `auto-revert-mode' without revert messages."
  (auto-revert-mode)
  (setq-local auto-revert-verbose nil))

;;;; bbdb

(defun blc-kill-bbdb-buffer ()
  "Kill `bbdb-buffer'."
  (when (bound-and-true-p bbdb-buffer)
    (kill-buffer bbdb-buffer)))

;;;; bog

(defun blc-bog-dired-stage ()
  "Visit `bog-stage-directory' in Dired."
  (interactive)
  (require 'bog)
  (dired bog-stage-directory))

(defun blc-bog-staged-bib ()
  "Like `bog-clean-and-rename-staged-bibs' but with completion."
  (interactive)
  (require 'bog)
  (let* ((bibs (directory-files bog-stage-directory nil (rx ".bib" eos)))
         (bib  (completing-read "Staged bib: " bibs nil t)))
    (bog--prepare-bib-file (expand-file-name bib bog-stage-directory) t)))

;;;; browse-url

(defun blc-kill-url (url &rest _)
  "Make URL the latest kill in the kill ring."
  (kill-new url))

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
    ("Kill"               . ,#'blc-kill-url          )
    ("Print"              . ,#'blc-print-url         )
    ("Print to PDF"       . ,#'blc-print-url-pdf     )
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
  (let* ((prompt  (if (string-blank-p url)
                      "Open browser: "
                    (blc--url-fmt "Open URL `%s' in: " url)))
         (browser (blc-get blc-browser-alist
                           (completing-read prompt blc-browser-alist nil t))))
    (when browser (apply browser url args))))

(function-put
 #'blc-browse-url 'interactive-form (interactive-form #'browse-url))

;;;; compile

(defvar blc-compile-duration 30
  "Seconds of compilation after which to notify of end.")

(defvar blc-compile-buffers (make-hash-table :test #'eq :size 4 :weakness 'key)
  "Map buffers to compilation start times and notification IDs.")

(defun blc-compile-start (proc)
  "Clock in compilation PROC.
Intended for `compilation-start-hook'."
  (let* ((buf (process-buffer proc))
         (val (gethash buf blc-compile-buffers)))
    (when-let (id (plist-get val :id))
      (notifications-close-notification id))
    (setq val (plist-put val :start (current-time)))
    (puthash buf val blc-compile-buffers)))

(defun blc-compile-end (buf msg)
  "Clock out compilation BUF and notify with MSG.
Intended for `compilation-finish-functions'."
  (let* ((val (gethash buf blc-compile-buffers))
         (beg (plist-get val :start))
         (id  (plist-get val :id)))
    (cond ((get-buffer-window buf 'visible)
           (when id (notifications-close-notification id)))
          ((< (float-time (time-subtract nil beg)) blc-compile-duration))
          ((plist-put val :id (notifications-notify
                               :title (string-trim (format "%s %s" buf msg))
                               :actions '("default" "Pop to buffer")
                               :on-action (lambda (_ _) (pop-to-buffer buf))
                               :replaces-id id))))))

;;;; counsel

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

;;;; eww

(defun blc-eww-bookmark-save ()
  "Copy the URL of the current bookmark into the kill ring."
  (interactive)
  (defvar eww-data)
  (let ((eww-data
         (or (get-text-property (line-beginning-position) 'eww-bookmark)
             (user-error "No bookmark on the current line"))))
    (eww-copy-page-url)))

(defun blc-eww-toggle-images ()
  "Toggle display of images in current `eww' buffer.
Adapted from URL
`https://emacs.stackexchange.com/a/38639/15748'."
  (interactive)
  (setq-local shr-inhibit-images (not shr-inhibit-images))
  (eww-reload t))

(defun blc-eww-open-in-other-window ()
  "Call `eww-open-in-new-buffer' in another window."
  (interactive)
  (let ((display-buffer-overriding-action blc-other-window-action))
    (call-interactively #'eww-open-in-new-buffer)))

;;;; files

(defun blc-confirm-kill-daemon (prompt)
  "Ask whether to kill daemon Emacs with PROMPT.
Intended as a predicate for `confirm-kill-emacs'."
  (or (not (daemonp))
      (yes-or-no-p prompt)))

;;;; flycheck

(defun blc-turn-off-flycheck (&rest _)
  "Disable `flycheck-mode'."
  (interactive)
  (when (bound-and-true-p flycheck-mode)
    (blc-turn-off #'flycheck-mode)))

;;;; ghub

(defconst blc-github-token-scopes '(notifications)
  "List of `ghub' scopes for `blc'.")

(defun blc-github-notifications ()
  "Asynchronously notify of any GitHub notifications."
  (interactive)
  (require 'ghub)
  (ghub-get
   "/notifications" ()
   :auth 'blc
   :callback
   (lambda (nots &rest _)
     (notifications-notify
      :title "GitHub Notifications"
      :body (let ((repos '(("other" . 0))))
              (dolist (not nots)
                (let ((name (map-nested-elt not '(repository name) "other")))
                  (blc-put* repos name (1+ (blc-get repos name 0)))))
              (let ((fmt (format
                          "%%-%ds %%%dd"
                          (apply #'max (map-keys-apply #'string-width repos))
                          (1+ (floor (log (apply #'max 1 (map-values repos))
                                          10))))))
                ;; Include a URL that can be opened by Dunst.
                (concat (blc-dom-to-xml
                         'a '((href . "https://github.com/notifications")))
                        (mapconcat (pcase-lambda (`(,name . ,count))
                                     (format fmt name count))
                                   repos "\n"))))))
   :errorback (lambda (err &rest _)
                (signal 'error (list "GitHub error: %S" (cdr err))))))

;;;; git-commit

(defun blc-git-commit-set-fill-column ()
  "Set local `fill-column' for `git-commit-mode' buffers."
  ;; Benefit over setq: displays debugging message
  (set-fill-column 68))

;;;; gnus

(defun blc--gnus-switch-buffer (action)
  "Call ACTION on first desirable Gnus buffer found.
Return result of ACTION.  See `blc-gnus' for a definition of
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

;;;; go-mode

(defun blc-go-whitespace-style ()
  "Adapt `whitespace-style' to Go's indentation style."
  (setq-local whitespace-style (append (remq 'tab-mark whitespace-style)
                                       '(space-before-tab))))

;;;; ibuffer

(defun blc-ibuffer-filter-groups ()
  "Set custom `ibuffer-filter-groups'."
  (require 'ibuf-ext)
  (setq ibuffer-filter-groups (blc-get ibuffer-saved-filter-groups "blc")))

(defun blc-ibuffer-ffap ()
  "Like `ibuffer-find-file', but backed by `ffap-file-finder'."
  (interactive)
  (require 'ffap)
  (let* ((buf (ibuffer-current-buffer))
         (buf (if (buffer-live-p buf) buf (current-buffer)))
         (default-directory (buffer-local-value 'default-directory buf)))
    (call-interactively ffap-file-finder)))

;;;; ielm

(defun blc-ielm-other-window ()
  "Call `ielm' in another window."
  (interactive)
  (let ((display-buffer-overriding-action blc-other-window-action))
    (call-interactively #'ielm)))

;;;; info

(defun blc-info-read-buffer ()
  "Read the name, file and node of an Info buffer.
Return the name of the buffer as a string or `nil'."
  (if-let* ((bufs (blc-keep
                   (lambda (buf)
                     (and-let* ((name (buffer-name buf))
                                ((/= (aref name 0) ?\s))
                                (id   (buffer-local-value
                                       'mode-line-buffer-identification buf)))
                       (setq id (substring-no-properties (cadr id)))
                       (setq id (string-replace "%%" "%" id))
                       (cons (concat name id) name)))
                   (blc-derived-buffers #'Info-mode)))
            ((cdr bufs)))
      (blc-get bufs (completing-read
                     "Info buffer: " (seq-sort-by #'car #'string-lessp bufs)))
    (cdar bufs)))

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

;;;; isearch

(defun blc-isearch-delight ()
  "Shorten lighter of `isearch-mode'."
  (setq isearch-mode "🔍"))

;;;; ivy

(defun blc-ivy-string< (_name cands)
  "Sort CANDS in lexicographic order.
Intended as a value for `ivy-sort-matches-functions-alist'."
  (sort (copy-sequence cands) #'string-lessp))

(defun blc-ivy-strip-init-inputs (regexp)
  "Undo some default settings for Ivy initial inputs.
Delete from `ivy-initial-inputs-alist' those entries whose key
name matches REGEXP."
  (setq-default
   ivy-initial-inputs-alist
   (map-remove (lambda (cmd _)
                 (string-match-p regexp (symbol-name cmd)))
               ivy-initial-inputs-alist)))

;;;; magit-autorevert

(defun blc-magit-auto-revert-p (buf)
  "Return non-nil if BUF should be auto-reverted by Magit.
Intended for `auto-revert-buffer-list-filter'."
  (not (provided-mode-derived-p (buffer-local-value 'major-mode buf)
                                #'pdf-view-mode)))

;;;; man

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

;;;; message

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
        (not (re-search-forward
              (rx (eval `(| "attach"
                            ,@(mapcar (lambda (s)
                                        `(regexp ,(char-fold-to-regexp s)))
                                      '("συναπτ" "συναψ" "συνημμ")))))
              (save-excursion (message-goto-signature) (point))
              t)))
      (map-some (lambda (_part params)
                  (assq 'disposition params))
                (mml-parse))
      (y-or-n-p "No attachments; send anyway? ")
      (keyboard-quit)))

;;;; org

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

;;;; pdf-tools

(defun blc-pdf-tools-defer ()
  "Install `pdf-tools' before turning on `pdf-view-mode'.
Uninstall self from `auto-mode-alist' and `magic-mode-alist', as
`pdf-tools-install' will install itself there.  Intended as a
deferred way of autoloading the `pdf-tools' package."
  (unless (with-demoted-errors "Error activating PDF Tools: %S"
            (pdf-tools-install nil t)
            t)
    (doc-view-mode-maybe))
  (dolist (sym '(auto-mode-alist magic-mode-alist))
    (set sym (rassq-delete-all #'blc-pdf-tools-defer (symbol-value sym)))))

;;;; project

(defun blc-project-complete-regexp ()
  "Like `project-find-regexp', but using Xref completion."
  (interactive)
  (defvar xref-show-xrefs-function)
  (let ((xref-show-xrefs-function #'xref-show-definitions-completing-read))
    (call-interactively #'project-find-regexp)))

;;;; python

(defun blc-python-pep-8-comments ()
  "Adapt `comment-inline-offset' to PEP-8 recommendations."
  (setq-local comment-inline-offset 2))

;;;; redtick

(defun blc-redtick-notify-break ()
  "Notify of start of break time."
  (notifications-notify :title "Break" :body "🏖"))

(defun blc-redtick-notify-work ()
  "Notify of end of break time."
  (notifications-notify :title "Work" :body "💼"))

(defun blc-stop-redtick ()
  "Stop `redtick' when disabling `redtick-mode'."
  (unless redtick-mode
    (cancel-timer redtick--timer)
    (redtick--stop-sound)))

;;;; simple

(defun blc-messages-trailing-whitespace ()
  "Disable `show-trailing-whitespace' in *Messages* buffer."
  (with-current-buffer (messages-buffer)
    (blc-turn-off-trailing-whitespace)))

;;;; solar

(defun blc-solar-set-location (&optional location)
  "Reconcile solar calendar with LOCATION from `blc-locations'."
  (interactive (list (completing-read "Location: " blc-locations nil t nil ()
                                      (blc-system-location))))
  (pcase-let (((map :country :lat :long)
               (blc-get blc-locations location)))
    (setq-default calendar-latitude      lat
                  calendar-longitude     long
                  calendar-location-name (format "%s, %s" location country))))

;;;; startup

(defun blc-scratch-cowtune ()
  "Return string contents of cowtune file or nil if non-existent.
Format contents as an `emacs-lisp-mode' comment suitable for
`initial-scratch-message'."
  (blc-with-contents (or (getenv "COWTUNE_FILE") "~/.cowtune")
    (blc-fortune-filter)
    (let ((comment-start ";;")
          (comment-empty-lines t)
          delete-trailing-lines)
      (comment-region (point-min) (point-max))
      (delete-trailing-whitespace (point-min) (point-max)))
    (buffer-string)))

;;;; term

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
    (let ((switch-to-buffer-obey-display-actions t))
      (funcall (if non-ansi #'term #'ansi-term)
               (or (bound-and-true-p explicit-shell-file-name)
                   (getenv "ESHELL")
                   shell-file-name)))))

(defun blc-term-p (name &optional _action)
  "Determine whether NAME names a `term-mode' buffer.
Intended as a condition for `display-buffer-alist'."
  (with-current-buffer name
    (derived-mode-p #'term-mode)))

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
                  (nom (buffer-name buf)))
         (with-current-buffer buf
           (unless (equal dir default-directory)
             (setq dir default-directory)
             (let ((base (and (string-match (rx bos ?* (+ word)) nom)
                              (substring nom 1 (match-end 0))))
                   (abbr (abbreviate-file-name (directory-file-name dir))))
               (rename-buffer (format "*%s %s*" base abbr) t)))))))))

;;;; webjump

(defun blc-webjump-browse-url (_name)
  "Wrap `browse-url-interactive-arg' for use in `webjump-sites'."
  (car (browse-url-interactive-arg "URL: ")))

;;;; xref-js2

(defun blc-xref-js2-install-backend ()
  "Locally install `xref-js2-xref-backend'."
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))

;;;; xt-mouse

(defun blc-turn-on-xterm-mouse (&optional frame)
  "Conditionally enable `xterm-mouse-mode' on FRAME.
Enable the mode only if FRAME is the first terminal frame
created.  FRAME defaults to the selected one."
  (or (display-graphic-p frame)
      (string-equal "initial_terminal" (terminal-name (frame-terminal frame)))
      xterm-mouse-mode
      (xterm-mouse-mode)))

;;; Variables

(setq-default
 ;; buffer.c
 fill-column                            blc-chars-per-line
 indicate-buffer-boundaries             t
 indicate-empty-lines                   t
 mode-line-format                       (blc-sed-tree " +" " " mode-line-format)

 ;; callint.c
 mark-even-if-inactive                  nil

 ;; dired.c
 completion-ignored-extensions          `(".fdb_latexmk" ".fls" ".xdv"
                                          ,@completion-ignored-extensions)

 ;; doc.c
 text-quoting-style                     'grave

 ;; fns.c
 use-short-answers                      t

 ;; frame.c
 frame-resize-pixelwise                 t

 ;; indent.c
 indent-tabs-mode                       nil

 ;; keyboard.c
 echo-keystrokes                        0.3

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
 redisplay-skip-fontification-on-input  t
 scroll-conservatively                  most-positive-fixnum
 scroll-margin                          1
 scroll-step                            1
 show-trailing-whitespace               t

 ;; xfns.c
 x-gtk-use-system-tooltips              nil

 ;; xterm.c
 x-wait-for-event-timeout               0.2

 ;; ag
 ag-highlight-search                    t

 ;; apt-utils
 apt-utils-show-all-versions            t

 ;; asm-mode
 asm-comment-char                       ?#

 ;; auth-source
 auth-source-cache-expiry               (blc-mins-to-secs 15)
 auth-source-debug                      'trivia
 auth-sources                           '(blc-pass)

 ;; autorevert
 auto-revert-avoid-polling              t

 ;; avy
 avy-all-windows                        'all-frames
 avy-background                         t

 ;; battery
 battery-load-low                       20
 battery-mode-line-format               "%L"

 ;; bbdb
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
 bibtex-comment-start                   "%"
 bibtex-entry-kill-ring-max             kill-ring-max
 bibtex-field-kill-ring-max             kill-ring-max
 bibtex-maintain-sorted-entries         t
 bibtex-search-entry-globally           t

 ;; bindings
 mode-line-percent-position             '(-3 "%o")

 ;; bog
 bog-citekey-file-name-separators       (rx ?.)
 bog-keymap-prefix                      "\C-cb"
 bog-root-directory                     (blc-parent-dir blc-bib-dir)

 ;; bookmark
 bookmark-default-file                  (blc-file blc-index-dir "bookmarks.el")
 bookmark-fontify                       nil
 bookmark-save-flag                     1
 bookmark-search-delay                  0

 ;; browse-url
 browse-url-browser-function            #'blc-browse-url
 browse-url-firefox-new-window-is-tab   t

 ;; calendar
 calendar-christian-all-holidays-flag   t
 calendar-date-style                    'iso
 calendar-islamic-all-holidays-flag     t
 calendar-time-zone-style               'numeric

 ;; cc-vars
 c-electric-pound-behavior              '(alignleft)

 ;; chess
 chess-images-default-size              blc-chars-per-line

 ;; comint
 comint-input-ignoredups                t
 comint-terminfo-terminal               "dumb-emacs-ansi"

 ;; comp
 native-comp-async-report-warnings-errors
 'silent

 ;; compile
 compilation-message-face               'default
 compilation-scroll-output              'first-error
 compile-command                        "make"

 ;; copyright
 copyright-names-regexp                 (regexp-quote user-full-name)

 ;; counsel
 counsel-describe-function-preselect    #'ivy-function-called-at-point
 counsel-git-grep-cmd-function
 #'counsel-git-grep-cmd-function-ignore-order
 counsel-grep-base-command              (string-join '("rg"
                                                       "--color" "never"
                                                       "--line-number"
                                                       "--no-heading"
                                                       "--smart-case"
                                                       "--with-filename"
                                                       "--regexp" "%s"
                                                       "%s")
                                                     " ")
 counsel-mode-map                       () ; Control remaps
 counsel-org-goto-display-tags          t
 counsel-org-goto-display-todo          t
 counsel-org-goto-face-style            'verbatim
 counsel-yank-pop-filter                #'identity

 ;; csv-mode
 csv-align-style                        'auto
 csv-invisibility-default               nil

 ;; cus-edit
 custom-unlispify-menu-entries          nil
 custom-unlispify-tag-names             nil

 ;; custom
 custom-theme-directory                 (blc-dir user-emacs-directory "lisp")

 ;; debbugs
 debbugs-gnu-branch-directory
 (blc-dir (blc-parent-dir source-directory) "emacs27")
 debbugs-gnu-emacs-current-release      "27.1"
 debbugs-gnu-send-mail-function         #'message-send-mail-with-sendmail
 debbugs-gnu-suppress-closed            nil
 debbugs-gnu-trunk-directory            source-directory

 ;; diary-lib
 diary-comment-start                    ";"
 diary-number-of-entries                3

 ;; dictionary
 dictionary-create-buttons              nil
 dictionary-default-strategy            "re"

 ;; diff-mode
 diff-font-lock-prettify                t

 ;; dired
 dired-auto-revert-buffer               t
 dired-dwim-target                      #'dired-dwim-target-next-visible
 dired-listing-switches                 (string-join
                                         '("--almost-all"
                                           "--classify"
                                           "--group-directories-first"
                                           "--human-readable"
                                           "-lv")
                                         " ")
 dired-maybe-use-globstar               t
 dired-recursive-copies                 'always

 ;; dired-aux
 dired-create-destination-dirs          'ask
 dired-do-revert-buffer                 t

 ;; doc-view
 doc-view-resolution                    150

 ;; dropbox
 dropbox-locale                         "en_IE"
 dropbox-verbose                        t

 ;; eldoc
 eldoc-echo-area-prefer-doc-buffer      t
 eldoc-echo-area-use-multiline-p        t

 ;; emacsbug
 report-emacs-bug-no-explanations       t

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
 ffap-rfc-directories
 (list (blc-dir (blc-user-dir "DOCUMENTS") "rfc"))

 ;; files
 auto-save-visited-interval             auto-save-timeout
 backup-by-copying                      t
 backup-directory-alist
 `(("" . ,(blc-dir (xdg-cache-home) "emacs")))
 confirm-kill-emacs                     #'blc-confirm-kill-daemon
 delete-old-versions                    t
 directory-free-space-args              "-hP"
 find-file-suppress-same-file-warnings  t
 find-file-visit-truename               t
 kept-new-versions                      4
 kept-old-versions                      2
 mode-require-final-newline             nil
 version-control                        t
 view-read-only                         t

 ;; flymake
 flymake-suppress-zero-counters         t

 ;; footnote
 footnote-body-tag-spacing              1
 footnote-mode-line-string              ""
 footnote-section-tag                   ""
 footnote-spaced-footnotes              nil

 ;; forge
 forge-database-file                    (blc-file blc-index-dir
                                                  "forge-db.sqlite")
 forge-topic-list-limit                 '(60 . -1)

 ;; frame
 window-divider-default-right-width     2

 ;; generic-x
 generic-use-find-file-hook             nil

 ;; git-annex
 git-annex-commit                       nil

 ;; git-commit
 git-commit-summary-max-length          50

 ;; gnus
 gnus-home-directory                    user-emacs-directory
 gnus-init-file                         (expand-file-name
                                         "gnus" gnus-home-directory)

 ;; gnus-desktop-notify
 gnus-desktop-notify-groups             'gnus-desktop-notify-explicit
 gnus-desktop-notify-format             "%3n: %G"
 gnus-desktop-notify-function           #'gnus-desktop-notify-send

 ;; go-mode
 godoc-reuse-buffer                     t

 ;; hacker-typer
 hacker-typer-show-hackerman            t

 ;; hackernews
 hackernews-visited-links-file          (blc-file blc-index-dir
                                                  "hackernews-visited.el")

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
 describe-bindings-outline              t
 help-window-select                     'other

 ;; holidays
 holiday-bahai-holidays                 ()
 holiday-oriental-holidays              ()

 ;; htmlize
 htmlize-css-name-prefix                "htmlize-"
 htmlize-html-charset                   "utf-8"
 htmlize-html-major-mode                #'mhtml-mode

 ;; ibuf-ext
 ibuffer-old-time                       24
 ibuffer-save-with-custom               nil
 ibuffer-show-empty-filter-groups       nil

 ;; ibuffer
 ibuffer-always-compile-formats         t
 ibuffer-default-sorting-mode           'alphabetic
 ibuffer-jump-offer-only-visible-buffers
 t

 ;; ido
 ido-enable-flex-matching               t

 ;; ielm
 ielm-noisy                             nil
 ielm-prompt                            "(>) "

 ;; image-dired
 image-dired-dir                        (blc-dir blc-index-dir "image-dired")

 ;; isearch
 isearch-allow-scroll                   t
 isearch-lazy-count                     t
 search-exit-option                     nil

 ;; ivy
 ivy-action-wrap                        t
 ivy-count-format                       "(%d/%d) "
 ivy-extra-directories                  ()
 ivy-on-del-error-function              #'ignore
 ivy-pulse-delay                        nil

 ;; ivy-bibtex
 bibtex-completion-display-formats
 '((t . "\
${author:30} ${date:4} ${title:*} ${=has-pdf=:1}${=has-note=:1} ${=type=:14}"))

 ;; jit-lock
 jit-lock-stealth-time                  4

 ;; js-2
 js2-allow-rhino-new-expr-initializer   nil
 js2-bounce-indent-p                    t
 js2-global-externs                     '("define" "location")
 js2-highlight-level                    3
 js2-include-node-externs               t
 js2-mode-assume-strict                 t
 js2-skip-preprocessor-directives       t

 ;; keycast
 keycast-insert-after                   'mode-line-misc-info
 keycast-remove-tail-elements           nil
 keycast-separator-width                0

 ;; ledger-mode
 ledger-post-amount-alignment-at        :decimal
 ledger-report-auto-refresh-sticky-cursor
 t
 ledger-report-use-header-line          t
 ledger-use-iso-dates                   t

 ;; logview
 logview-cache-filename                (blc-file blc-index-dir
                                                 "logview-cache.extmap")
 logview-views-file                    (blc-file blc-index-dir "logview.views")

 ;; lpr
 lpr-add-switches                       nil
 lpr-command                            "hp-print"

 ;; magit-autorevert
 auto-revert-buffer-list-filter         #'blc-magit-auto-revert-p

 ;; magit-branch
 magit-branch-popup-show-variables      t

 ;; magit-diff
 magit-diff-adjust-tab-width            t
 magit-diff-refine-hunk                 t

 ;; magit-extras
 magit-bind-magit-project-status        nil

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
 markdown-gfm-use-electric-backquote    nil
 markdown-header-scaling                t
 markdown-spaces-after-code-fence       0

 ;; message
 message-cite-function                  #'message-cite-original
 message-confirm-send                   t
 message-expand-name-standard-ui        t
 message-forward-before-signature       nil
 message-make-forward-subject-function  #'message-forward-subject-fwd
 message-mail-alias-type                nil
 message-sendmail-envelope-from         'header
 message-signature                      (car (split-string user-full-name))

 ;; mines
 mines-empty-cell-char                  ?\s
 mines-flagged-cell-char                ?⚑

 ;; minibuffer
 completing-read-function               #'ivy-completing-read
 completion-in-region-function          #'ivy-completion-in-region
 completions-detailed                   t
 completions-format                     'one-column
 completions-group                      t

 ;; mm-decode
 mm-decrypt-option                      'ask
 mm-default-directory                   (blc-user-dir "DOWNLOAD")
 mm-external-terminal-program           "x-terminal-emulator"
 mm-sign-option                         'guided
 mm-verify-option                       'always

 ;; mml-sec
 mml-secure-verbose                     t

 ;; mpc
 mpc-frame-alist                        '((tool-bar-lines . 1))

 ;; mule-cmds
 default-input-method                   "greek"
 read-char-by-name-sort                 'code

 ;; mwheel
 mouse-wheel-tilt-scroll                t
 mouse-wheel-flip-direction             t

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
    "http://rss.nytimes.com/services/xml/rss/nyt/World.xml")
   ("NYT Tech"
    "http://rss.nytimes.com/services/xml/rss/nyt/Technology.xml")
   ("NYT Sci"
    "http://rss.nytimes.com/services/xml/rss/nyt/Science.xml")
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

 ;; nov
 nov-save-place-file                    (blc-file blc-index-dir "nov-places.el")
 nov-text-width                         blc-chars-per-line

 ;; novice
 disabled-command-function              nil

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
 org-goto-interface                     'outline-path-completion
 org-goto-max-level                     10
 org-hierarchical-todo-statistics       nil
 org-highlight-latex-and-related        '(entities latex script)
 org-log-done                           'note
 org-log-into-drawer                    t
 org-log-redeadline                     'note
 org-log-reschedule                     'note
 org-modules                            ()
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
 org-agenda-align-tags-to-column        (- blc-chars-per-line)
 org-agenda-category-icon-alist         '(("" (space :width (16))))
 org-agenda-deadline-leaders            '("" "%3dd +" "%3dd -")
 org-agenda-scheduled-leaders           '("" "%3dd -")
 org-agenda-sticky                      t
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
 org-list-allow-alphabetical            t
 org-list-demote-modify-bullet          '(("+" . "-") ("-" . "+"))
 org-list-use-circular-motion           t

 ;; outline
 outline-minor-mode-cycle               t

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

 ;; password-cache
 password-cache                         nil

 ;; pdf-misc
 pdf-misc-print-programm                lpr-command

 ;; pdf-view
 pdf-view-display-size                  'fit-page

 ;; proced
 proced-auto-update-flag                t

 ;; project
 project-compilation-buffer-name-function
 #'project-prefixed-buffer-name
 project-kill-buffer-conditions
 `(buffer-file-name
   (and ,(rx bos (not ?\s))
        (major-mode . fundamental-mode))
   (derived-mode . compilation-mode)
   (derived-mode . diff-mode)
   (derived-mode . dired-mode)
   (derived-mode . magit-section-mode))
 project-list-file                      (blc-file blc-index-dir "projects")
 project-switch-use-entire-map          t

 ;; prolog
 prolog-system                          'swi

 ;; python
 python-indent-guess-indent-offset-verbose
 nil

 ;; redtick
 redtick-history-file                   nil
 redtick-play-sound                     t
 redtick-work-sound                     ""

 ;; reftex
 reftex-cite-format                     'biblatex
 reftex-comment-citations               t
 reftex-plug-into-AUCTeX                t
 reftex-revisit-to-follow               t

 ;; replace
 list-matching-lines-current-line-face  'highlight
 list-matching-lines-jump-to-current-line
 t

 ;; rfc-mode
 rfc-mode-browse-input-function         #'completing-read
 rfc-mode-directory                     (car ffap-rfc-directories)

 ;; sendmail
 send-mail-function                     #'sendmail-send-it
 sendmail-program                       "msmtp"

 ;; sh-script
 sh-basic-offset                        2

 ;; shr
 shr-bullet                             "• "
 shr-hr-line                            ?─

 ;; simple
 async-shell-command-display-buffer     nil
 kill-do-not-save-duplicates            t
 kill-whole-line                        t
 mail-user-agent                        'gnus-user-agent
 next-error-recenter                    '(4)
 read-extended-command-predicate        #'command-completion-default-include-p
 read-mail-command                      'gnus
 save-interprogram-paste-before-kill    t
 set-mark-command-repeat-pop            t
 shell-command-prompt-show-cwd          t
 what-cursor-show-names                 t
 yank-pop-change-selection              t

 ;; smiley
 smiley-style                           'emoji

 ;; solar
 calendar-time-display-form
 '(24-hours ":" minutes (and time-zone (concat " (" time-zone ")")))

 ;; speedbar
 speedbar-show-unknown-files            t
 speedbar-update-flag                   nil
 speedbar-use-images                    t
 speedbar-vc-do-check                   nil

 ;; startup
 auto-save-list-file-prefix             (blc-file blc-index-dir
                                                  "auto-save-list/.saves-")
 inhibit-default-init                   t
 inhibit-startup-screen                 t
 initial-scratch-message                (or (blc-scratch-cowtune)
                                            initial-scratch-message)
 user-mail-address                      (or (car (blc-msmtp-addresses))
                                            user-mail-address)

 ;; swiper
 swiper-goto-start-of-match             t

 ;; term
 term-ansi-buffer-base-name             "term"
 term-suppress-hard-newline             t

 ;; tex
 TeX-auto-save                          t
 TeX-debug-warnings                     t
 TeX-parse-self                         t

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

 ;; transient
 transient-enable-popup-navigation      t

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
 wdired-use-dired-vertical-movement     'sometimes

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
 whitespace-global-modes
 (list 'not
       #'csv-mode
       #'dired-mode
       #'json-mode
       #'magit-mode
       #'markdown-mode
       #'nov-mode
       #'org-mode
       #'shell-mode)
 whitespace-style
 '(face lines-tail missing-newline-at-eof tab-mark)

 ;; wid-edit
 widget-menu-minibuffer-flag            t

 ;; windmove
 windmove-window-distance-delta         2
 windmove-wrap-around                   t

 ;; window
 display-buffer-alist
 `((,#'blc-term-p . ,blc-other-window-action))
 frame-auto-hide-function               #'blc-delete-spare-frame
 pop-up-frames                          'graphic-only
 scroll-error-top-bottom                t
 split-window-keep-point                nil

 ;; xref
 xref-search-program                    'ripgrep

 ;; youtube-dl
 youtube-dl-directory                   (blc-user-dir "VIDEOS"))

;;; Hooks

(blc-hook
  ;; auctex
  (:hooks TeX-after-compilation-finished-functions
          :fns TeX-revert-document-buffer)
  (:hooks LaTeX-mode-hook :fns (blc-TeX-command-default
                                turn-on-reftex))

  ;; auth-source
  (:hooks auth-source-backend-parser-functions :fns blc-pass-backend-parse)

  ;; bbdb
  (:hooks gnus-after-exiting-gnus-hook :fns blc-kill-bbdb-buffer)
  (:hooks gnus-startup-hook            :fns bbdb-insinuate-gnus)

  ;; blc-lib
  (:fns blc-turn-off-trailing-whitespace :hooks (5x5-mode-hook
                                                 Info-mode-hook
                                                 calendar-mode-hook
                                                 comint-mode-hook
                                                 compilation-mode-hook
                                                 eshell-mode-hook
                                                 eww-buffers-mode-hook
                                                 eww-mode-hook
                                                 help-mode-hook
                                                 log-view-mode-hook
                                                 logview-mode-hook
                                                 message-mode-hook
                                                 messages-buffer-mode-hook
                                                 minibuffer-setup-hook
                                                 nov-mode-hook
                                                 term-mode-hook
                                                 youtube-dl-list-mode-hook))

  ;; bog
  (:hooks org-mode-hook :fns bog-mode)

  ;; bug-reference
  (:hooks prog-mode-hook :fns bug-reference-prog-mode)
  (:hooks text-mode-hook :fns bug-reference-mode)

  ;; cc-mode
  (:hooks c-mode-common-hook :fns hs-minor-mode)

  ;; compile
  (:hooks compilation-filter-hook      :fns ansi-color-compilation-filter)
  (:hooks compilation-finish-functions :fns blc-compile-end)
  (:hooks compilation-start-hook       :fns blc-compile-start)

  ;; dafny-mode
  (:hooks dafny-mode-hook :fns blc-turn-off-prettify-symbols)

  ;; dired
  (:hooks dired-mode-hook :fns (blc-turn-on-silent-auto-revert
                                blc-turn-on-line-truncation
                                turn-on-gnus-dired-mode))

  ;; electric
  (:fns blc-turn-off-electric-indent-local :hooks (haskell-cabal-mode-hook
                                                   haskell-mode-hook))

  ;; elisp-mode
  (:hooks emacs-lisp-mode-hook :fns blc-rainbow-mode)

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
  (:hooks gnus-started-hook :fns blc-gc-thresh-restore)

  ;; go-mode
  (:hooks go-mode-hook :fns blc-go-whitespace-style)

  ;; haskell-mode
  (:hooks haskell-mode-hook :fns haskell-indent-mode)

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

  ;; ibuffer
  (:hooks ibuffer-mode-hook :fns blc-ibuffer-filter-groups)

  ;; isearch
  (:hooks isearch-mode-hook :fns blc-isearch-delight)

  ;; js2-mode
  (:hooks js2-mode-hook :fns (js2-highlight-unused-variables-mode
                              js2-refactor-mode
                              blc-xref-js2-install-backend))

  ;; magit-diff
  (:fns blc-turn-on-visual-lines :hooks (magit-diff-mode-hook
                                         magit-status-mode-hook))

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

  ;; python
  (:hooks python-mode-hook :fns blc-python-pep-8-comments)

  ;; redtick
  (:hooks redtick-after-rest-hook :fns blc-redtick-notify-work)
  (:hooks redtick-before-rest-hook :fns blc-redtick-notify-break)
  (:hooks redtick-mode-hook :fns blc-stop-redtick)

  ;; simple
  (:fns display-line-numbers-mode :hooks visual-line-mode-hook)
  (:fns turn-on-auto-fill         :hooks (bookmark-edit-annotation-mode
                                          LaTeX-mode-hook
                                          org-mode-hook))

  ;; startup
  (:hooks window-setup-hook :depth t :fns (blc-messages-trailing-whitespace
                                           blc-report-init-time))

  ;; term
  (:hooks term-exec-hook :fns blc-term-rename)

  ;; xt-mouse
  (:fns blc-turn-on-xterm-mouse :hooks (after-make-frame-functions
                                        window-setup-hook)))

;;; Bindings

(define-prefix-command 'blc-jump-map)
(define-prefix-command 'blc-org-map)

(blc-define-keys
  ((current-global-map)
   ([?\C-\S-v]                            . #'scroll-other-window)
   ([S-next]                              . #'blc-small-scroll-up)
   ([S-prior]                             . #'blc-small-scroll-down)
   ([S-up]                                . #'windmove-up)
   ([S-down]                              . #'windmove-down)
   ([S-left]                              . #'windmove-left)
   ([S-right]                             . #'windmove-right)
   ([f6]                                  . #'gif-screencast)
   ([remap bookmark-jump]                 . #'counsel-bookmark)
   ([remap capitalize-word]               . #'capitalize-dwim)
   ([remap count-lines-page]              . #'blc-count-lines)
   ([remap describe-bindings]             . #'counsel-descbinds)
   ([remap describe-function]             . #'counsel-describe-function)
   ([remap describe-symbol]               . #'counsel-describe-symbol)
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
   ([remap list-buffers]                  . #'ibuffer-list-buffers)
   ([remap load-library]                  . #'counsel-load-library)
   ([remap load-theme]                    . #'counsel-load-theme)
   ([remap menu-bar-open]                 . #'counsel-tmm)
   ([remap org-goto]                      . #'counsel-org-goto)
   ([remap org-set-tags-command]          . #'counsel-org-tag)
   ([remap pop-global-mark]               . #'counsel-mark-ring)
   ([remap rename-buffer]                 . #'blc-rename-buffer)
   ([remap save-buffers-kill-terminal]    . #'save-buffers-kill-emacs)
   ([remap switch-to-buffer]              . #'ivy-switch-buffer)
   ([remap switch-to-buffer-other-window] . #'ivy-switch-buffer-other-window)
   ([remap upcase-word]                   . #'upcase-dwim)
   ([remap zap-to-char]                   . #'zap-up-to-char))

  (mode-specific-map
   ([?\M-n]                               . #'next-logical-line)
   ([?\M-p]                               . #'previous-logical-line)
   ("/"                                   . #'define-word-at-point)
   ("["                                   . #'raise-sexp)
   ("]"                                   . #'delete-pair)
   ("C"                                   . #'copy-from-above-command)
   ("G"                                   . #'blc-github-notifications)
   ("M"                                   . #'blc-mbsync-all)
   ("O"                                   . #'ff-find-other-file)
   ("b"                                   . #'bog-command-map)
   ("c"                                   . #'compile)
   ("e"                                   . #'ielm)
   ("4e"                                  . #'blc-ielm-other-window)
   ("i"                                   . #'blc-indent-relative)
   ("j"                                   . #'blc-jump-map)
   ("m"                                   . #'blc-mbsync)
   ("o"                                   . #'blc-org-map)
   ("u"                                   . #'counsel-unicode-char))

  (blc-jump-map
   ("b"                                   . #'ibuffer-jump)
   ("d"                                   . #'counsel-dired-jump)
   ("e"                                   . #'eww)
   ("l"                                   . #'counsel-locate)
   ("m"                                   . #'magit-find-file)
   ("4m"                                  . #'magit-find-file-other-window)
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

  (bog-command-map
   ("B"                                   . #'blc-bog-staged-bib)
   ("S"                                   . #'blc-bog-dired-stage))

  (ctl-x-map
   ("\C-n"                                . #'blc-open-next-line)
   ("\C-p"                                . #'blc-open-previous-line)
   ("/"                                   . #'engine-mode-prefixed-map)
   ("7"                                   . #'blc-transpose-split)
   ("B"                                   . #'blc-bury-buffer)
   ("M"                                   . #'blc-gnus))

  (ctl-x-4-map
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
   ("V"                                   . #'scroll-other-window-down))

  (goto-map
   ("\t"                                  . #'blc-move-to-column)
   ("e"                                   . #'first-error)
   ("f"                                   . #'avy-goto-line))

  (help-map
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

  (project-prefix-map
   ("S"                                   . #'project-search)
   ("o"                                   . #'blc-project-complete-regexp))

  (search-map
   ("A"                                   . #'counsel-ag)
   ("c"                                   . #'fileloop-continue)
   ("g"                                   . #'counsel-git-grep)
   ("r"                                   . #'counsel-rg)
   ("s"                                   . #'counsel-grep-or-swiper)))


;;; Packages

;;;; ag

(with-eval-after-load 'ag
  (add-to-list 'ag-arguments "--context=5"))

;;;; apt-sources-list

(add-to-list 'auto-mode-alist
             (cons (rx (| ".sources"
                          (: "sources" (? ".list.d/" (+ anychar)) ".list"))
                       eos)
                   #'apt-sources-list-mode))

;;;; auctex

(with-eval-after-load 'latex
  (add-to-list 'LaTeX-clean-intermediate-suffixes (rx ".vrb"))
  (auctex-latexmk-setup))

(with-eval-after-load 'tex
  (blc-define-keys
    (TeX-mode-map
     ([remap TeX-documentation-texdoc] . #'blc-TeX-doc)
     ([remap TeX-complete-symbol]      . #'completion-at-point)))

  ;; Set priority of pre-configured PDF viewers
  (dolist (viewer '("Zathura" "PDF Tools"))
    (when (assoc viewer TeX-view-program-list-builtin)
      (push (list 'output-pdf viewer) TeX-view-program-selection))))

;;;; auth-source

(with-eval-after-load 'auth-source
  (blc-put auth-source-protocols 'smtp '("smtp" "smtps" "25" "465" "587")))

;;;; battery

(display-battery-mode)

;;;; bbdb

(with-eval-after-load 'bbdb
  (map-do #'add-to-list
          `(;; Support Eircode
            (bbdb-legal-postcodes
             . ,(rx-let ((nchar (n) (= n (in "A-N" "P-Z" digit))))
                  (rx bos (nchar 3) (? ?\s) (nchar 4) eos)))
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

;;;; biblio-download

(with-eval-after-load 'biblio-download
  (when (require 'bog nil t)
    (setq-default biblio-download-directory bog-stage-directory)))

;;;; bibtex

(with-eval-after-load 'bibtex
  (bibtex-set-dialect reftex-cite-format)
  (setq bibtex-entry-format (delq 'numerical-fields bibtex-entry-format))
  (dolist (fmt '(delimiters realign))
    (add-to-list 'bibtex-entry-format fmt))
  (add-to-list 'bibtex-files 'bibtex-file-path))

;;;; cc-mode

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map "\C-m" #'c-context-line-break)

  (let ((name "blc"))
    (c-add-style name '("linux"
                        (c-basic-offset . 2)
                        (c-offsets-alist
                         (access-label      .  /)
                         (arglist-close     .  0)
                         (case-label        .  +)
                         (inher-intro       . ++)
                         (inlambda          .  0)
                         (inline-open       .  0)
                         (innamespace       .  0)
                         (member-init-intro . ++))))

    (blc-put c-default-style 'other name)

    (define-advice c-set-style (:after (&rest _) blc-comment-style)
      "Set default comment style after `c-indentation-style'."
      (when (eq major-mode #'c-mode)
        (c-toggle-comment-style
         (if (string-equal c-indentation-style name) -1 1))))))

;;;; chess

(with-eval-after-load 'chess
  (setq-default chess-images-directory
                (blc-dir (blc-package-dir 'chess) "pieces" "xboard")))

;;;; comint

(with-eval-after-load 'comint
  (define-key comint-mode-map "\C-c\C-r" nil))

;;;; conf-mode

(add-to-list 'auto-mode-alist (cons (rx ".dirs" eos) #'conf-unix-mode))

;;;; counsel

(with-eval-after-load 'counsel
  (ivy-add-actions #'counsel-M-x
                   `(("j" ,#'blc-counsel-M-x-other-window "other window")))

  ;; Do not match start of input for counsel commands
  (blc-ivy-strip-init-inputs (rx bos "counsel-")))

;;;; cus-edit

(when (file-exists-p
       (setq-default custom-file
                     (expand-file-name "custom.el" user-emacs-directory)))
  (lwarn 'blc :warning "Custom file %s exists but not loaded" custom-file))

;;;; custom

(load-theme 'modus-operandi t)

;;;; dash

(with-eval-after-load 'dash
  (dash-enable-font-lock))

;;;; deb-view

(add-to-list 'auto-mode-alist (cons (rx ".deb" eos) #'deb-view-mode))
(with-eval-after-load 'dired
  (require 'deb-view nil t))

;;;; debbugs-gnu

(with-eval-after-load 'debbugs-gnu
  (setq-default debbugs-gnu-default-severities debbugs-gnu-all-severities))

;;;; delight

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

   ;; hi-lock
   (hi-lock-mode
    (:eval (if (or hi-lock-interactive-patterns hi-lock-file-patterns) "⛯" ""))
    hi-lock)

   ;; ielm
   (inferior-emacs-lisp-mode "(>)" :major)

   ;; info
   (Info-mode "📘" :major)

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

   ;; outline
   (outline-minor-mode "ⓞ" outline)

   ;; rainbow-mode
   (rainbow-mode "🌈" rainbow-mode)

   ;; subword
   (subword-mode nil subword)

   ;; view
   (view-mode "👓" view)

   ;; whitespace
   (global-whitespace-mode nil whitespace)))

;;;; delsel

(delete-selection-mode)

;;;; diff

(with-eval-after-load 'diff
  (setq diff-switches (blc-as-list diff-switches))
  (add-to-list 'diff-switches "--show-c-function"))

;;;; dired

(with-eval-after-load 'dired
  (define-key dired-mode-map "_" #'dired-create-empty-file))

;;;; dired-aux

(with-eval-after-load 'dired-aux
  (mapc (apply-partially #'add-to-list 'dired-compress-files-alist)
        '(("\\.tar\\.7z\\'" . "tar -c %i | 7zr a -si %o")
          ("\\.7z\\'"       . "7zr a %o %i"))))

;;;; dired-x

(with-eval-after-load 'dired
  (require 'dired-x)

  (setq-default dired-omit-files (rx (| (: bos ?. (not ?.))
                                        (regexp dired-omit-files))))

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

;;;; eglot

(with-eval-after-load 'eglot
  (let ((modes (list #'c++-mode #'c-mode))
        ;; Don't litter projects with `ccls' cache files.
        (init (json-serialize
               `(:cache (:directory ,(blc-dir (xdg-cache-home) "ccls"))))))
    (dolist (server `(("ccls" ,(concat "--init=" init)) ("clangd")))
      (push (cons modes server) eglot-server-programs))))

;;;; engine-mode

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

;;;; eww

(with-eval-after-load 'eww
  (blc-define-keys
    (eww-bookmark-mode-map
     ("T" . #'blc-eww-toggle-images)
     ("n" . #'next-line)
     ("p" . #'previous-line)
     ("w" . #'blc-eww-bookmark-save))
    (eww-mode-map
     ([remap eww-open-in-new-buffer] . #'blc-eww-open-in-other-window))))

;;;; files

(add-to-list 'safe-local-variable-values
             '(eval . (when buffer-file-name (view-mode))))
(auto-save-visited-mode)

;;;; find-func

(with-eval-after-load 'find-func
  (setq-default find-function-source-path (blc-src-path)))

;;;; flex-mode

(add-to-list 'auto-mode-alist (cons (rx ".lex" eos) #'flex-mode))

;;;; flymake

(with-eval-after-load 'flymake
  (blc-define-keys
    (flymake-mode-map
     ([?\M-n] . #'flymake-goto-next-error)
     ([?\M-p] . #'flymake-goto-prev-error))))

;;;; forge

(with-eval-after-load 'forge
  (remove-hook 'forge-post-mode-hook #'turn-on-flyspell))

;;;; frame

(window-divider-mode)

;;;; gif-screencast

(with-eval-after-load 'gif-screencast
  (define-key gif-screencast-mode-map [f7] #'gif-screencast-stop))

;;;; git-annex

(with-eval-after-load 'dired
  (require 'git-annex nil t))

;;;; git-commit

(with-eval-after-load 'git-commit
  (add-to-list 'git-commit-style-convention-checks 'overlong-summary-line))

;;;; gnus

(with-eval-after-load 'gnus
  (blc-gc-thresh-maximise))

;;;; go-mode

(with-eval-after-load 'go-mode
  (when (executable-find "goimports")
    (setq-default gofmt-command "goimports")))

;;;; gscholar-bibtex

(with-eval-after-load 'gscholar-bibtex
  (when-let ((src (assoc "Google Scholar" gscholar-bibtex-available-sources)))
    (setq-default gscholar-bibtex-default-source (car src))))

;;;; hacker-typer

(with-eval-after-load 'hacker-typer
  (require 'mm-util)
  (setq-default
   hacker-typer-files
   (mapcar (apply-partially #'concat "file://")
           (directory-files (blc-dir source-directory "src") t (rx ".c" eos) t))
   hacker-typer-random-range
   (mapcar (apply-partially #'* 2)
           hacker-typer-random-range)))

;;;; hideshow

(with-eval-after-load 'hideshow
  (define-key hs-minor-mode-map "\C-c\t" #'hs-toggle-hiding))

;;;; highlight-escape-sequences

(turn-on-hes-mode)

;;;; holidays

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

;;;; ibuf-ext

(with-eval-after-load 'ibuf-ext
  (push `("blc"
          ("Help"
           (or (predicate . (apply #'derived-mode-p ibuffer-help-buffer-modes))
               (and (name . "Ivy Help") (starred-name))))
          ("Gnus"
           (or (saved . "gnus")
               (name . "mbsync")
               (derived-mode . gnus-server-mode)
               (predicate . (memq (current-buffer)
                                  (list (bound-and-true-p gnus-dribble-buffer)
                                        (bound-and-true-p bbdb-buffer))))
               (predicate . (equal (bound-and-true-p bbdb-buffer-name)
                                   (buffer-name)))
               (predicate . (seq-some (apply-partially #'equal (buffer-name))
                                      blc-gnus-log-buffers))))
          ,@(mapcar
             (lambda (root)
               (let ((pr (project-current nil root)))
                 `(,(directory-file-name root)
                   (predicate . (equal (if (eq blc-project 'unset)
                                           (setq blc-project (project-current))
                                         blc-project)
                                       ',pr))
                   (predicate . (not (eq (bound-and-true-p ielm-working-buffer)
                                         (current-buffer)))))))
             (project-known-project-roots))
          ("Package" (saved . "package"))
          ("Code"
           (or (derived-mode . prog-mode)
               (derived-mode . conf-mode)))
          ("Dired" (derived-mode . dired-mode))
          ("Log"
           (or (derived-mode . TeX-output-mode)
               (derived-mode . compilation-mode)
               (derived-mode . ivy-occur-mode)
               (derived-mode . messages-buffer-mode)
               (derived-mode . tags-table-mode)
               (predicate . (equal (bound-and-true-p dired-log-buffer)
                                   (buffer-name)))
               (and (starred-name)
                    (or (name . "Async Shell Command")
                        (name . "Backtrace")
                        (name . "Warnings")
                        (name . "WoMan-Log")
                        (name . "dropbox")))))
          ("PDF" (derived-mode . pdf-view-mode))
          ("Image" (derived-mode . image-mode))
          ("Process" (or (process) (derived-mode . eshell-mode)))
          ("TeX" (saved . "TeX"))
          ("Text" (saved . "text document"))
          ("Web" (saved . "web"))
          ("Book"
           (or (derived-mode . bookmark-bmenu-mode)
               (derived-mode . bookmark-edit-annotation-mode)
               (and (name . "Bookmark Annotation") (starred-name)))))
        ibuffer-saved-filter-groups)

  (let* ((dirs (list blc-dataroot-dir package-user-dir))
         (full (mapcar #'expand-file-name dirs))
         (abbr (mapcar #'abbreviate-file-name full))
         (re   (blc-rx `(: bos (| ,@full ,@abbr)))))
    (add-to-list 'ibuffer-saved-filters `("package" (filename . ,re)))))

;;;; ibuffer

(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map [remap ibuffer-find-file] #'blc-ibuffer-ffap)

  (mapc (apply-partially #'add-to-list 'ibuffer-help-buffer-modes)
        '(Custom-mode Man-mode shortdoc-mode woman-mode)))

;;;; info

(with-eval-after-load 'info
  (define-key Info-mode-map "k" #'blc-info-kill))

;;;; info-look

(with-eval-after-load 'info-look
  (dash-register-info-lookup))

;;;; ivy

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

  ;; Set defaults
  (blc-put ivy-format-functions-alist t #'ivy-format-function-arrow)
  (blc-put ivy-more-chars-alist       t 2)
  (blc-put ivy-re-builders-alist      t #'ivy--regex-ignore-order)

  ;; Fix ordering
  (map-do (lambda (sort callers)
            (dolist (caller callers)
              (blc-put ivy-sort-functions-alist caller sort)))
          `((nil                 t)
            (,#'always           ,#'ivy-bibtex
                                 ,#'package-install)
            (,#'blc-file-lessp   ,#'project-find-file)
            (,#'string-lessp     ,#'blc-scratch
                                 ,#'counsel-M-x
                                 ,#'find-face-definition)))

  (blc-put ivy-sort-matches-functions-alist
           #'ivy-completion-in-region #'blc-ivy-string<)

  (dolist (caller (list #'Info-menu #'webjump))
    (setq ivy-completing-read-handlers-alist
          (map-delete ivy-completing-read-handlers-alist caller)))

  ;; Recursive minibuffers
  (minibuffer-depth-indicate-mode)

  ;; Do not match start of input for org commands
  (blc-ivy-strip-init-inputs (rx bos "org-")))

;;;; ivy-bibtex

(with-eval-after-load 'ivy-bibtex
  (add-to-list 'bibtex-completion-additional-search-fields "date")
  (mapc (apply-partially #'add-to-list 'bibtex-completion-bibliography)
        (blc-bib-files))

  (when (require 'bog nil t)
    (setq-default bibtex-completion-library-path bog-file-directory
                  bibtex-completion-notes-path   bog-note-directory)))

;;;; jq-mode

(with-eval-after-load 'json-mode
  (define-key json-mode-map "\C-c\C-q" #'jq-interactively))

;;;; js

(with-eval-after-load 'js
  (setq-default js-enabled-frameworks
                (seq-intersection '(dojo javascript prototype)
                                  js-enabled-frameworks)))

;;;; js2-mode

(map-do (lambda (alist re)
          (add-to-list alist (cons re #'js2-mode)))
        `((auto-mode-alist        . ,(rx ".js" eos))
          (interpreter-mode-alist . ,(rx (| "node" "nodejs")))))

(with-eval-after-load 'js2-mode
  (blc-define-keys
    (js2-mode-map
     ("\r" . #'js2-line-break)
     ([?\M-.]))))

;;;; js2-refactor

(with-eval-after-load 'js2-refactor
  (js2r-add-keybindings-with-prefix "\C-c\C-m"))

;;;; json-mode

(add-to-list 'auto-mode-alist (cons (rx ?. (| "jl" "jsonl") eos) #'json-mode))

;;;; ledger-mode

(add-to-list 'auto-mode-alist (cons (rx ".ledger" eos) #'ledger-mode))

(with-eval-after-load 'ledger-mode
  (define-key ledger-mode-map "\e\t" #'completion-at-point)

  (setq-default ledger-default-date-format ledger-iso-date-format)

  (ledger-reports-add "blc-account"
                      (string-join '("%(binary)"
                                     "--dc"
                                     "--file %(ledger-file)"
                                     "register %(account)")
                                   " ")))

;;;; lunar

(with-eval-after-load 'lunar
  (setq-default
   lunar-phase-names
   (mapcar (lambda (name)
             (char-to-string (char-from-name (concat name " symbol") t)))
           lunar-phase-names)))

;;;; magit

(with-eval-after-load 'magit
  (require 'blc-magit))

;;;; magit-extras

(with-eval-after-load 'magit-extras
  ;; Adapted from URL `https://github.com/npostavs/emacs.d'
  (setq-default
   magit-pop-revision-stack-format
   (list "format:[%N] "
         "\
[%N]: %s
%h %ci
https://git.sv.gnu.org/cgit/emacs.git/commit/?id=%H\n"
         (caddr magit-pop-revision-stack-format))))

;;;; magit-log

(with-eval-after-load 'magit-log
  (setq-default magit-log-margin
                '(t age-abbreviated magit-log-margin-width t 16)))

;;;; magit-utils

(with-eval-after-load 'magit-utils
  (require 'ivy))

;;;; make-mode

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
    (set targets (seq-remove (apply-partially #'string-search ".")
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

;;;; man

(with-eval-after-load 'man
  (blc-define-keys
    (Man-mode-map
     ("]" . #'blc-man-next-buffer)
     ("[" . #'blc-man-previous-buffer))))

;;;; markdown-mode

(with-eval-after-load 'markdown-mode
  (let ((lang "lang-el"))               ; For StackExchange
    (add-to-list 'markdown-gfm-additional-languages lang)
    (blc-put* markdown-code-lang-modes lang #'emacs-lisp-mode)))

;;;; message

(with-eval-after-load 'message
  (define-key message-mode-map "\C-c\C-ff" #'blc-message-set-msmtp-from)

  (add-to-list 'message-required-mail-headers 'To)

  (setq-default message-dont-reply-to-names
                (list (regexp-opt '("bug-gnu-emacs@gnu.org"
                                    "submit@debbugs.gnu.org"
                                    "control@debbugs.gnu.org"
                                    "owner@debbugs.gnu.org"))))

  (when-let* ((addresses (blc-msmtp-addresses))
              (addresses (regexp-opt addresses)))
    (setq-default message-alternative-emails addresses)
    (push addresses message-dont-reply-to-names))

  (setq-default message-expand-name-databases
                (delq 'eudc message-expand-name-databases)))

;;;; mpc

(with-eval-after-load 'mpc
  (setq mpc-frame-alist (map-delete mpc-frame-alist 'font)))

;;;; nov

(add-to-list 'auto-mode-alist (cons (rx ".epub" eos) #'nov-mode))

;;;; org

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

;;;; org-agenda

(with-eval-after-load 'org-agenda
  (if-let* ((key "n")
            (cmd (seq-take (blc-get org-agenda-custom-commands key) 2))
            ((= (length cmd) 2)))
      (blc-put* org-agenda-custom-commands key
                `(,@cmd () ,(blc-file org-directory "agenda.html")))
    (lwarn 'blc :error "Could not hijack `org-agenda-custom-commands'"))

  (mapc (lambda (icon)
          (blc-put* org-agenda-category-icon-alist
                    (rx bos (literal (file-name-base icon)) eos)
                    (list (file-truename icon) nil nil :ascent 'center)))
        (and-let* ((dir (blc-dir org-directory "icons"))
                   ((file-directory-p dir)))
          (nreverse
           (directory-files dir t directory-files-no-dot-files-regexp t)))))

;;;; org-capture

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

;;;; org-pomodoro

(with-eval-after-load 'org-pomodoro
  (setq-default org-pomodoro-format
                (blc-sed-tree "pomodoro" "🍅" org-pomodoro-format nil t)))

;;;; ox-html

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

;;;; ox-publish

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
           '("daft" "recipes"))
   org-publish-timestamp-directory (blc-dir blc-index-dir "org-timestamps")))

;;;; paren

(show-paren-mode)

;;;; paren-face

(global-paren-face-mode)

;;;; pdf-tools

(map-do (lambda (alist key)
          (add-to-list alist (cons key #'blc-pdf-tools-defer)))
        `((auto-mode-alist  . ,(rx ".pdf" eos))
          (magic-mode-alist . "%PDF")))

;;;; project

(with-eval-after-load 'project
  (define-key project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

;;;; prolog

(add-to-list 'auto-mode-alist (cons (rx ".pl" eos) #'prolog-mode))

;;;; reftex

(with-eval-after-load 'reftex
  (mapc (apply-partially #'add-to-list 'reftex-default-bibliography)
        (blc-bib-files)))

;;;; simple

(column-number-mode)

;;;; so-long

(global-so-long-mode)

;;;; solar

(with-eval-after-load 'solar
  (when-let* ((loc (blc-system-location)))
    (blc-solar-set-location loc)))

;;;; subword

(global-subword-mode)

;;;; term

(with-eval-after-load 'term
  (dolist (mapsym '(term-mode-map term-raw-map))
    (dolist (fn '(term-char-mode term-line-mode))
      (define-key (symbol-value mapsym)
        (vector 'remap fn) #'blc-toggle-subterm-mode))))

;;;; time

(display-time-mode)

;;;; url

(with-eval-after-load 'url-cookie
  (add-to-list 'url-cookie-untrusted-urls "economist\\.com"))

;;;; web-mode

(add-to-list 'auto-mode-alist (cons (rx ".mustache" eos) #'web-mode))

;;;; whitespace

(global-whitespace-mode)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; init.el ends here
