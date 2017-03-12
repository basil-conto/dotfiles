;;; .gnus.el --- gnus init file for blc -*- lexical-binding: t -*-

;;; Commentary:

;; TODO:
;; * Improve byte-compilation
;; * Asynchronous rescanning

;;; Code:

(eval-when-compile
  (defvar gnus-tmp-group))

(defun blc-user-date--format (date)
  "Format DATE in a format suitable for `gnus-user-date'."
  (format-time-string "%a, %d %b %Y %T %z" date))

(defun blc-user-date (date)
  "Massage DATE before passing it to `gnus-user-date'."
  (let ((conv (if (stringp date)
                  #'identity
                #'blc-user-date--format)))
    (gnus-user-date (funcall conv date))))

(defun gnus-user-format-function-dgroup (&rest _)
  "User-defined Gnus group line timestamp format."
  (if-let ((time (gnus-group-timestamp gnus-tmp-group)))
      (blc-user-date time)
    ""))

(defun blc-demon-scan-mail ()
  "Daemonically scan for new mail as master.
After scanning, truncate growing network log buffers to
`message-log-max' lines."
  (unless gnus-slave
    (gnus-demon-scan-news)
    (dolist (log blc-gnus-log-buffers)
      (with-current-buffer log
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (delete-region (point-min)
                         (line-beginning-position
                          (- 1 message-log-max))))))))

(defun blc-demon-scan-news ()
  "Activate higher news levels before `blc-demon-scan-mail'."
  (let ((gnus-activate-level (1+ gnus-activate-level)))
    (blc-demon-scan-mail)))

(defun blc-turn-on-demon-scan-mail ()
  "Add daemonic handlers for mail and news scanning."
  (interactive)
  (mapc (-applify #'gnus-demon-add-handler)
        `((,#'blc-demon-scan-mail 300  5)
          (,#'blc-demon-scan-news 900 30))))

(defun blc-gnus-kill-log-buffers ()
  "Kill buffers named in `blc-gnus-log-buffers'."
  (kill-matching-buffers (format "\\`%s\\'" (regexp-opt blc-gnus-log-buffers))))

(defun blc-gnus-topic-fold ()
  "Toggle folding of current topic.
See URL `https://www.emacswiki.org/emacs/GnusTopics'."
  (interactive)
  (gnus-topic-goto-topic (gnus-current-topic))
  (gnus-topic-fold))

(use-package gnus
  :defer
  :init
  (setq-default
   gnus-extract-address-components        #'mail-extract-address-components
   gnus-group-uncollapsed-levels          3
   gnus-invalid-group-regexp              "[: `'\"]\\|^$"
   gnus-select-method                     '(nnnil)
   gnus-summary-line-format
   (concat "%U"                           ; Read status
           "%R"                           ; Replied status
           "%z"                           ; Article zcore
           "%O "                          ; Download mark
           "%B"                           ; Thread tree
           "%(%2{%-24,24f%}"              ; From/To
           "%-29= : "                     ; Colon
           "%3{%-50,50s%}%)"              ; Subject
           "%4{%9&user-date;%}"           ; Age-sensitive date
           "\n")
   gnus-update-message-archive-method     t)

  ;; FIXME: NNTP firewall
  (let* ((nngmail
          '((nnimap-address               "imap.gmail.com")
            (nnimap-record-commands       t)
            (nnimap-server-port           "imaps")
            (nnimap-stream                tls)
            (nnir-search-engine           imap)))

         (gmail `(,@(mapcar (pcase-lambda (`(,name))
                              `(nnimap ,name ,@nngmail))
                            (blc-mail-ids))))

         (news  '((nntp "news.gwene.org"
                        (nntp-record-commands t)))))
    (setq-default
     gnus-secondary-select-methods        `(,@gmail ,@news))))

(use-package gnus-art
  :bind (:map gnus-article-mode-map
              ("M-D" . blc-download))
  :init
  (setq-default
   gnus-blocked-images                    nil)

  :config
  ;; Reorder headers
  (let* ((heads 'gnus-sorted-header-list)
         (order (symbol-value heads))
         (tos   '("^To:" "^Delivered-To:" "^Reply-To:")))
    (set-default heads (-splice-list (-cut string= (car tos) <>) tos order)))

  ;; Customise header faces
  (-let* ((head-to                        "^\\(?:Delivered-\\)?To:")
          (head-vis                       (-list gnus-visible-headers))
          (faces-old                      gnus-header-face-alist)
          (faces-new                      `((,head-to nil font-lock-string-face)
                                            ("^Date:" nil font-lock-type-face)))
          (faces                          (-union faces-new faces-old))
          ((heads names contents)         (-unzip faces))
          (names                          (-cycle (-non-nil names))))
    (setq-default
     gnus-header-face-alist               (-zip heads names contents)
     gnus-visible-headers                 (-union `(,head-to) head-vis))))

(use-package gnus-cloud
  :defer
  :init
  (setq-default
   gnus-cloud-method                      "nnimap:personal"
   gnus-cloud-storage-method              'epg
   gnus-cloud-synced-files
   `(,(f-swap-ext gnus-startup-file "eld")
     (:directory ,gnus-directory :match ".*.SCORE\\'"))))

(use-package gnus-desktop-notify
  :ensure
  :commands gnus-desktop-notify-send
  :init
  (setq-default
   gnus-desktop-notify-groups             'gnus-desktop-notify-explicit
   gnus-desktop-notify-format             "%3n: %G"
   gnus-desktop-notify-function           #'gnus-desktop-notify-send)
  (gnus-desktop-notify-mode))

(use-package gnus-demon
  :defer
  :init
  (setq-default gnus-demon-timestep 1))

(use-package gnus-group
  :defer
  :init
  (add-hook 'gnus-exit-gnus-hook #'blc-gnus-kill-log-buffers)
  (mapc (-cut add-hook <> #'gnus-group-set-timestamp)
        '(gnus-group-catchup-group-hook
          gnus-select-group-hook))

  (setq-default
   gnus-goto-next-group-when-activating   nil
   gnus-group-line-format
   (concat "%M"                           ; Marked articles
           "%S"                           ; Subscription
           "%p"                           ; Marked for processing
           "%m"                           ; New mail
           "%B"                           ; Open summary buffer
           "%P"                           ; Topic indentation
           "%5y? %3T!"                    ; Unread and ticked articles
           " : "                          ; Colon
           "%(%-40,40c%)"                 ; Collapsed group name
           "%9u&dgroup;"                  ; Last read
           "\n")))

(use-package gnus-msg
  :defer
  :init
  (setq-default gnus-gcc-mark-as-read t))

(use-package gnus-notifications
  :disabled                               ; Notifies of all unread messages ;_;
  :defer
  :init
  (add-hook 'gnus-after-getting-new-news-hook #'gnus-notifications)
  (setq-default
   gnus-notifications-minimum-level       3))

(use-package gnus-spec
  :defer
  :init
  (setq-default
   gnus-face-2                            'font-lock-keyword-face
   gnus-face-3                            'font-lock-string-face
   gnus-face-4                            'font-lock-comment-face))

(use-package gnus-start
  :defer
  :init
  (setq-default
   gnus-activate-level                    3
   gnus-check-new-newsgroups              nil
   gnus-read-newsrc-file                  nil
   gnus-save-killed-list                  nil
   gnus-save-newsrc-file                  nil))

(use-package gnus-sum
  :defer
  :init
  (setq-default
   gnus-auto-select-first                 nil
   gnus-auto-select-next                  nil
   gnus-sum-thread-tree-false-root        ""
   gnus-sum-thread-tree-leaf-with-other   "├➤ "
   gnus-sum-thread-tree-root              ""
   gnus-sum-thread-tree-single-leaf       "╰➤ "
   gnus-sum-thread-tree-indent            " "
   gnus-sum-thread-tree-vertical          "│ "
   gnus-summary-display-while-building    16
   gnus-summary-gather-subject-limit      'fuzzy
   gnus-summary-next-group-on-exit        nil
   gnus-summary-thread-gathering-function #'gnus-gather-threads-by-references
   gnus-user-date-format-alist            ; Max. length 10
   `(((gnus-seconds-today)                . "%R")
     ((float-time
       (time-add
        ,(days-to-time 1)
        (gnus-seconds-today)))            . "Y %R")
     (,(float-time (days-to-time 7))      . "%a %d")
     ((gnus-seconds-year)                 . "%d %b")
     (t                                   . "%d/%m/%y")))

  :config
  (mapc (-applify #'add-to-list)
        `((gnus-article-sort-functions ,#'gnus-article-sort-by-date t)
          (gnus-thread-sort-functions  ,#'gnus-thread-sort-by-date  t))))

(use-package gnus-topic
  :bind (:map gnus-topic-mode-map
              ;; FIXME: Different tab characters?
              ("<tab>" . blc-gnus-topic-fold)
              ("TAB"   . blc-gnus-topic-fold))
  :init
  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode))

(use-package gnus-util
  :defer
  :init
  (setq-default
   gnus-add-timestamp-to-message          'log
   gnus-verbose                           10
   gnus-verbose-backends                  10))

(use-package mm-decode
  :defer
  :init
  (setq-default
   mm-html-blocked-images                 nil
   mm-text-html-renderer                  'gnus-w3m))

(use-package nnir
  :defer
  :config
  (let ((key "gmail"))
    (add-to-list 'nnir-imap-search-arguments `(,key . "X-GM-RAW"))
    (setq-default nnir-imap-default-search-key key)))

;;; .gnus.el ends here
