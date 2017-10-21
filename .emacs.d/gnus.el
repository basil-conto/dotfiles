;;; gnus.el --- gnus init file for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://github.com/basil-conto/dotfiles

;;; Commentary:

;; TODO:
;; * Asynchronous rescanning
;; * Notifications

;;; Code:

;;; Dependencies

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

(require 'blc-lib)
(eval-when-compile
  (require 'blc-macs))

(require 'map)
(require 'seq)
(require 'shr)
(require 'url-util)
(eval-when-compile
  (require 'subr-x)
  (require 'thunk))

;;; Byte-compiler declarations

(blc-declare-vars
  gnus-article-mode-map
  gnus-directory
  gnus-header-face-alist
  gnus-level-default-subscribed
  gnus-sorted-header-list
  gnus-startup-file
  gnus-tmp-group
  gnus-topic-mode-map
  gnus-visible-headers
  nnir-imap-search-arguments)

(blc-declare-fns
  (gnus-demon gnus-demon-add-handler
              gnus-demon-scan-news)
  (gnus-group gnus-group-set-timestamp
              gnus-group-timestamp)
  (gnus-sum   gnus-article-sort-by-most-recent-date
              gnus-article-sort-by-number
              gnus-gather-threads-by-references
              gnus-thread-sort-by-date
              gnus-thread-sort-by-most-recent-date
              gnus-thread-sort-by-number
              gnus-user-date)
  (gnus-topic gnus-current-topic
              gnus-topic-fold
              gnus-topic-goto-topic
              gnus-topic-mode))

(blc-autoloads
  (blc-notmuch nnir-run-blc-notmuch))

;;; Utilities

(defun blc-download (&optional url file)
  "Download contents of URL to a file named FILE.
Wraps `w3m-download' or emulates it when unavailable, working
with both raw URLs and links."
  (interactive "i\nF")
  (if (fboundp 'w3m-download)
      (w3m-download url file)
    (url-copy-file (or url
                       (url-get-url-at-point)  ; Raw URL
                       (shr-url-at-point nil)) ; Link/image
                   file 0)))                   ; Confirm existing file

(defun blc-gnus-user-date (date)
  "Massage DATE before passing it to `gnus-user-date'."
  (gnus-user-date (funcall (if (stringp date)
                               #'identity
                             #'blc-rfc2822)
                           date)))

(defun gnus-user-format-function-dgroup (&rest _)
  "User-defined Gnus group line timestamp format."
  (if-let* ((time (gnus-group-timestamp gnus-tmp-group)))
      (blc-gnus-user-date time)
    ""))

(defun blc-gnus-truncate-logs ()
  "Truncate Gnus log buffers to `message-log-max' lines."
  (mapc (lambda (log)
          (with-current-buffer log
            (goto-char (point-max))
            (let ((inhibit-read-only t))
              (delete-region (point-min)
                             (line-beginning-position
                              (- 1 message-log-max))))))
        (seq-filter #'get-buffer blc-gnus-log-buffers)))

(defun blc-gnus-kill-logs ()
  "Kill buffers named in `blc-gnus-log-buffers'."
  (kill-matching-buffers (blc-rx `(: bos (| ,@blc-gnus-log-buffers) eos))))

(defun blc-gnus-topic-fold ()
  "Toggle folding of current topic.
See URL `https://www.emacswiki.org/emacs/GnusTopics'."
  (interactive)
  (gnus-topic-goto-topic (gnus-current-topic))
  (gnus-topic-fold))

;;; Options

(setq-default
 ;; gnus
 gnus-extract-address-components        #'mail-extract-address-components
 gnus-group-uncollapsed-levels          3
 gnus-invalid-group-regexp              "[: `'\"]\\|^$"
 gnus-select-method                     '(nnnil)
 gnus-summary-line-format               (blc-gnus-summary-line-format)
 gnus-update-message-archive-method     t
 gnus-secondary-select-methods
 `(,@(seq-map-indexed
      (pcase-lambda (`(,user) i)
        `(nnimap ,user
                 (nnimap-address         ,(format-network-address
                                           `[127 1 0 ,(1+ i)]))
                 (nnimap-record-commands t)
                 (nnimap-stream          network)
                 (nnimap-user            ,user)
                 (nnir-search-engine     imap)))
      (blc-mbsync-maildirs))
   ;; FIXME: Firewall
   (nntp "news.gwene.org"
         (nntp-record-commands t)))

 ;; gnus-art
 gnus-blocked-images                    nil

 ;; gnus-group
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
         "\n")

 ;; gnus-msg
 gnus-gcc-mark-as-read                  t

 ;; gnus-notifications
 gnus-notifications-minimum-level       3

 ;; gnus-spec
 gnus-face-2                            'font-lock-keyword-face
 gnus-face-3                            'font-lock-string-face
 gnus-face-4                            'font-lock-comment-face

 ;; gnus-start
 gnus-activate-level                    gnus-level-default-subscribed
 gnus-check-new-newsgroups              nil
 gnus-read-newsrc-file                  nil
 gnus-save-killed-list                  nil
 gnus-save-newsrc-file                  nil

 ;; gnus-sum
 gnus-article-sort-functions
 `(,#'gnus-article-sort-by-number
   ,#'gnus-article-sort-by-most-recent-date)
 gnus-auto-select-first                 nil
 gnus-auto-select-next                  nil
 gnus-subthread-sort-functions
 `(,#'gnus-thread-sort-by-number
   ,#'gnus-thread-sort-by-date)
 gnus-sum-thread-tree-false-root        ""
 gnus-sum-thread-tree-leaf-with-other   " ├➤ "
 gnus-sum-thread-tree-root              ""
 gnus-sum-thread-tree-single-leaf       " ╰➤ "
 gnus-sum-thread-tree-indent            " "
 gnus-sum-thread-tree-vertical          " │ "
 gnus-summary-gather-subject-limit      'fuzzy
 gnus-summary-next-group-on-exit        nil
 gnus-summary-thread-gathering-function #'gnus-gather-threads-by-references
 gnus-thread-sort-functions
 `(,#'gnus-thread-sort-by-number
   ,#'gnus-thread-sort-by-most-recent-date)
 gnus-user-date-format-alist            ; Max. length 10
 `(((gnus-seconds-today)                . "%R")
   ((float-time
     (time-add
      ',(days-to-time 1)
      (gnus-seconds-today)))            . "Y %R")
   (,(float-time (days-to-time 7))      . "%a %d")
   ((gnus-seconds-year)                 . "%d %b")
   (t                                   . "%d/%m/%y"))

 ;; gnus-util
 gnus-add-timestamp-to-message          'log
 gnus-verbose                           10
 gnus-widen-article-window              t

 ;; nnheader
 gnus-verbose-backends                  10

 ;; nnir
 nnir-notmuch-remove-prefix
 (regexp-opt (map-values (blc-mbsync-maildirs))))

;;; Hooks

(blc-hook
  ;; FIXME: Adding `gnus-notifications' to `gnus-after-getting-new-news-hook'
  ;;        notifies of all unread messages ;_;
  (:hooks gnus-after-getting-new-news-hook :fns blc-gnus-truncate-logs)
  (:hooks gnus-exit-gnus-hook              :fns blc-gnus-kill-logs)
  (:hooks gnus-group-catchup-group-hook    :fns gnus-group-set-timestamp)
  (:hooks gnus-group-mode-hook             :fns gnus-topic-mode)
  (:hooks gnus-select-group-hook           :fns gnus-group-set-timestamp)
  (:hooks gnus-summary-mode-hook           :fns hl-line-mode))

;;; Deferrals

(with-eval-after-load 'gnus-art
  (define-key gnus-article-mode-map "\M-D" #'blc-download)

  (let ((to (rx bol (| "Delivered-To" "To") ?:)))
    (setq-default
     gnus-header-face-alist
     (let ((headers (map-keys gnus-header-face-alist))
           (faces   (map-values-apply #'cadr gnus-header-face-alist)))
       (seq-mapn (lambda (header face)
                   `(,header gnus-header-name ,face))
                 `(,to                   "^Date:"            ,@headers)
                 `(font-lock-string-face font-lock-type-face ,@faces)))

     gnus-sorted-header-list
     (mapcan (lambda (header)
               `(,header ,@(when (string= header "^To:")
                             (copy-sequence '("^Delivered-To:" "^Reply-To:")))))
             gnus-sorted-header-list)

     gnus-visible-headers
     (blc-rx `(| (regexp ,to) (regexp ,gnus-visible-headers))))))

(with-eval-after-load 'gnus-topic
  (define-key
    gnus-topic-mode-map [remap gnus-topic-indent] #'blc-gnus-topic-fold))

(with-eval-after-load 'nnir
  (map-do
   #'add-to-list
   `((nnir-engines               . (blc-notmuch ,#'nnir-run-blc-notmuch ()))
     (nnir-imap-search-arguments . ("gmail" . "X-GM-RAW")))))

;;; gnus.el ends here
