;;; gnus.el --- gnus init file for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://gitlab.com/basil-conto/dotfiles

;;; Commentary:

;; TODO:
;; * Asynchronous rescanning
;; * Notifications

;;; Code:

;;; Dependencies

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

(require 'blc-lib)
(require 'blc-mbsync)

(require 'map)
(require 'seq)
(eval-when-compile
  (require 'gnus-sum)
  (require 'nnheader)
  (require 'subr-x))

;;; Byte-compiler declarations

(eval-when-compile
  (defvar gnus-article-mode-map)
  (defvar gnus-buffer-configuration)
  (defvar gnus-directory)
  (defvar gnus-level-default-subscribed)
  (defvar gnus-newsgroup-limit)
  (defvar gnus-sorted-header-list)
  (defvar gnus-startup-file)
  (defvar gnus-summary-mode-map)
  (defvar gnus-tmp-group)
  (defvar gnus-topic-mode-map)
  (defvar gnus-visible-headers)
  (defvar nnir-imap-search-arguments)

  (declare-function bbdb-mua-summary-unify                "bbdb-mua")
  (declare-function diff-file-next                        "diff-mode")
  (declare-function diff-file-prev                        "diff-mode")
  (declare-function diff-hunk-next                        "diff-mode")
  (declare-function diff-hunk-prev                        "diff-mode")
  (declare-function gnus-demon-add-handler                "gnus-demon")
  (declare-function gnus-demon-scan-news                  "gnus-demon")
  (declare-function gnus-group-set-timestamp              "gnus-group")
  (declare-function gnus-group-timestamp                  "gnus-group")
  (declare-function gnus-score-find-single                "gnus-score")
  (declare-function gnus-article-sort-by-most-recent-date "gnus-sum")
  (declare-function gnus-article-sort-by-number           "gnus-sum")
  (declare-function gnus-summary-goto-article             "gnus-sum")
  (declare-function gnus-summary-save-parts               "gnus-sum")
  (declare-function gnus-thread-sort-by-date              "gnus-sum")
  (declare-function gnus-thread-sort-by-most-recent-date  "gnus-sum")
  (declare-function gnus-thread-sort-by-number            "gnus-sum")
  (declare-function gnus-user-date                        "gnus-sum")
  (declare-function gnus-current-topic                    "gnus-topic")
  (declare-function gnus-topic-fold                       "gnus-topic")
  (declare-function gnus-topic-goto-topic                 "gnus-topic")
  (declare-function gnus-topic-mode                       "gnus-topic")
  (declare-function gnus-completing-read                  "gnus-util"))

(autoload 'nnir-run-blc-notmuch "blc-notmuch")

;;; Utilities

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
  (dolist (log (seq-filter #'get-buffer blc-gnus-log-buffers))
    (with-current-buffer log
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (delete-region (point-min)
                       (line-beginning-position
                        (- 1 message-log-max)))))))

(defun blc-gnus-kill-logs ()
  "Kill buffers named in `blc-gnus-log-buffers'."
  (kill-matching-buffers (blc-rx `(: bos (| ,@blc-gnus-log-buffers) eos))))

(defun blc-gnus-topic-fold ()
  "Toggle folding of current topic.
See URL `https://www.emacswiki.org/emacs/GnusTopics'."
  (interactive)
  (gnus-topic-goto-topic (gnus-current-topic))
  (gnus-topic-fold))

(defun blc-gnus-apply-attachments (n)
  "Apply all Gnus patch attachments to `source-directory'.
Pass all text/x-diff parts to git-am(1) and display its output in
the buffer \"*git am*\".  Argument N follows the process/prefix
convention (see the Info node `(gnus) Process/Prefix')."
  (interactive "P")
  (let ((dir (make-temp-file "blc-gnus-" t)))
    (gnus-summary-save-parts (rx (| "text/x-diff" "text/x-patch")) dir n)
    (let* ((default-directory source-directory)
           (files (directory-files dir t (rx ".patch" eos)))
           (proc  (make-process :name "git am"
                                :buffer "*git am*"
                                :connection-type 'pipe
                                :command `("git" "am" ,@files))))
      (display-buffer (process-buffer proc) '(() (allow-no-window . t)))
      (add-function :after (process-sentinel proc)
                    (lambda (proc _msg)
                      (when (and (blc-process-success-p proc)
                                 (file-directory-p dir))
                        (delete-directory dir t)
                        (message "Deleted temporary directory %s" dir)))))))

(defun blc-gnus-format-article (article)
  "Format Gnus ARTICLE for `blc-gnus-goto-article'."
  (let* ((head (gnus-data-header (gnus-data-find article)))
         (time (blc-gnus-user-date (mail-header-date head)))
         (from (bbdb-mua-summary-unify (mail-header-from head))))
    (cons (format "%8s %-24s %s" time from (mail-header-subject head))
          article)))

(defun blc-gnus-goto-article ()
  "Like `gnus-summary-goto-article', but human-readable."
  (interactive)
  (let ((arts (mapcar #'blc-gnus-format-article gnus-newsgroup-limit)))
    (gnus-summary-goto-article
     (blc-get arts (gnus-completing-read "Article" arts t)))))

;;; Options

(setq-default
 ;; gnus
 gnus-extract-address-components        #'mail-extract-address-components
 gnus-group-uncollapsed-levels          3
 gnus-select-method                     '(nnnil)
 gnus-summary-line-format
 (concat "%U"                           ; Read status
         "%R"                           ; Replied status
         "%z"                           ; Article zcore
         "%O"                           ; Download mark
         "%4{:%8&user-date;%} "         ; Date & colon
         "%(%2{%-24,24uB%}"             ; From (BBDB-unified)
         " %B"                          ; Thread tree
         "%3{%s%}%)"                    ; Subject
         "\n")
 gnus-update-message-archive-method     t
 gnus-save-score                        t
 gnus-secondary-select-methods
 `(,@(map-keys-apply (lambda (user)
                       `(nnimap ,user
                                (nnimap-address         "127.0.0.1")
                                (nnimap-record-commands t)
                                (nnimap-stream          network)
                                (nnimap-user            ,user)
                                (nnir-search-engine     imap)))
                     (blc-mbsync-chandirs))
   ;; FIXME: Firewall
   (nntp "news.gwene.org"
         (nntp-record-commands t)))

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

 ;; gnus-score
 gnus-inhibit-slow-scoring              t
 gnus-score-expiry-days                 nil
 gnus-score-find-score-files-function   #'gnus-score-find-single

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
 gnus-auto-center-summary               nil
 gnus-auto-select-first                 nil
 gnus-auto-select-next                  nil
 gnus-sort-gathered-threads-function    #'gnus-thread-sort-by-date
 gnus-subthread-sort-functions
 `(,#'gnus-thread-sort-by-number
   ,#'gnus-thread-sort-by-date)
 gnus-sum-thread-tree-false-root        ""
 gnus-sum-thread-tree-leaf-with-other   "├➤"
 gnus-sum-thread-tree-root              ""
 gnus-sum-thread-tree-single-leaf       "╰➤"
 gnus-sum-thread-tree-indent            " "
 gnus-sum-thread-tree-vertical          "│"
 gnus-summary-dummy-line-format         (format "%5c%11c(%27s%%)\n" ?: ?% "%S")
 gnus-summary-gather-subject-limit      'fuzzy
 gnus-summary-next-group-on-exit        nil
 gnus-thread-hide-killed                nil
 gnus-thread-sort-functions
 `(,#'gnus-thread-sort-by-number
   ,#'gnus-thread-sort-by-most-recent-date)
 gnus-user-date-format-alist            ; Max. length 8
 `(((gnus-seconds-today)                . "%R")
   ((float-time
     (time-add
      ',(days-to-time 1)
      (gnus-seconds-today)))            . "-%R")
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
 (regexp-opt (map-values (blc-mbsync-chandirs))))

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

;;; Libraries

;; gnus-art

(with-eval-after-load 'gnus-art
  (blc-define-keys
    (gnus-article-mode-map
     ("vN" . #'diff-file-next)
     ("vP" . #'diff-file-prev)
     ("vd" . #'blc-download)
     ("vn" . #'diff-hunk-next)
     ("vp" . #'diff-hunk-prev)))

  (setq-default
   gnus-sorted-header-list
   (mapcan (lambda (header)
             (cons header (when (string-equal header "^To:")
                            (copy-sequence '("^Delivered-To:" "^Reply-To:")))))
           gnus-sorted-header-list)

   gnus-visible-headers
   (blc-rx `(| (: bol (| "Delivered-To" "To") ?:)
               (regexp ,gnus-visible-headers)))))

;; gnus-sum

(with-eval-after-load 'gnus-sum
  (blc-define-keys
    (gnus-summary-mode-map
     ([?\M-r])
     ([?\M-s])
     ([remap gnus-summary-goto-article] . #'blc-gnus-goto-article)
     ("va"                              . #'blc-gnus-apply-attachments))))

;; gnus-topic

(with-eval-after-load 'gnus-topic
  (define-key
    gnus-topic-mode-map [remap gnus-topic-indent] #'blc-gnus-topic-fold))

;; gnus-win

(gnus-add-configuration '(article (frame 1.0
                                         (summary 1.0 point frame-focus)
                                         (article 1.0))))

;; nnir

(with-eval-after-load 'nnir
  (map-do
   #'add-to-list
   `((nnir-engines               . (blc-notmuch ,#'nnir-run-blc-notmuch ()))
     (nnir-imap-search-arguments . ("gmail" . "X-GM-RAW")))))

;;; gnus.el ends here
