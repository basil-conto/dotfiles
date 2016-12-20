;; FIXME:
;; * Move custom package initialisation to separate file
;; * Byte-compile
(require 'dash)

(eval-when-compile
  (setq-default use-package-verbose 'debug)

  (require 'nnheader)
  (require 'use-package)

  (defvar gnus-tmp-group))

(defun gnus-user-format-function-dgroup (&rest _)
  "User-defined Gnus group line timestamp format."
  (if-let ((time (gnus-group-timestamp gnus-tmp-group)))
      (gnus-user-date (format-time-string "%a, %d %b %Y %T %z" time))
    ""))

(defun gnus-user-format-function-dsum (header)
  "User-defined Gnus summary line timestamp format."
  (let ((date (mail-header-parse-date (mail-header-date header))))
    (format-time-string "%a %d %b %Y %T" date)))

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
   gnus-select-method                     '(nnnil)
   gnus-summary-line-format
   (concat "%U"                           ; Read status
           "%R"                           ; Replied status
           "%z"                           ; Article zcore
           "%O "                          ; Download mark
           "%4{%u&dsum;%} "               ; Full timestamp
           "%B"                           ; Thread tree
           "%(%2{%-32,32f%} %3{%s%}%)"    ; From/To, Subject
           "\n"))

  ;; FIXME: NNTP firewall
  (let* ((nngmail
          '((nnimap-address               "imap.gmail.com")
            (nnimap-record-commands       t)
            (nnimap-server-port           "imaps")
            (nnimap-stream                tls)
            (nnir-search-engine           imap)))

         (gmail `(,@(-map (-lambda ((name))
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
  (setq-default
   gnus-visible-headers
   (-union '("^Delivered-To:")
           (-list gnus-visible-headers))))

(use-package gnus-group
  :defer
  :init
  (mapc (-rpartial #'add-hook #'gnus-group-set-timestamp)
        '(gnus-group-catchup-group-hook
          gnus-select-group-hook))

  (setq-default
   gnus-goto-next-group-when-activating   nil
   gnus-group-line-format
   (concat "%M"                           ; Marked articles
           "%S"                           ; Subscription
           "%p"                           ; Marked for processing
           "%16u&dgroup;"                 ; Last read timestamp
           "%P"                           ; Topic indentation
           "%5y? %3T!"                    ; Unread and ticked articles
           "%m"                           ; New mail
           ":"                            ; Colon
           "%B"                           ; Open summary buffer
           "%(%c%)"                       ; Collapsed group name
           "\n")))

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
   gnus-auto-select-next                  nil
   gnus-sum-thread-tree-false-root        ""
   gnus-sum-thread-tree-leaf-with-other   "├➤ "
   gnus-sum-thread-tree-root              ""
   gnus-sum-thread-tree-single-leaf       "╰➤ "
   gnus-sum-thread-tree-indent            " "
   gnus-sum-thread-tree-vertical          "│ "
   gnus-summary-gather-subject-limit      'fuzzy
   gnus-summary-next-group-on-exit        nil
   gnus-summary-thread-gathering-function #'gnus-gather-threads-by-references
   gnus-user-date-format-alist
   '(((gnus-seconds-today)                . "Today %R")
     ((float-time
       (time-add
        (days-to-time 1)
        (gnus-seconds-today)))            . "Yesterday %R")
     ((float-time
       (days-to-time 7))                  . "%a %R")
     ((gnus-seconds-month)                . "%a %d")
     ((gnus-seconds-year)                 . "%b %d")
     (t                                   . "%b %d %Y"))))

(use-package gnus-topic
  :bind (:map gnus-topic-mode-map
              ;; FIXME: Different tab characters?
              ("<tab>" . blc-gnus-topic-fold)
              ("TAB"   . blc-gnus-topic-fold))
  :init
  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode))

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
