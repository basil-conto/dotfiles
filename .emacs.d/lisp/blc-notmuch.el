;;; blc-notmuch.el --- nnir notmuch engine workaround -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://github.com/basil-conto/dotfiles

;;; Commentary:

;; This is an attempt to fix some nnir+notmuch issues while
;; introducing a new one, namely a coupling to the Maildir mailbox
;; format on Un*x filesystems.

;; The code is very much alpha, exploratory and specific to the
;; author's setup, which involves multiple Gmail-synced Maildirs under
;; a single parent directory indexed by notmuch.

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))
(require 'blc-lib)

(require 'nnir)
(require 'nnmaildir)
(require 'seq)

(eval-when-compile
  (require 'subr-x))

(defvar blc-notmuch-log-stream
  (with-current-buffer (get-buffer-create "*blc notmuch log*")
    (setq buffer-read-only t)
    (point-max-marker))
  "Stream receiving blc-notmuch debugging logs.")

(defun blc-notmuch--log (fmt &rest args)
  "Log a message to `blc-notmuch-log-stream'.
Like `message', but each line is prefixed with a timestamp."
  (let ((inhibit-read-only t))
    (princ (format "%s> %s\n"
                   (format-time-string "%F %T")
                   (apply #'format fmt args))
           blc-notmuch-log-stream)))

(defmacro blc-notmuch--log-vars (&rest vars)
  "Call `blc-notmuch--log' on multiple VARS."
  (declare (indent 0))
  (macroexp-progn (mapcar (lambda (var)
                            `(blc-notmuch--log ,(format "%s: %%s" var) ,var))
                          vars)))

(defun blc-notmuch--cmd (query server &rest groups)
  "Return blc-notmuch command to perform QUERY on SERVER GROUPS."
  `(,nnir-notmuch-program
    "search"
    "--format=text"
    "--output=files"
    ,@(blc-keep
       (lambda (group)
         ;; Filter [Gmail] labels?
         (concat "folder:" (subst-char-in-string
                            ?: ?/ (gnus-short-group-name group))))
       groups t)
    ,@(nnir-read-server-parm 'nnir-notmuch-additional-switches server)
    ,query))

(defun blc-notmuch--reporter (query &optional group)
  "Return a progress reporter for blc-notmuch QUERY on GROUP."
  (make-progress-reporter
   (apply #'concat "Doing blc-notmuch query " query (and group `(" on " ,group)))
   0 1))

(defun nnir-run-blc-notmuch (query server &optional groups)
  "Like `nnir-run-notmuch', but works (to an extent)."
  (blc-notmuch--log-vars query server groups)

  (let ((qstring (alist-get 'query query)))

    (when (string-empty-p qstring)
      (user-error "blc-notmuch: You didn't enter anything"))

    (let* ((prefix    (blc-rx `(: bos (regexp
                                       ,(nnir-read-server-parm
                                         'nnir-notmuch-remove-prefix server)))))
           (cmd       (apply #'blc-notmuch--cmd qstring server groups))
           (reporter  (blc-notmuch--reporter qstring)))

      (blc-notmuch--log-vars qstring prefix)
      (blc-notmuch--log "calling: %s" (string-join cmd " "))

      (condition-case err
          (blc-keep
           (lambda (line)
             (when-let* ((dir (file-name-directory line)))
               (let* ((art   (file-name-nondirectory line))
                      (group (blc-sed prefix ""
                                      (directory-file-name (blc-parent-dir dir))
                                      t))
                      (name  (gnus-group-full-name group server))
                      (num   (nnmaildir-base-name-to-article-number
                              (substring art 0 (string-match-p ":" art))
                              group nil)))
                 (blc-notmuch--log-vars line dir art group name num)
                 `[,name ,num 0])))
           (let* ((lines (apply #'process-lines cmd))
                  (files (seq-filter #'file-readable-p lines)))
             (blc-notmuch--log-vars lines files)
             (progress-reporter-done reporter)
             files))
        (error
         (nnheader-report 'nnir "Couldn't run blc-notmuch: %s" (cadr err)))))))

(provide 'blc-notmuch)

;; Local Variables:
;; byte-compile-dynamic: t
;; End:

;;; blc-notmuch.el ends here
