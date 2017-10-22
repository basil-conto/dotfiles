;;; blc-pass.el --- faster, simpler auth-source-pass -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://github.com/basil-conto/dotfiles

;;; Commentary:

;; This package is a clean-slate POC attempt at improving the built-in
;; lisp/auth-source-pass.el package. Currently most of the
;; functionality is supported at one-third the search time cost.

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

(require 'blc-lib)
(eval-when-compile
  (require 'blc-macs))

(require 'auth-source)
(require 'seq)
(eval-when-compile
  (require 'subr-x))

(defvar blc-pass-dir (or (getenv "PASSWORD_STORE_DIR")
                         "~/.password-store")
  "Location of password-store directory.")

(defun blc-pass--extractor (key &rest vals)
  "Return a regexp matching KEY: VALS password-store entries.
Both KEY and VALS can be any valid `rx' form. KEY need not end in
a trailing colon, as that is already provided. When non-nil, VALS
are spliced into an `or' form. The resulting regexp contains one
capturing group around the actual value found. "
  (blc-rx
   `(: bol ,key ?: (* space) (group ,(if vals `(| ,@vals) '(+ word))) eol)))

(defun blc-pass--extract (regexp)
  "Return first match group of REGEXP in current buffer.
Does not move point."
  (save-excursion
    (and (blc-search-forward regexp)
         (match-string 1))))

(defun blc-pass-search (&rest spec)
  "Search password-store according to SPEC.
See `auth-source-search' for a description of the plist SPEC.
Supported keywords are :host, :user, :port, :max and :require.
Values of :host consitute the base name of GPG files to search."
  (pcase-let*
      (((plist :host (app blc-as-list hosts) :require (app blc-as-list requires)
               :user (app blc-as-list users) :port    (app blc-as-list ports)
               :max  max)
        spec)
       ((and (app (memq :user  ) req-user)
             (app (memq :port  ) req-port)
             (app (memq :secret) req-sec ))
        requires)
       (users (apply #'blc-pass--extractor '(: "user" (? "name")) users))
       (ports (apply #'blc-pass--extractor "port" ports))
       (files (directory-files-recursively
               blc-pass-dir
               (blc-rx `(: ,@(and hosts `(bos (| ,@hosts))) ".gpg" eos))))
       (count 0)
       (results))
    (while (and (< count max) files)
      (let ((file (pop files)))
        (blc-with-contents file
          (end-of-line)
          (let ((host (file-name-sans-extension
                       (file-relative-name file blc-pass-dir)))
                (user (blc-pass--extract users))
                (port (blc-pass--extract ports))
                (sec  (and-let* (((not (bolp)))
                                 (sec (buffer-substring (point-min) (point))))
                        (lambda () sec)))
                result)
            (when (seq-every-p
                   (pcase-lambda (`[,key ,val ,req])
                     (or (and val (setq result (plist-put result key val)))
                         (not req)))
                   `([:host ,host       nil]
                     [:user ,user ,req-user]
                     [:user ,port ,req-port]
                     [:secret ,sec ,req-sec]))
              (push result results)
              (setq count (1+ count)))))))
    results))

(defvar blc-pass-backend-type 'blc-pass
  "The `auth-source-backend' :type of `blc-pass-backend'.")

(defvar blc-pass-backend
  (auth-source-backend :type            blc-pass-backend-type
                       :source          blc-pass-dir
                       :search-function #'blc-pass-search)
  "Instance of `auth-source-backend' for `blc-pass'.")

(defun blc-pass-backend-parse (source)
  "Return `blc-pass' backend if SOURCE is `blc-pass'."
  (and (eq source blc-pass-backend-type)
       blc-pass-backend))

(provide 'blc-pass)

;; Local Variables:
;; byte-compile-dynamic: t
;; End:

;;; blc-pass.el ends here
