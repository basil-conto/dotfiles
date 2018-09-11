;;; blc-pass.el --- faster, simpler auth-source-pass -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://gitlab.com/basil-conto/dotfiles

;;; Commentary:

;; This package is a clean-slate POC attempt at improving the built-in
;; lisp/auth-source-pass.el package. Currently most of the
;; functionality is supported at one-third the search time cost.

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

(require 'blc-lib)

(require 'auth-source)
(require 'map)
(require 'seq)
(eval-when-compile
  (require 'subr-x))

(defvar blc-pass-dir (or (getenv "PASSWORD_STORE_DIR")
                         "~/.password-store")
  "Location of password-store directory.")

(defun blc-pass--get (spec key)
  "Return value(s) associated with KEY in plist SPEC as a list."
  (blc-as-list (plist-get spec key)))

(defun blc-pass--files (spec)
  "Return `blc-pass-dir' files matching plist SPEC's :host."
  (let ((hosts (blc-pass--get spec :host)))
    (directory-files-recursively
     blc-pass-dir
     (blc-rx `(: ,@(and hosts `(bos (| ,@hosts))) ".gpg" eos)))))

(defun blc-pass--field (spec key &optional field)
  "Return a matcher object for KEY according to plist SPEC.
FIELD is a regexp in `rx' sexp form. The object returned is of
the form [KEY TEST REQUIRED]. TEST is a function which returns a
FIELD: VALUE entry corresponding to KEY from the current buffer
or nil if not found. REQUIRED non-nil means TEST should return
non-nil."
  (let ((vals (and field (blc-pass--get spec key))))
    (vector key
            (if field
                (let ((re (blc-rx
                           `(: bol ,field ?: (* space)
                               (group ,(if vals `(| ,@vals) '(+ (not space))))
                               eol))))
                  (lambda ()
                    (and (save-excursion (blc-search-forward re))
                         (match-string 1))))
              (lambda ()
                (unless (bobp)
                  (blc-const (buffer-substring (point-min) (point))))))
            (or vals (memq key (blc-pass--get spec :require))))))

(defun blc-pass--test (spec)
  "Return a function testing buffer contents against plist SPEC.
The returned predicate returns non-nil when called in a matching
buffer; in particular, it returns any supported key-value entries
found in the buffer as an alist."
  (let ((tests (map-apply (apply-partially #'blc-pass--field spec)
                          '((:secret . ())
                            (:user   . (: "user" (? "name")))
                            (:port   . "port")))))
    (lambda ()
      (end-of-line)
      (let (props)
        (and (seq-every-p (pcase-lambda (`[,key ,test ,req])
                            (if-let ((val (funcall test)))
                                (push (cons key val) props)
                              (not req)))
                          tests)
             (or props t))))))

(defun blc-pass-search (&rest spec)
  "Search password-store according to SPEC.
See `auth-source-search' for a description of the plist SPEC.
Supported keywords are :host, :user, :port, :max and :require.
Values of :host consitute the base name of GPG files to search."
  (let* ((spec  (or spec (list :type 'blc-pass))) ; Non-empty for `plist-put'
         (max   (plist-get spec :max))
         (files (blc-pass--files spec))
         (test  (blc-pass--test  spec))
         tokens)
    (while (and (> max 0) files)
      (let ((file (pop files)))
        (blc-with-contents file
          (when-let* ((matches (funcall test))
                      (token   (copy-sequence spec)))
            (when (consp matches)
              (map-do (apply-partially #'plist-put token) matches))
            (plist-put token :host (file-name-sans-extension
                                    (file-relative-name file blc-pass-dir)))
            (push token tokens)
            (setq max (1- max))))))
    tokens))

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

;;; blc-pass.el ends here
