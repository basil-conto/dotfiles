;;; blc-macs.el --- convenience macros for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://github.com/basil-conto/dotfiles

;;; Commentary:

;; TODO:
;; * Consider `define-inline'
;; * Reword plist `pcase-defmacro' docstring

;;; Code:

(require 'seq)
(eval-when-compile
  (require 'subr-x))

;;; Structural patterns

(pcase-defmacro npred (fun)
  "Build a negated `pred' `pcase' pattern.
That is, match when FUN applied to the object returns nil."
  (let ((obj (make-symbol "obj")))
    `(and ,obj (guard (not (thread-last ,obj ,fun))))))

(pcase-defmacro plist (&rest args)
  "Build a `pcase' pattern matching property list elements.
ARGS should itself form a plist whose values are bound to
elements of the `pcase' expression corresponding to its keys.
Lookup is performed using `plist-get'."
  `(and (pred seqp)
        ,@(mapcar (pcase-lambda ((seq key var))
                    `(app (pcase--flip plist-get ,key) ,var))
                  (seq-partition args 2))))

;;; Declarations

(defun blc--and-compile (exps)
  "Return equivalent to `(eval-and-compile ,@EXPS)."
  (and exps `(eval-and-compile ,@exps)))

(defun blc--when-compile (exps)
  "Return equivalent to `(eval-when-compile ,@EXPS)."
  (and exps `(eval-when-compile ,@exps)))

(defun blc--file-fns (gen &rest alist)
  "Transform ALIST as per `blc-declare-fns' using GEN."
  (mapcan (pcase-lambda (`(,feature . ,fns))
            (let ((file (prin1-to-string feature t)))
              (mapcar (lambda (fn)
                        (funcall gen fn file))
                      fns)))
          alist))

(defmacro blc-autoloads (&rest alist)
  "Autoload multiple functions.
See `blc-declare-fns' for the format of ALIST."
  (declare (indent 0))
  (blc--and-compile (apply #'blc--file-fns
                           (lambda (fn file)
                             `(autoload ',fn ,file))
                           alist)))

(defmacro blc-declare-fns (&rest alist)
  "Declare external functions to the byte-compiler.
Elements of ALIST should have the form (FILE . FNS), where FNS is
a list of functions defined in FILE, which in turn represents
either a filename, as accepted by `locate-library', or a
corresponding feature name."
  (declare (indent 0))
  (blc--when-compile (apply #'blc--file-fns
                            (lambda (fn file)
                              `(declare-function ,fn ,file))
                            alist)))

(defmacro blc-declare-vars (&rest symbols)
  "Declare SYMBOLS as variables using `defvar'."
  (declare (indent 0))
  (blc--when-compile (mapcar (lambda (sym)
                               `(defvar ,sym))
                             symbols)))

;;; Miscellanea

(defmacro blc-with-contents (pathexpr &rest body)
  "Evaluate BODY in a buffer with the contents of file PATHEXPR."
  (declare (indent 1))
  (macroexp-let2 nil path pathexpr
    `(when (file-readable-p ,path)
       (with-temp-buffer
         (insert-file-contents ,path)
         ,@body))))

(provide 'blc-macs)

;;; blc-macs.el ends here
