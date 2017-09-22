;;; blc-macs.el --- convenience macros for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://github.com/basil-conto/dotfiles

;;; Commentary:

;; TODO:
;; * Consider `define-inline'
;; * Reword plist `pcase-defmacro' docstring

;;; Code:
(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))
(require 'blc-lib)

(require 'map)
(require 'seq)
(eval-when-compile
  (require 'subr-x))

;;; Structural patterns

(eval-and-compile

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
                    (seq-partition args 2)))))

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

;;; Hooks

(defmacro blc-hook (&rest plists)
  "Add functions to hooks using `add-hook'.
Elements of PLISTS should form a plist with the following
recognised keys corresponding to the arguments of `add-hook':

:hooks HOOKS -- HOOKS is either a single hook variable or a list
thereof.

:fns FNS -- FNS is either a single function to be added to HOOKS
or a list thereof.

:append APPEND -- See `add-hook'.

:local LOCAL -- See `add-hook'."
  (declare (indent 0))
  (macroexp-progn
   (mapcan (pcase-lambda ((plist :hooks  hooks  :fns   fns
                                 :append append :local local))
             (mapcan (lambda (hook)
                       (mapcar (lambda (fn)
                                 `(add-hook ',hook #',fn ,append ,local))
                               (blc-as-list fns)))
                     (blc-as-list hooks)))
           plists)))

;;; Keys

(defmacro blc-define-keys (&rest alist)
  "Bind multiple keys per multiple keymaps.
Elements of ALIST should have the form (KEYMAP . BINDINGS), where
KEYMAP is an expression evaluating to a keymap. For each element
of the alist BINDINGS of the form (KEY . DEF), `define-key' is
called on KEYMAP, KEY and DEF."
  (declare (indent 0))
  (macroexp-progn
   (map-apply (lambda (map bindings)
                (macroexp-let2 nil map map
                  (macroexp-progn
                   (map-apply (lambda (key def)
                                `(define-key ,map ,key ,def))
                              bindings))))
              alist)))

;; Files & buffers

(defmacro blc-with-contents (path &rest body)
  "Evaluate BODY in a buffer with the contents of file PATH."
  (declare (indent 1))
  (macroexp-let2 nil path path
    `(when (file-readable-p ,path)
       (with-temp-buffer
         (insert-file-contents ,path)
         ,@body))))

(provide 'blc-macs)

;;; blc-macs.el ends here
