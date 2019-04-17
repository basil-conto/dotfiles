;;; blc-lib-tests.el --- tests for blc-lib.el -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://gitlab.com/basil-conto/dotfiles

;;; Commentary:

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))
(require 'blc-lib)

(require 'ert)

(ert-deftest blc-tree-map ()
  "Test `blc-tree-map' behaviour."
  (should-not    (blc-tree-map #'1+ ()))
  (should (equal (blc-tree-map #'1+ 0) 1))
  (should (equal (blc-tree-map #'1+ '(0 . 1)) '(1 . 2)))
  (should (equal (blc-tree-map #'1+ '(0 ((1 2) . 3) ())) '(1 ((2 3) . 4) ()))))

(ert-deftest blc-file-lessp-relative ()
  "Test `blc-file-lessp' with relative paths."
  (should-not (blc-file-lessp "" ""))
  (should     (blc-file-lessp "" "lisp"))
  (should     (blc-file-lessp "" "lisp/"))
  (should-not (blc-file-lessp "lisp" ""))
  (should-not (blc-file-lessp "lisp/" ""))

  (should-not (blc-file-lessp "lisp" "lisp"))
  (should     (blc-file-lessp "lisp" "lisp/"))
  (should-not (blc-file-lessp "lisp/" "lisp"))
  (should-not (blc-file-lessp "lisp/" "lisp/"))

  (should     (blc-file-lessp "lisp" "test"))
  (should     (blc-file-lessp "lisp" "test/"))
  (should-not (blc-file-lessp "lisp/" "test"))
  (should     (blc-file-lessp "lisp/" "test/"))

  (should-not (blc-file-lessp "test" "lisp"))
  (should     (blc-file-lessp "test" "lisp/"))
  (should-not (blc-file-lessp "test/" "lisp"))
  (should-not (blc-file-lessp "test/" "lisp/"))

  (should-not (blc-file-lessp "lisp/cedet" "test"))
  (should     (blc-file-lessp "lisp/cedet" "test/"))
  (should-not (blc-file-lessp "lisp/cedet/" "test"))
  (should     (blc-file-lessp "lisp/cedet/" "test/"))

  (should     (blc-file-lessp "test" "lisp/cedet"))
  (should     (blc-file-lessp "test" "lisp/cedet/"))
  (should-not (blc-file-lessp "test/" "lisp/cedet"))
  (should-not (blc-file-lessp "test/" "lisp/cedet/"))

  (should     (blc-file-lessp "lisp/cedet" "lisp/simple"))
  (should     (blc-file-lessp "lisp/cedet" "lisp/simple/"))
  (should-not (blc-file-lessp "lisp/cedet/" "lisp/simple"))
  (should     (blc-file-lessp "lisp/cedet/" "lisp/simple/"))

  (should-not (blc-file-lessp "lisp/simple" "lisp/cedet"))
  (should     (blc-file-lessp "lisp/simple" "lisp/cedet/"))
  (should-not (blc-file-lessp "lisp/simple/" "lisp/cedet"))
  (should-not (blc-file-lessp "lisp/simple/" "lisp/cedet/"))

  (should-not (blc-file-lessp "lisp/cedet/ede" "lisp/simple"))
  (should     (blc-file-lessp "lisp/cedet/ede" "lisp/simple/"))
  (should-not (blc-file-lessp "lisp/cedet/ede/" "lisp/simple"))
  (should     (blc-file-lessp "lisp/cedet/ede/" "lisp/simple/"))

  (should     (blc-file-lessp "lisp/simple" "lisp/cedet/ede"))
  (should     (blc-file-lessp "lisp/simple" "lisp/cedet/ede/"))
  (should-not (blc-file-lessp "lisp/simple/" "lisp/cedet/ede"))
  (should-not (blc-file-lessp "lisp/simple/" "lisp/cedet/ede/")))

(ert-deftest blc-file-lessp-absolute ()
  "Test `blc-file-lessp' with absolute paths."
  (should-not (blc-file-lessp "/" "/"))
  (should     (blc-file-lessp "/" "/etc"))
  (should     (blc-file-lessp "/" "/etc/"))
  (should-not (blc-file-lessp "/etc" "/"))
  (should-not (blc-file-lessp "/etc/" "/"))

  (should     (blc-file-lessp "/etc" "/usr"))
  (should     (blc-file-lessp "/etc" "/usr/"))
  (should-not (blc-file-lessp "/etc/" "/usr"))
  (should     (blc-file-lessp "/etc/" "/usr/"))

  (should-not (blc-file-lessp "/usr" "/etc"))
  (should     (blc-file-lessp "/usr" "/etc/"))
  (should-not (blc-file-lessp "/usr/" "/etc"))
  (should-not (blc-file-lessp "/usr/" "/etc/"))

  (should-not (blc-file-lessp "~" "~"))
  (should     (blc-file-lessp "~" "~/"))
  (should     (blc-file-lessp "~" "~/foo"))
  (should     (blc-file-lessp "~" "~/foo/"))
  (should-not (blc-file-lessp "~/" "~"))
  (should-not (blc-file-lessp "~/foo" "~"))
  (should-not (blc-file-lessp "~/foo/" "~"))

  (should-not (blc-file-lessp "~/foo" "~/bar"))
  (should     (blc-file-lessp "~/foo" "~/bar/"))
  (should-not (blc-file-lessp "~/foo/" "~/bar"))
  (should-not (blc-file-lessp "~/foo/" "~/bar/"))

  (should     (blc-file-lessp "~/bar" "~/foo"))
  (should     (blc-file-lessp "~/bar" "~/foo/"))
  (should-not (blc-file-lessp "~/bar/" "~/foo"))
  (should     (blc-file-lessp "~/bar/" "~/foo/")))

(ert-deftest blc-file-lessp-mixed ()
  "Test `blc-file-lessp' mixing relative and absolute paths."
  (should     (blc-file-lessp "" "/"))
  (should     (blc-file-lessp "" "/etc"))
  (should     (blc-file-lessp "" "/etc/"))
  (should-not (blc-file-lessp "/" ""))
  (should-not (blc-file-lessp "/etc" ""))
  (should-not (blc-file-lessp "/etc/" ""))

  (should     (blc-file-lessp "" "~"))
  (should     (blc-file-lessp "" "~/"))
  (should     (blc-file-lessp "" "~/foo"))
  (should     (blc-file-lessp "" "~/foo/"))
  (should-not (blc-file-lessp "~" ""))
  (should-not (blc-file-lessp "~/" ""))
  (should-not (blc-file-lessp "~/foo" ""))
  (should-not (blc-file-lessp "~/foo/" "")))

(provide 'blc-lib-tests)

;;; blc-lib-tests.el ends here
