;;; blc-lib-tests.el --- tests for blc-lib.el -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://github.com/basil-conto/dotfiles

;;; Commentary:

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))
(require 'blc-lib)

(require 'ert)

(ert-deftest blc-path-lessp-relative ()
  "Test `blc-path-lessp' with relative paths."
  (should (eq (blc-path-lessp "" "") nil))
  (should (eq (blc-path-lessp "" "lisp") t))
  (should (eq (blc-path-lessp "" "lisp/") t))
  (should (eq (blc-path-lessp "lisp" "") nil))
  (should (eq (blc-path-lessp "lisp/" "") nil))

  (should (eq (blc-path-lessp "lisp" "lisp") nil))
  (should (eq (blc-path-lessp "lisp" "lisp/") t))
  (should (eq (blc-path-lessp "lisp/" "lisp") nil))
  (should (eq (blc-path-lessp "lisp/" "lisp/") nil))

  (should (eq (blc-path-lessp "lisp" "test") t))
  (should (eq (blc-path-lessp "lisp" "test/") t))
  (should (eq (blc-path-lessp "lisp/" "test") nil))
  (should (eq (blc-path-lessp "lisp/" "test/") t))

  (should (eq (blc-path-lessp "test" "lisp") nil))
  (should (eq (blc-path-lessp "test" "lisp/") t))
  (should (eq (blc-path-lessp "test/" "lisp") nil))
  (should (eq (blc-path-lessp "test/" "lisp/") nil))

  (should (eq (blc-path-lessp "lisp/cedet" "test") nil))
  (should (eq (blc-path-lessp "lisp/cedet" "test/") t))
  (should (eq (blc-path-lessp "lisp/cedet/" "test") nil))
  (should (eq (blc-path-lessp "lisp/cedet/" "test/") t))

  (should (eq (blc-path-lessp "test" "lisp/cedet") t))
  (should (eq (blc-path-lessp "test" "lisp/cedet/") t))
  (should (eq (blc-path-lessp "test/" "lisp/cedet") nil))
  (should (eq (blc-path-lessp "test/" "lisp/cedet/") nil))

  (should (eq (blc-path-lessp "lisp/cedet" "lisp/simple") t))
  (should (eq (blc-path-lessp "lisp/cedet" "lisp/simple/") t))
  (should (eq (blc-path-lessp "lisp/cedet/" "lisp/simple") nil))
  (should (eq (blc-path-lessp "lisp/cedet/" "lisp/simple/") t))

  (should (eq (blc-path-lessp "lisp/simple" "lisp/cedet") nil))
  (should (eq (blc-path-lessp "lisp/simple" "lisp/cedet/") t))
  (should (eq (blc-path-lessp "lisp/simple/" "lisp/cedet") nil))
  (should (eq (blc-path-lessp "lisp/simple/" "lisp/cedet/") nil))

  (should (eq (blc-path-lessp "lisp/cedet/ede" "lisp/simple") nil))
  (should (eq (blc-path-lessp "lisp/cedet/ede" "lisp/simple/") t))
  (should (eq (blc-path-lessp "lisp/cedet/ede/" "lisp/simple") nil))
  (should (eq (blc-path-lessp "lisp/cedet/ede/" "lisp/simple/") t))

  (should (eq (blc-path-lessp "lisp/simple" "lisp/cedet/ede") t))
  (should (eq (blc-path-lessp "lisp/simple" "lisp/cedet/ede/") t))
  (should (eq (blc-path-lessp "lisp/simple/" "lisp/cedet/ede") nil))
  (should (eq (blc-path-lessp "lisp/simple/" "lisp/cedet/ede/") nil)))

(ert-deftest blc-path-lessp-absolute ()
  "Test `blc-path-lessp' with absolute paths."
  (should (eq (blc-path-lessp "/" "/") nil))
  (should (eq (blc-path-lessp "/" "/etc") t))
  (should (eq (blc-path-lessp "/" "/etc/") t))
  (should (eq (blc-path-lessp "/etc" "/") nil))
  (should (eq (blc-path-lessp "/etc/" "/") nil))

  (should (eq (blc-path-lessp "/" "~") t))
  (should (eq (blc-path-lessp "/" "~/") t))
  (should (eq (blc-path-lessp "/" "~/foo") t))
  (should (eq (blc-path-lessp "/" "~/foo/") t))
  (should (eq (blc-path-lessp "~" "/") nil))
  (should (eq (blc-path-lessp "~/" "/") nil))
  (should (eq (blc-path-lessp "~/foo" "/") nil))
  (should (eq (blc-path-lessp "~/foo/" "/") nil))

  (should (eq (blc-path-lessp "/etc" "/usr") t))
  (should (eq (blc-path-lessp "/etc" "/usr/") t))
  (should (eq (blc-path-lessp "/etc/" "/usr") nil))
  (should (eq (blc-path-lessp "/etc/" "/usr/") t))

  (should (eq (blc-path-lessp "/usr" "/etc") nil))
  (should (eq (blc-path-lessp "/usr" "/etc/") t))
  (should (eq (blc-path-lessp "/usr/" "/etc") nil))
  (should (eq (blc-path-lessp "/usr/" "/etc/") nil))

  (should (eq (blc-path-lessp "~" "~") nil))
  (should (eq (blc-path-lessp "~" "~/") t))
  (should (eq (blc-path-lessp "~" "~/foo") t))
  (should (eq (blc-path-lessp "~" "~/foo/") t))
  (should (eq (blc-path-lessp "~/" "~") nil))
  (should (eq (blc-path-lessp "~/foo" "~") nil))
  (should (eq (blc-path-lessp "~/foo/" "~") nil))

  (should (eq (blc-path-lessp "~/foo" "~/bar") nil))
  (should (eq (blc-path-lessp "~/foo" "~/bar/") t))
  (should (eq (blc-path-lessp "~/foo/" "~/bar") nil))
  (should (eq (blc-path-lessp "~/foo/" "~/bar/") nil))

  (should (eq (blc-path-lessp "~/bar" "~/foo") t))
  (should (eq (blc-path-lessp "~/bar" "~/foo/") t))
  (should (eq (blc-path-lessp "~/bar/" "~/foo") nil))
  (should (eq (blc-path-lessp "~/bar/" "~/foo/") t)))

(ert-deftest blc-path-lessp-absolute-gnu-linux ()
  "Test `blc-path-lessp' with absolute paths on GNU/Linux."
  (skip-unless (eq system-type 'gnu/linux))
  (should (eq (blc-path-lessp "/etc" "~") t))
  (should (eq (blc-path-lessp "/etc" "~/") t))
  (should (eq (blc-path-lessp "/etc" "~/foo") t))
  (should (eq (blc-path-lessp "/etc" "~/foo/") t))
  (should (eq (blc-path-lessp "~" "/etc") nil))
  (should (eq (blc-path-lessp "~/" "/etc") nil))
  (should (eq (blc-path-lessp "~/foo" "/etc") nil))
  (should (eq (blc-path-lessp "~/foo/" "/etc") nil))

  (should (eq (blc-path-lessp "/etc/" "~") t))
  (should (eq (blc-path-lessp "/etc/" "~/") t))
  (should (eq (blc-path-lessp "/etc/" "~/foo") t))
  (should (eq (blc-path-lessp "/etc/" "~/foo/") t))
  (should (eq (blc-path-lessp "~" "/etc/") nil))
  (should (eq (blc-path-lessp "~/" "/etc/") nil))
  (should (eq (blc-path-lessp "~/foo" "/etc/") nil))
  (should (eq (blc-path-lessp "~/foo/" "/etc/") nil))

  (should (eq (blc-path-lessp "/tmp/" "~") nil))
  (should (eq (blc-path-lessp "/tmp/" "~/") nil))
  (should (eq (blc-path-lessp "/tmp/" "~/foo") nil))
  (should (eq (blc-path-lessp "/tmp/" "~/foo/") nil))
  (should (eq (blc-path-lessp "~" "/tmp/") t))
  (should (eq (blc-path-lessp "~/" "/tmp/") t))
  (should (eq (blc-path-lessp "~/foo" "/tmp/") t))
  (should (eq (blc-path-lessp "~/foo/" "/tmp/") t)))

(ert-deftest blc-path-lessp-mixed ()
  "Test `blc-path-lessp' mixing relative and absolute paths."
  (should (eq (blc-path-lessp "" "/") t))
  (should (eq (blc-path-lessp "" "/etc") t))
  (should (eq (blc-path-lessp "" "/etc/") t))
  (should (eq (blc-path-lessp "/" "") nil))
  (should (eq (blc-path-lessp "/etc" "") nil))
  (should (eq (blc-path-lessp "/etc/" "") nil))

  (should (eq (blc-path-lessp "" "~") t))
  (should (eq (blc-path-lessp "" "~/") t))
  (should (eq (blc-path-lessp "" "~/foo") t))
  (should (eq (blc-path-lessp "" "~/foo/") t))
  (should (eq (blc-path-lessp "~" "") nil))
  (should (eq (blc-path-lessp "~/" "") nil))
  (should (eq (blc-path-lessp "~/foo" "") nil))
  (should (eq (blc-path-lessp "~/foo/" "") nil))

  (should (eq (blc-path-lessp "lisp" "/") nil))
  (should (eq (blc-path-lessp "lisp/" "/") nil))
  (should (eq (blc-path-lessp "lisp/simple" "/") nil))
  (should (eq (blc-path-lessp "/" "lisp") t))
  (should (eq (blc-path-lessp "/" "lisp/") t))
  (should (eq (blc-path-lessp "/" "lisp/simple") t))

  (should (eq (blc-path-lessp "lisp" "/etc") nil))
  (should (eq (blc-path-lessp "/etc" "lisp") t))
  (should (eq (blc-path-lessp "lisp/" "/etc/") nil))
  (should (eq (blc-path-lessp "/etc/" "lisp/") t))
  (should (eq (blc-path-lessp "lisp/simple" "/etc/") nil))
  (should (eq (blc-path-lessp "/etc/" "lisp/simple") t)))

(provide 'blc-lib-tests)

;;; blc-lib-tests.el ends here
