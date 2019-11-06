;;; early-init.el --- early init file for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://gitlab.com/basil-conto/dotfiles

;;; Code:

(defalias 'blc-gc-thresh-restore
  (let ((thresh gc-cons-threshold))
    (lambda () (setq gc-cons-threshold thresh)))
  "Restore default `gc-cons-threshold' value.
See URL `http://bling.github.io/blog/2016/01/18/\
why-are-you-changing-gc-cons-threshold/'.")

(eval-and-compile
  (defun blc-gc-thresh-maximise ()
    "Increase GC threshold to at least 64MiB."
    (setq gc-cons-threshold (max (ash 1 26) gc-cons-threshold)))

  (blc-gc-thresh-maximise))

(setq-default package-enable-at-startup nil)

(add-hook 'window-setup-hook #'blc-gc-thresh-restore t)

;;; early-init.el ends here
