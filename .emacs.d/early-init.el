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
    "Increase GC threshold to at least 64MiB"
    (setq gc-cons-threshold (max (ash 1 26) gc-cons-threshold)))

  (blc-gc-thresh-maximise))

(defun blc-turn-on-xterm-mouse (&optional frame)
  "Conditionally enable `xterm-mouse-mode' on FRAME.
Enable the mode only if FRAME is the first terminal frame
created.  FRAME defaults to the selected one."
  (or (display-graphic-p frame)
      (string-equal "initial_terminal" (terminal-name (frame-terminal frame)))
      xterm-mouse-mode
      (xterm-mouse-mode)))

(setq-default package-enable-at-startup nil)

(add-hook 'after-make-frame-functions #'blc-turn-on-xterm-mouse)

;;; early-init.el ends here
