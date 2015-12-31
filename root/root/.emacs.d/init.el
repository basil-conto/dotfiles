;;; ============
;;; Key bindings
;;; ============

;;; Transpose window split
(defun transpose-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2)
    (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window)
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically))
    (switch-to-buffer nil)))

(cl-loop
 for (k . v)
 in '(;; Murder
      ("C-k"         . kill-whole-line)
      ("C-x C-k"     .       kill-line)
      ;; Windows
      ("C-x 4"       . transpose-split)
      ;; Buffers
      ("S-<prior>"   . previous-buffer)
      ("S-<next>"    .     next-buffer)
      ;; Mutatis mutandis within tmux
      ("M-[ 5 ; 2 ~" . previous-buffer)
      ("M-[ 6 ; 2 ~" .     next-buffer))
 do (global-set-key (kbd k) v))

;;; =========
;;; Scrolling
;;; =========

(setq
 ;; One line at a time
 mouse-wheel-scroll-amount       '(1 ((shift) . 1))
 scroll-conservatively           10000
 scroll-error-top-bottom         t
 scroll-preserve-screen-position t
 scroll-step                     1)

;;; =======
;;; General
;;; =======

(load-theme 'tango-dark)

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono 9"))

(setq
 font-lock-maximum-decoration 2
 jit-lock-stealth-time        4
 inhibit-startup-screen       1
 uniquify-buffer-name-style   'forward)

(    blink-cursor-mode -1)
(   column-number-mode   )
(delete-selection-mode   )
(      show-paren-mode   )
(        tool-bar-mode -1)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;;; ===========
;;; Indentation
;;; ===========

(setq-default
 indent-tabs-mode  nil
 tab-always-indent t
 tab-width         2)

(setq indent-line-function 'insert-tab)

(defun fix-electric-indent ()
  (electric-indent-local-mode -1))

;;; ======
;;; Backup
;;; ======

(setq
 ;; Don't clobber symlinks
 backup-by-copying      t
 ;; Backup directory
 backup-directory-alist '(("." . "~/.backup/"))
 delete-old-versions    t
 kept-new-versions      6
 kept-old-versions      2
 ;; Versioned backups
 version-control        t)

;;; ================
;;; Package settings
;;; ================

;; apt-sources

(eval-when-compile (require 'apt-sources))
(add-hook 'apt-sources-mode-hook 'fix-electric-indent)

;; bytecomp

(autoload 'byte-compile-file "bytecomp" nil t)
(global-set-key (kbd "C-c c") 'byte-compile-file)

;; conf-mode

(add-hook 'conf-mode-hook 'fix-electric-indent)

;; ido

(eval-when-compile (require 'ido))
(ido-mode)
(setq ido-enable-flex-matching 1)

;; linum

(eval-when-compile (require 'linum))
(global-linum-mode)
(setq linum-format
      (lambda (line)
        (let ((w (length (number-to-string (count-lines (point-min)
                                                        (point-max))))))
          (propertize (format (format "%%%dd\u2502" w) line) 'face 'linum))))

;; lisp-mode

(add-hook 'lisp-mode-hook 'fix-electric-indent)

;; sh-script

(eval-when-compile (require 'sh-script))
(setq sh-basic-offset 2
      sh-indentation  2)

;; whitespace

(eval-when-compile (require 'whitespace))
(global-whitespace-mode)
(setq whitespace-style
      '(face tabs trailing empty tab-mark lines-tail))

;; windmove

(cl-loop
 for (k . v)
 in '(("S-<up>"      . windmove-up   )
      ("S-<down>"    . windmove-down )
      ("S-<left>"    . windmove-left )
      ("S-<right>"   . windmove-right)
      ;; Mutatis mutandis within tmux
      ("M-[ 1 ; 2 A" . windmove-up   )
      ("M-[ 1 ; 2 B" . windmove-down )
      ("M-[ 1 ; 2 D" . windmove-left )
      ("M-[ 1 ; 2 C" . windmove-right))
 do (global-set-key (kbd k) v))
