;;; blc-magit.el --- magit init file for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://github.com/basil-conto/dotfiles

;;; Commentary:

;; The GPG header code is adapted from the GitHub Gist by fice-t at
;; https://gist.github.com/fice-t/c84c3bc7007d0d4bcacfeb2c0e42ac27.

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

(require 'blc-lib)
(require 'blc-pkg)

(require 'magit)
(require 'magit-git)
(require 'magit-section)

(require 'map)
(eval-when-compile
  (require 'subr-x))

(defvar blc-magit-gpg-types
  '((?G "VALID"       magit-signature-good)
    (?B "BAD"         magit-signature-bad)
    (?U "UNKNOWN"     magit-signature-untrusted)
    (?X "EXPIRED"     magit-signature-expired)
    (?Y "EXPIRED KEY" magit-signature-expired-key)
    (?R "REVOKED"     magit-signature-revoked)
    (?E "ERROR"       magit-signature-error))
  "Map GPG sign types to their description and face.")

(defun blc-magit-insert-revision-gpg (rev)
  "Insert GPG information about REV into revision buffer."
  (pcase-let* ((`(,(app string-to-char type) ,signer ,key
                  ,(app (pcase--flip string-trim-right "\n+") raw))
                (with-temp-buffer
                  (magit-rev-insert-format "%G?%x00%GS%x00%GK%x00%GG" rev)
                  (split-string (buffer-string) "\0")))
               (`(,status ,face) (map-elt blc-magit-gpg-types type)))
    (when (and status face)
      (magit-insert-section (gpg status (not (eq type ?E)))
        (insert "GPG Status: "
                (propertize (or status "") 'face face)
                " (press "
                (substitute-command-keys "\\[magit-section-toggle]")
                " to toggle raw output)\n")
        (unless (string-empty-p signer)
          (magit-insert-section (gpg signer)
            (insert "GPG Signer: " (propertize signer 'face face) ?\n)))
        (unless (string-empty-p key)
          (magit-insert-section (gpg key)
            (insert "GPG Key:    " (propertize key 'face face) ?\n)))
        (insert ?\n)
        (magit-insert-heading)
        (magit-insert-section (gpg raw)
          (insert (propertize raw 'face face) "\n\n"))))))

;; Add signature revision headers
(magit-add-section-hook 'magit-revision-sections-hook
                        #'blc-magit-insert-revision-gpg
                        #'magit-insert-revision-headers
                        t)

;; Always highlight tabs
(blc-put magit-diff-highlight-indentation "" 'tabs)

;; Status buffer
(dolist (fn '(magit-insert-repo-header magit-insert-remote-header))
  (magit-add-section-hook
   'magit-status-headers-hook fn 'magit-insert-head-branch-header))

;; Repo list: insert dirty column in third position
(push (list "D" 1 #'magit-repolist-column-dirty ())
      (nthcdr 2 magit-repolist-columns))

;; Default arguments
(map-do #'add-to-list
        '((magit-log-arguments    . "--show-signature")
          (magit-merge-arguments  . "--ff-only"       )
          (magit-rebase-arguments . "--interactive"   )))

;; Limit number of commits in log
(let (case-fold-search)
  (setq-default
   magit-log-arguments
   (blc-sed-tree (rx "-n" (group (+ digit))) "64" magit-log-arguments t t 1)))

;; Modes
(magit-wip-after-apply-mode)
(magit-wip-after-save-mode)
(magit-wip-before-change-mode)

(provide 'blc-magit)

;;; blc-magit.el ends here
