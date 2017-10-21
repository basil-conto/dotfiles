;;; magit.el --- magit init file for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://github.com/basil-conto/dotfiles

;;; Commentary:

;; The GPG header code is adapted from the GitHub Gist by fice-t at
;; https://gist.github.com/fice-t/c84c3bc7007d0d4bcacfeb2c0e42ac27.

;;; Code:

(require 'map)
(require 'package)
(eval-and-compile
  (unless package--initialized
    (package-initialize)))

(require 'magit)
(require 'magit-git)
(require 'magit-section)

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
  "Insert GPG information about REV into revision buffer.
Adapted from URL `https://gist.github.com/fice-t/\
c84c3bc7007d0d4bcacfeb2c0e42ac27'."
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

;;; magit.el ends here
