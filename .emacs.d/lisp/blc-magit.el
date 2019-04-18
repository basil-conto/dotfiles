;;; blc-magit.el --- magit init file for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://gitlab.com/basil-conto/dotfiles

;;; Commentary:

;; The GPG header code is adapted from the GitHub Gist by fice-t at
;; <https://gist.github.com/fice-t/c84c3bc7007d0d4bcacfeb2c0e42ac27>.

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

(require 'blc-lib)
(require 'blc-pkg)

(require 'forge)
(require 'magit)
(require 'magit-extras)
(require 'magit-git)
(require 'magit-section)

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

(defun blc-magit-insert-revision-gpg ()
  "Insert GPG information into revision buffer."
  (pcase-let* ((rev magit-buffer-revision)
               (`(,(app string-to-char type) ,signer ,key
                  ,(app (pcase--flip string-trim-right "\n+") raw))
                (with-temp-buffer
                  (magit-rev-insert-format "%G?%x00%GS%x00%GK%x00%GG" rev)
                  (split-string (buffer-string) "\0")))
               (`(,status ,face) (alist-get type blc-magit-gpg-types)))
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

(defun blc-magit-insert-emacs-revision ()
  "Insert the head of `magit-revision-stack' before point.
Format the Git revision as per CONTRIBUTE guidelines."
  (interactive)
  (let ((process-environment (cons "TZ=UTC" process-environment))
        (default-directory   (or (cadar magit-revision-stack)
                                 (user-error "Revision stack is empty"))))
    (and (eq 0 (call-process "git" nil t t "show" "--quiet" "--date=iso-local"
                             "--format=%aI!%aE" (caar magit-revision-stack)))
         (bolp)
         (delete-char -1))))

;; Add signature revision headers
(magit-add-section-hook 'magit-revision-sections-hook
                        #'blc-magit-insert-revision-gpg
                        #'magit-insert-revision-headers
                        t)

;; Always highlight tabs
(blc-put* magit-diff-highlight-indentation "" 'tabs)

;; Status buffer
(dolist (fn '(magit-insert-repo-header magit-insert-remote-header))
  (magit-add-section-hook
   'magit-status-headers-hook fn 'magit-insert-head-branch-header))

;; Repo list: insert dirty column in third position
(push (list "D" 1 #'magit-repolist-column-dirty ())
      (nthcdr 2 magit-repolist-columns))

;; Set default log arguments
(mapatoms (lambda (sym)
            (when-let (args (get sym 'magit-log-default-arguments))
              (put sym 'magit-log-default-arguments
                   `("-n64" "--show-signature" ,@args)))))

;; Modes
(magit-wip-after-apply-mode)
(magit-wip-after-save-mode)
(magit-wip-before-change-mode)

(provide 'blc-magit)

;;; blc-magit.el ends here
