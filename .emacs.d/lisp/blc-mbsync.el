;;; blc-mbsync.el --- mbsync conveniences for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil@contovou.net>
;; Homepage: https://git.sr.ht/~blc/dotfiles

;;; Commentary:

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

(require 'blc-lib)

(require 'map)
(require 'seq)
(eval-when-compile
  (require 'subr-x)
  (require 'thunk))

(defalias 'blc-mbsync-chandirs
  (thunk-delay
   (blc-with-contents "~/.mbsyncrc"
     (let (chandirs)
       (while (blc-search-forward (rx bol "MaildirStore"))
         (when (blc-search-forward
                (rx bol "Path" (+ space) (group (+ (not space))) eol))
           (let ((dir (match-string-no-properties 1)))
             (push (cons (file-name-nondirectory (directory-file-name dir))
                         (expand-file-name dir))
                   chandirs))))
       (nreverse chandirs))))
  "Thunk with alist of channels to maildirs in ~/.mbsyncrc.")

(defun blc--mbsync-crm (prompt)
  "Read multiple mbsync channels with PROMPT.
Candidates offered are the keys of `blc-mbsync-chandirs', as well
as the catch-all value `--all', making them all valid arguments
for the mbsync executable."
  (let ((chans (cons "--all" (map-keys (blc-mbsync-chandirs)))))
    (completing-read-multiple
     prompt chans nil 'confirm nil 'blc-mbsync-history chans)))

(defun blc--mbsync-chans-to-dirs (&rest chans)
  "Translate mbsync CHANS to maildirs.
See `blc--mbsync-crm' for valid CHANS."
  (let ((chandirs (blc-mbsync-chandirs)))
    (blc-keep (apply-partially #'blc-get chandirs)
              (if (member "--all" chans)
                  (map-keys chandirs)
                chans))))

(defun blc--mbsync-folders (&rest chans)
  "Return subdirectories of maildirs of mbsync CHANS.
See `blc--mbsync-crm' for valid CHANS."
  (mapcan (lambda (dir)
            (seq-filter #'file-accessible-directory-p
                        (directory-files dir t
                                         directory-files-no-dot-files-regexp)))
          (apply #'blc--mbsync-chans-to-dirs chans)))

(defun blc--mbsync-uid (file)
  "Return maildir UID of FILE or nil."
  (and (string-match (rx ",U=" (group (+ digit))) file)
       (match-string 1 file)))

(defun blc--mbsync-rename (window alist)
  "Rename files according to ALIST with WINDOW selected.
Prompt for confirmation before proceeding with the (FROM . TO)
rename in ALIST.

This is a subroutine of `blc-mbsync-deduplicate' and intended as
the QUIT-FUNCTION of `with-temp-buffer-window' or similar."
  (with-selected-window window
    (if-let* ((n (length alist))
              ((> n 0))
              ((yes-or-no-p (format "Rename %d files? " n))))
        (dotimes-with-progress-reporter (i n)
            (format "Renaming %d files..." n)
          (pcase-let ((`(,from . ,to) (pop alist)))
            (rename-file from to)))
      (quit-window t))))

(defun blc-mbsync-deduplicate (&rest chans)
  "Deduplicate UIDs in all sub-maildirs of mbsync CHANS.
The user is shown a transcript of pending rename operations and
asked to confirm their execution.

See `blc--mbsync-crm' for valid CHANS."
  (interactive (blc--mbsync-crm "Deduplicate mbsync maildirs: "))
  (with-current-buffer-window
   "*blc-mbsync-dups*" () #'blc--mbsync-rename
   (let* ((folders  (apply #'blc--mbsync-folders chans))
          (nfolders (length folders))
          renames)
     (dotimes-with-progress-reporter (i nfolders (nreverse renames))
         (format "Scanning %d folders..." nfolders)
       (let ((map (make-hash-table :test #'equal)))
         (dolist (file (directory-files-recursively (pop folders) ""))
           (when-let* ((uid (blc--mbsync-uid file)))
             (let ((dups (gethash uid map)))
               (unless (member file dups)
                 (puthash uid (cons file dups) map)))))
         (maphash (lambda (_ dups)
                    (dolist (dup (butlast (sort dups #'file-newer-than-file-p)))
                      (let ((new (replace-regexp-in-string
                                  (rx ",U=" (+ (not ?,))) "" dup t t)))
                        (insert (format "Rename %s\n    -> %s\n" dup new))
                        (push (cons dup new) renames))))
                  map))))))

(defun blc--mbsync-maxuid (maildir)
  "Return max. valid UID in mbsync MAILDIR."
  (apply #'max 0 (blc-keep (lambda (file)
                             (and-let* ((uid (blc--mbsync-uid file)))
                               (string-to-number uid)))
                           (directory-files-recursively maildir ""))))

(defun blc-mbsync-maximise-uid (uid &rest chans)
  "Recalculate max. valid UID in mbsync CHANS if necessary.
Inform user and do nothing if multiple occurences of UID are
found."
  (interactive (cons (if current-prefix-arg
                         (prefix-numeric-value current-prefix-arg)
                       (read-number "Current max. valid UID: "))
                     (blc--mbsync-crm "Search mbsync maildirs: ")))

  (let ((uidre (rx bol (literal (number-to-string uid)) eol))
        files)
    (with-temp-buffer
      (dolist (dir (apply #'blc--mbsync-chans-to-dirs chans))
        (dolist (file (directory-files-recursively
                       dir (rx bos ".uidvalidity" eos)))
          (insert-file-contents file nil nil nil t)
          (save-excursion
            (forward-line)
            (when (looking-at-p uidre)
              (push file files))))))

    (pcase files
      ('()
       (user-error "Max. valid UID %d not found in maildirs %s"
                   uid (string-join chans " ")))

      (`(,file)
       (let ((maxuid (blc--mbsync-maxuid (file-name-directory file))))
         (cond ((>= uid maxuid)
                (message "Nothing to see here, folks"))
               ((yes-or-no-p
                 (format "Replace UID %d with %d in %s? " uid maxuid file))
                (with-temp-file file
                  (insert-file-contents file)
                  (forward-line)
                  (when (looking-at uidre)
                    (replace-match (number-to-string maxuid) t t)))))))

      (_
       (with-current-buffer-window
        "*blc-mbsync-maxuids*" () nil
        (insert (format "\
The following maildirs, along with their actual max. UID,
all share the same valid max. UID of %d:\n\n" uid))
        (save-excursion
          (dolist (file (nreverse files))
            (let ((dir (file-name-directory file)))
              (insert (format "%d %s\n" (blc--mbsync-maxuid dir) dir)))))
        (align-regexp (point) (point-max) (rx (group (+ space)))))))))

(defun blc-mbsync (&rest chans)
  "Call mbsync with CHANS asynchronously via a shell.
When called interactively, prompt the user with completion for
multiple channels to synchronise.  Otherwise, CHANS should form a
list of shell-quoted strings to pass to mbsync."
  (interactive (blc--mbsync-crm "Synchronise mbsync channels: "))
  (let ((cmd (string-join (cons "mbsync" chans) " ")))
    (async-shell-command cmd (format "*%s*" cmd))))

(defun blc-mbsync-all ()
  "Call `blc-mbsync' for each channel in `blc-mbsync-chandirs'."
  (interactive)
  (mapc #'blc-mbsync (map-keys (blc-mbsync-chandirs))))

(provide 'blc-mbsync)

;;; blc-mbsync.el ends here
