;;; blc-backup.el --- backup routines for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://gitlab.com/basil-conto/dotfiles

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

(require 'blc-lib)

(autoload 'comint-carriage-motion "comint")

(defvar blc-backup-dir "~/Backup/"
  "Directory containing backup-related files.")

(defvar blc-backup-dpkg-file (blc-file blc-backup-dir "dpkg-selections.txt")
  "File to write dpkgs selections to in `blc-backup-dpkg'.")

(defvar blc-backup-etc-file (blc-file blc-backup-dir "etckeeper.tar.gz")
  "Name of etckeeper archive to use in `blc-backup-etc'.")

(defvar blc-backup-index-file (blc-file blc-backup-dir "index.txt")
  "File listing files to back up in `blc-backup-rsync'.")

(defvar blc-backup-hook
  (list #'blc-backup-dpkg #'blc-backup-etc #'blc-backup-rsync)
  "List of backup functions to call in `blc-backup'.")

(defun blc-backup-dpkg ()
  "Back up dpkg selections to `blc-backup-dpkg-file'."
  (with-temp-file blc-backup-dpkg-file
    (let ((ret (call-process "dpkg" nil t nil "--get-selections")))
      (or (eq ret 0) (error "dpkg exited with status %s" ret)))))

(defun blc-backup-etc ()
  "Back up etckeeper to `blc-backup-etc-file' via git-archive."
  (let* ((default-directory "/sudo::/etc")
         (ret (process-file "git" nil nil nil "archive"
                            "-o" blc-backup-etc-file "master")))
    (or (eq ret 0) (error "git-archive exited with status %s" ret))))

(defun blc-backup--filter (proc str)
  "Process filter which uses `comint-carriage-motion'."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          (let ((start (goto-char (marker-position (process-mark proc)))))
            (insert-before-markers str)
            (comint-carriage-motion start (point))))
        (if moving (goto-char (process-mark proc)))))))

(defun blc-backup-rsync ()
  "Back up files in `blc-backup-index-file' via rsync."
  (let* ((default-directory "~/")
         (dest (read-shell-command "rsync destination: ")))
    (make-process :name "blc-backup-rsync"
                  :buffer (pop-to-buffer "*blc-backup-rsync*")
                  :command (list "rsync" "-ahrvP"
                                 (concat "--files-from=" blc-backup-index-file)
                                 "/home/blc" dest)
                  :connection-type 'pipe
                  :filter #'blc-backup--filter)))

(defun blc-backup ()
  "Sequentially run backup functions in `blc-backup-hook'."
  (interactive)
  (run-hooks 'blc-backup-hook))

(provide 'blc-backup)

;;; blc-backup.el ends here
