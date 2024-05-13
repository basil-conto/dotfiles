;;; blc-backup.el --- backup routines for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil@contovou.net>
;; Homepage: https://git.sr.ht/~blc/dotfiles

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

(require 'blc-lib)

(autoload 'comint-carriage-motion "comint")

(defvar blc-backup-dir "~/Backup/"
  "Directory containing backup-related files.")

(defvar blc-backup-dpkg-file (blc-file blc-backup-dir "dpkg-selections.txt.gz")
  "File to write dpkgs selections to in `blc-backup-dpkg'.")

(defvar blc-backup-etc-file (blc-file blc-backup-dir "etckeeper.tar.gz")
  "Name of etckeeper archive to use in `blc-backup-etc'.")

(defvar blc-backup-opt-file (blc-file blc-backup-dir "opt.txt.gz")
  "File to write /opt directory listing to in `blc-backup-opt'.")

(defvar blc-backup-cabal-file (blc-file blc-backup-dir "cabal-install.txt.gz")
  "File to write cabal package list to in `blc-backup-cabal'.")

(defvar blc-backup-cargo-file (blc-file blc-backup-dir "cargo-install.txt.gz")
  "File to write cargo crate list to in `blc-backup-cargo'.")

(defvar blc-backup-go-file (blc-file blc-backup-dir "go-install.txt.gz")
  "File to write go package list to in `blc-backup-go'.")

(defvar blc-backup-luarocks-file
  (blc-file blc-backup-dir "luarocks-install.txt.gz")
  "File to write luarocks list to in `blc-backup-luarocks'.")

(defvar blc-backup-npm-file (blc-file blc-backup-dir "npm-install.txt.gz")
  "File to write npm package list to in `blc-backup-npm'.")

(defvar blc-backup-opam-file (blc-file blc-backup-dir "opam-install.txt.gz")
  "File to write opam package list to in `blc-backup-opam'.")

(defvar blc-backup-snap-file (blc-file blc-backup-dir "snap-install.txt.gz")
  "File to write snaps list to in `blc-backup-snap'.")

(defvar blc-backup-index-file (blc-file blc-backup-dir "rsync-index.txt")
  "File listing files to back up in `blc-backup-rsync'.")

(defvar blc-backup-exclude-file (blc-file blc-backup-dir "rsync-exclude.txt")
  "File listing files to back up in `blc-backup-rsync'.")

(defvar blc-backup-hook
  (list #'blc-backup-dpkg
        #'blc-backup-etc
        #'blc-backup-opt
        #'blc-backup-cabal
        #'blc-backup-cargo
        #'blc-backup-go
        #'blc-backup-luarocks
        #'blc-backup-npm
        #'blc-backup-opam
        #'blc-backup-snap
        #'blc-backup-rsync)
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

(defun blc-backup-opt ()
  "Back up directory list under /opt to `blc-backup-opt-file'."
  (with-temp-file blc-backup-opt-file
    (let ((ret (call-process "find" nil t nil
                             "/opt" "-maxdepth" "2" "-type" "d")))
      (or (eq ret 0) (error "find exited with status %s" ret)))))

(defun blc-backup-cabal ()
  "Back up cabal package list to `blc-backup-cabal-file'."
  (with-temp-file blc-backup-cabal-file
    (let ((ret (call-process "cabal" nil t nil "list" "--installed")))
      (or (eq ret 0) (error "cabal exited with status %s" ret)))))

(defun blc-backup-cargo ()
  "Back up cargo crate list to `blc-backup-cargo-file'."
  (with-temp-file blc-backup-cargo-file
    (let ((ret (call-process "cargo" nil t nil "install" "--list")))
      (or (eq ret 0) (error "cargo exited with status %s" ret)))))

(defun blc-backup-go ()
  "Back up go packages to `blc-backup-go-file'."
  (with-temp-file blc-backup-go-file
    (let ((ret (call-process "find" nil t nil (expand-file-name "~/go/bin"))))
      (or (eq ret 0) (error "find exited with status %s" ret)))))

(defun blc-backup-luarocks ()
  "Back up luarocks list to `blc-backup-luarocks-file'."
  (with-temp-file blc-backup-luarocks-file
    (let ((ret (call-process "luarocks" nil t nil "list")))
      (or (eq ret 0) (error "luarocks exited with status %s" ret)))))

(defun blc-backup-npm ()
  "Back up npm package list to `blc-backup-npm-file'."
  (with-temp-file blc-backup-npm-file
    (let ((ret (call-process "npm" nil t nil "--global" "ls")))
      (or (eq ret 0) (error "npm exited with status %s" ret)))))

(defun blc-backup-opam ()
  "Back up opam package list to `blc-backup-opam-file'."
  (with-temp-file blc-backup-opam-file
    (let ((ret (call-process "opam" nil t nil "list")))
      (or (eq ret 0) (error "opam exited with status %s" ret)))))

(defun blc-backup-snap ()
  "Back up snaps list to `blc-backup-snap-file'."
  (with-temp-file blc-backup-snap-file
    (let ((ret (call-process "snap" nil t nil "list")))
      (or (eq ret 0) (error "snap exited with status %s" ret)))))

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
  (let* ((default-directory (expand-file-name "~/"))
         (dest (read-shell-command "rsync destination: ")))
    (make-process :name "blc-backup-rsync"
                  :buffer (pop-to-buffer "*blc-backup-rsync*")
                  :command
                  (list "rsync" "-ahrvP"
                        (concat "--files-from=" blc-backup-index-file)
                        (concat "--exclude-from=" blc-backup-exclude-file)
                        (expand-file-name "~") dest)
                  :connection-type 'pipe
                  :filter #'blc-backup--filter)))

(defun blc-backup ()
  "Sequentially run backup functions in `blc-backup-hook'."
  (interactive)
  (run-hooks 'blc-backup-hook))

(provide 'blc-backup)

;;; blc-backup.el ends here
