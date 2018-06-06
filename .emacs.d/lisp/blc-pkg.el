;;; blc-pkg.el --- package.el init file for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://github.com/basil-conto/dotfiles

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

(require 'blc-lib)

(require 'map)
(require 'package)
(require 'seq)
(eval-when-compile
  (require 'subr-x))

;; Sandbox this nuisance
(define-advice package--save-selected-packages
    (:override (&optional value) blc-no-save)
  "Like `package--save-selected-packages', but do not save."
  (when value
    (lwarn 'blc :debug "\
Old `package-selected-packages': %S
New `package-selected-packages': %S"
           package-selected-packages
           (setq package-selected-packages value))))

(define-advice package-quickstart-refresh
    (:around (fn &rest args) blc-turn-off-hooks)
  "Disable `emacs-lisp-mode' hooks and report progress."
  (let (prog-mode-hook
        emacs-lisp-mode-hook
        (start (current-time))
        (journo (make-progress-reporter
                 "Refreshing `package-quickstart-file'..." 0 0)))
    (apply fn args)
    (message "%sdone (%.3fs)"
             (aref (cdr journo) 3)
             (float-time (time-subtract nil start)))))

(define-advice package-menu-execute
    (:after-while (&rest _) blc-quickstart-refresh)
  "Refresh `package-quickstart-file' after menu transactions."
  (package-quickstart-refresh))

(defun blc-package-install (pkg)
  "Install and do not select PKG with demoted errors."
  (condition-case err
      (package-install pkg t)
    (error (lwarn 'blc :error "%S" err))))

(defun blc-package-dir (pkg)
  "Return installation directory of external PKG or nil."
  (and-let* ((dsc (cadr (assoc-string pkg package-alist)))
             (dir (package-desc-dir dsc))
             ((stringp dir)))
    dir))

(defun blc--package-read-dir ()
  "Read a package name and return its directory.
Return `package-user-dir' if no directory is found."
  (or (blc-package-dir
       (completing-read "Package name: "
                        (sort (map-keys package-alist) #'string-lessp)
                        nil t nil 'blc-package-history))
      package-user-dir))

(defun blc-package-find (pkg)
  "Visit installation directory of PKG name.
Visit `package-user-dir' if such a directory is not found."
  (interactive (list (blc--package-read-dir)))
  (find-file pkg))

(defun blc-package-find-other-window (pkg)
  "Like `blc-package-find', but use another window."
  (interactive (list (blc--package-read-dir)))
  (find-file-other-window pkg))

(defun blc-package-find-other-frame (pkg)
  "Like `blc-package-find', but use another frame."
  (interactive (list (blc--package-read-dir)))
  (find-file-other-frame pkg))

;; Archives
(seq-do-indexed (pcase-lambda (`(,id . ,url) i)
                  (blc-put package-archives           id url)
                  (blc-put package-archive-priorities id (1+ i)))
                '(("melpa" . "https://melpa.org/packages/")
                  ("org"   . "http://orgmode.org/elpa/")))

(setq-default
 package-menu-hide-low-priority t
 package-quickstart-file        (blc-file blc-index-dir "package-quickstart.el")
 package-selected-packages
 '(2048-game

   academic-phrases
   ace-window
   ag
   alert
   apache-mode
   apt-sources-list
   ascii-art-to-unicode
   atomic-chrome
   auctex
   auctex-latexmk
   auth-password-store
   avy

   babel
   bbdb
   better-shell
   bibtex-utils
   bison-mode
   bongo
   boogie-friends

   chess
   cmake-mode
   counsel-gtags
   csharp-mode
   cssh
   csv-mode

   dash
   debbugs
   debpaste
   define-word
   delight
   dictionary
   disaster
   discover-my-major
   dropbox
   ducpel

   ebib
   el-search
   embrace
   emms
   engine-mode
   enwc
   ess
   ewmctrl
   exec-path-from-shell
   expand-region
   eyebrowse

   figlet
   fireplace
   flame
   flx
   free-keys

   ggtags
   ghc
   ghub
   git-annex
   git-commit
   gitattributes-mode
   gitconfig-mode
   gitignore-mode
   glab
   gnus-desktop-notify
   google-contacts
   google-maps
   google-this
   gscholar-bibtex

   hacker-typer
   hackernews
   haskell-mode
   hayoo
   helm-make
   highlight-escape-sequences
   htmlize

   ialign
   idris-mode
   interleave
   irony
   irony-eldoc
   ivy
   ivy-bibtex
   ivy-hydra
   ivy-pages
   ivy-pass

   jq-mode
   js2-mode
   js2-refactor
   json-mode

   know-your-http-well

   latex-extra
   ledger-mode
   list-unicode-display
   logview
   lorem-ipsum
   lyrics

   macrostep
   magit
   magit-annex
   markdown-mode
   matlab-mode
   mines
   minimap
   mustache-mode

   neato-graph-bar
   nodejs-repl

   org-mime
   org-pdfview
   org-plus-contrib
   org-pomodoro

   pacmacs
   paren-face
   pass
   passmm
   pdf-tools
   playerctl
   pomidor
   pulseaudio-control

   rainbow-mode
   regex-tool
   restclient

   sass-mode
   sicp
   skype
   sl
   slime-volleyball
   solarized-theme
   sr-speedbar
   sudoku
   sx
   systemd

   threes
   tuareg
   typit

   unfill

   visual-fill-column

   w3m
   web-mode
   wgrep
   wiki-summary
   with-editor
   writeroom-mode
   wttrin

   xref-js2

   yaml-mode))

;; Locate and activate installed packages
(package-initialize)

;; Install missing packages
(when-let*
    ((missing (seq-remove #'package-installed-p package-selected-packages))
     ((y-or-n-p-with-timeout (format "Install %d missing packages? "
                                     (length missing))
                             5 t)))
  (package-refresh-contents)
  (mapc #'blc-package-install missing)
  (package--quickstart-maybe-refresh))

;; Afford shadowing installed packages
(eval-and-compile
  (let ((dir (expand-file-name "lisp" user-emacs-directory)))
    (setq load-path (cons dir (delete dir load-path)))))

(provide 'blc-pkg)

;;; blc-pkg.el ends here
