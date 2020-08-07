;;; blc-pkg.el --- package.el init file for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://gitlab.com/basil-conto/dotfiles

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

(require 'blc-lib)

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
    (:around (&rest args) blc-turn-off-hooks)
  "Disable `emacs-lisp-mode' hooks and report progress."
  (let (prog-mode-hook
        emacs-lisp-mode-hook
        (start (current-time))
        (journo (make-progress-reporter
                 "Refreshing `package-quickstart-file'..." 0 0)))
    (apply args)
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
                        (sort (mapcar #'car package-alist) #'string-lessp)
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
                  (blc-put* package-archives           id url)
                  (blc-put* package-archive-priorities id (1+ i)))
                '(("melpa" . "https://melpa.org/packages/")
                  ("org"   . "http://orgmode.org/elpa/")))

(setq-default
 ;; abbrev
 abbrev-file-name               (blc-file blc-index-dir "abbrevs.el")

 ;; emms
 emms-directory                 (blc-dir blc-index-dir "emms")

 ;; nsm
 nsm-settings-file              (blc-file blc-index-dir "network-security.el")

 ;; srecode
 srecode-map-save-file          nil

 ;; transient
 transient-history-file         (blc-file blc-index-dir "transient-history.el")
 transient-levels-file          (blc-file blc-index-dir "transient-levels.el")
 transient-values-file          (blc-file blc-index-dir "transient-values.el")

 ;; url
 url-configuration-directory    (blc-dir blc-index-dir "url")

 ;; package
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
   avy

   babel
   bbdb
   biblio
   bison-mode
   bluetooth
   bog
   bongo
   boogie-friends

   chess
   cmake-mode
   counsel
   counsel-gtags
   crontab-mode
   csharp-mode
   csv-mode

   dash
   debbugs
   debian-el
   debpaste
   deadgrep
   define-word
   delight
   dictionary
   disaster
   discover-my-major
   dropbox
   ducpel

   eglot
   el-search
   elisp-benchmarks
   emms
   engine-mode
   enwc
   ess
   ewmctrl
   expand-region

   figlet
   fireplace
   flame
   forge
   free-keys

   ggtags
   ghub
   gif-screencast
   git-annex
   git-commit
   gitattributes-mode
   gitconfig-mode
   gitignore-mode
   gnus-desktop-notify
   go-mode
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
   ivy-pass

   jq-mode
   js2-mode
   js2-refactor
   json-mode

   know-your-http-well

   ledger-mode
   logview
   lorem-ipsum
   lyrics

   macrostep
   magit
   magit-annex
   magit-libgit
   markdown-mode
   matlab-mode
   mines
   modus-operandi-theme
   mustache-mode

   neato-graph-bar
   nodejs-repl
   nov

   org-mime
   org-pdftools
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

   racket-mode
   rainbow-mode
   restclient
   rg
   rmsbolt

   scanner
   scribble-mode
   sicp
   skype
   sl
   slime-volleyball
   sudoku
   swiper
   sx
   systemd

   threes
   tuareg
   typit

   unfill

   visual-fill-column

   w3m
   web-mode
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
  (dolist (dir `(("lisp/org" ,blc-dataroot-dir)
                 ("lisp"     ,user-emacs-directory)))
    (setq dir (apply #'expand-file-name dir))
    (setq load-path (cons dir (delete dir load-path)))))

(provide 'blc-pkg)

;;; blc-pkg.el ends here
