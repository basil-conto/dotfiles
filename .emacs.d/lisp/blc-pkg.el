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

(eval-when-compile
  (declare-function async-bytecomp-package-mode "ext:async-bytecomp"))

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
 package-selected-packages
 '(2048-game

   ace-window
   ag
   alert
   apache-mode
   apt-sources-list
   ascii-art-to-unicode
   async
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
   counsel
   counsel-gtags
   counsel-projectile
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
   embrace
   emms
   engine-mode
   enwc
   ess
   ewmctrl
   exec-path-from-shell
   expand-region
   eyebrowse

   fic-mode
   figlet
   fill-column-indicator
   fireplace
   flx
   free-keys

   ggtags
   ghc
   git-annex
   git-commit
   gitattributes-mode
   gitconfig-mode
   gitignore-mode
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
   projectile
   pulseaudio-control

   rainbow-mode
   regex-tool
   restclient

   sass-mode
   sicp
   skype
   sl
   slime-volleyball
   sr-speedbar
   sudoku
   swiper
   sx
   systemd

   threes
   tile
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

  (when (memq 'async missing)
    (setq missing (delq 'async missing))
    (blc-package-install 'async))

  (setq-default async-bytecomp-allowed-packages '(all))
  (condition-case err
      (async-bytecomp-package-mode)
    (error (lwarn 'blc :error "%S" err)))

  (mapc #'blc-package-install missing))

(provide 'blc-pkg)

;;; blc-pkg.el ends here
