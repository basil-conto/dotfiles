;;; blc-pkg.el --- package.el init file for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil@contovou.net>
;; Homepage: https://git.sr.ht/~blc/dotfiles

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

(require 'blc-lib)

(require 'package)

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
             (float-time (time-since start)))))

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
                        (sort (mapcar #'car package-alist) :in-place t)
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

(setq-default
 ;; abbrev
 abbrev-file-name               (blc-file blc-index-dir "abbrevs.el")

 ;; emms
 emms-directory                 (blc-dir blc-index-dir "emms")

 ;; nsm
 nsm-settings-file              (blc-file blc-index-dir "network-security.eld")

 ;; srecode
 srecode-map-save-file          nil

 ;; transient
 transient-history-file         (blc-file blc-index-dir "transient-history.el")
 transient-levels-file          (blc-file blc-index-dir "transient-levels.el")
 transient-values-file          (blc-file blc-index-dir "transient-values.el")

 ;; url
 url-configuration-directory    (blc-dir blc-index-dir "url")

 ;; package
 package-archives
 '(("gnu"    . "https://elpa.gnu.org/devel/")
   ("nongnu" . "https://elpa.nongnu.org/nongnu-devel/")
   ("melpa"  . "https://melpa.org/packages/"))
 package-archive-priorities     (seq-map-indexed (lambda (a i) (cons (car a) i))
                                                 (reverse package-archives))
 package-menu-hide-low-priority t
 package-pinned-packages        '((json-mode        . "gnu")
                                  (slime-volleyball . "gnu"))
 package-quickstart-file        (blc-file blc-index-dir "package-quickstart.el")
 package-review-policy          t
 package-selected-packages
 '(2048-game

   academic-phrases
   adoc-mode
   ag
   apache-mode
   apt-sources-list
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
   cider
   clojure-mode
   cmake-mode
   company-coq
   counsel
   crontab-mode
   csharp-mode
   csv-mode

   dap-mode
   dape
   dash
   debbugs
   debian-el
   deadgrep
   define-word
   delight
   dockerfile-mode
   doric-themes
   ducpel

   ef-themes
   eglot-inactive-regions
   el-search
   elisp-benchmarks
   emacsql
   emms
   engine-mode
   enwc
   ess
   expand-region

   figlet
   firefox-javascript-repl
   fireplace
   flame
   font-lock-profiler
   font-lock-studio
   forge

   ghub
   gif-screencast
   git-annex
   git-modes
   gnus-desktop-notify
   go-mode
   graphviz-dot-mode
   gscholar-bibtex

   hacker-typer
   hackernews
   haskell-mode
   haskell-ts-mode
   helm-make
   highlight-escape-sequences
   highlight-refontification
   htmlize

   ialign
   idris-mode
   inf-clojure
   ivy
   ivy-bibtex
   ivy-hydra
   ivy-pass

   jinx
   jq-mode
   js2-mode
   js2-refactor
   json-mode

   keycast
   know-your-http-well

   latex-table-wizard
   ledger-mode
   logview
   lorem-ipsum
   lsp-metals
   lsp-mode
   lua-mode
   lyrics

   macrostep
   magit
   magit-annex
   magit-tbdiff
   markdown-mode
   mathjax
   matlab-mode
   merlin
   mines

   neato-graph-bar
   nginx-mode
   nix-mode
   nodejs-repl
   nov

   org-contrib
   org-modern
   org-pomodoro
   org-tree-slide
   org-tree-slide-pauses
   osm

   pacmacs
   paren-face
   pass
   pdf-tools
   plz-see
   proof-general
   pulseaudio-control

   racket-mode
   rainbow-mode
   rbit
   redtick
   relint
   restclient
   rfc-mode
   rg
   rmsbolt
   rust-mode

   scala-mode
   scanner
   scribble-mode
   sed-mode
   show-font
   sicp
   sl
   slime-volleyball
   sly
   sudoku
   swiper
   systemd

   threes
   tuareg
   typit

   unfill
   urgrep
   url-http-oauth

   visual-fill-column

   web-mode

   xr
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
  (and (memq 'pdf-tools missing)
       (fboundp 'pdf-tools-install)
       (pdf-tools-install t t t))
  (package--quickstart-maybe-refresh))

;; Afford shadowing installed packages
(eval-and-compile
  (dolist (dir `(("lisp/org" ,blc-dataroot-dir)
                 ("lisp"     ,user-emacs-directory)))
    (setq dir (apply #'expand-file-name dir))
    (setq load-path (cons dir (delete dir load-path)))))

(provide 'blc-pkg)

;;; blc-pkg.el ends here
