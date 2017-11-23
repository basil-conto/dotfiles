;;; blc-dark-theme.el --- Zenburn-based theme for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://github.com/basil-conto/dotfiles

;;; Commentary:

;; This theme borrows heavily from zenburn-theme.el at
;; <https://github.com/bbatsov/zenburn-emacs>.

;;; Code:

(deftheme blc-dark
  "Zenburn-based theme for blc.")

(let* ((bg    "#3F3F3F")
       (bg+05 "#494949")
       (bg+1  "#4F4F4F")
       (bg+2  "#5F5F5F")
       (bg+3  "#6F6F6F")
       (bg-05 "#383838")
       (bg-1  "#2B2B2B")
       (bg-2  "#000000")
       (blu   "#8CD0D3")
       (blu+1 "#94BFF3")
       (blu-1 "#7CB8BB")
       (blu-2 "#6CA0A3")
       (blu-3 "#5C888B")
       (blu-4 "#4C7073")
       (blu-5 "#366060")
       (cyn   "#93E0E3")
       (fg    "#DCDCCC")
       (fg+1  "#FFFFEF")
       (fg-1  "#656555")
       (grn   "#7F9F7F")
       (grn+1 "#8FB28F")
       (grn+2 "#9FC59F")
       (grn+3 "#AFD8AF")
       (grn+4 "#BFEBBF")
       (grn-1 "#5F7F5F")
       (mgn   "#DC8CC3")
       (orn   "#DFAF8F")
       (red   "#CC9393")
       (red+1 "#DCA3A3")
       (red-1 "#BC8383")
       (red-2 "#AC7373")
       (red-3 "#9C6363")
       (red-4 "#8C5353")
       (ylw   "#F0DFAF")
       (ylw-1 "#E0CF9F")
       (ylw-2 "#D0BF8F")
       (box   (lambda (n) `(:box (:line-width ,n :style released-button))))
       (box-1 (funcall box -1)))

  (apply
   #'custom-theme-set-variables
   'blc-dark
   `(;; Built-in

     ;; ansi-color
     (ansi-color-names-vector [,bg ,red ,grn ,ylw ,blu ,mgn ,cyn ,fg])

     ;; gnus
     (gnus-logo-colors '(,blu-4 ,blu))

     ;; gnus-art
     (gnus-header-face-alist
      '(("Date"                  gnus-header-name font-lock-type-face)
        ("\\(?:Delivered-\\)?To" gnus-header-name font-lock-string-face)
        ("From"                  gnus-header-name gnus-header-from)
        ("Newsgroups:.*,"        gnus-header-name gnus-header-newsgroups)
        ("Subject"               gnus-header-name gnus-header-subject)
        (""                      gnus-header-name gnus-header-content)))

     ;; gnus-spec
     (gnus-face-2 'font-lock-keyword-face)
     (gnus-face-3 'font-lock-string-face )
     (gnus-face-4 'font-lock-comment-face)

     ;; org-faces
     (org-todo-keyword-faces '(("NEXT" . ,mgn  )
                               ("EXEC" . ,orn  )
                               ("MEET" . ,ylw  )
                               ("WAIT" . ,cyn  )
                               ("BALK" . ,fg   )
                               ("VOID" . ,blu-2)))

     ;; vc-annotate
     (vc-annotate-background     ,bg-1)
     (vc-annotate-color-map      '(( 20. . ,red-1)
                                   ( 40. . ,red  )
                                   ( 60. . ,orn  )
                                   ( 80. . ,ylw-2)
                                   (100. . ,ylw-1)
                                   (120. . ,ylw  )
                                   (140. . ,grn-1)
                                   (160. . ,grn  )
                                   (180. . ,grn+1)
                                   (200. . ,grn+2)
                                   (220. . ,grn+3)
                                   (240. . ,grn+4)
                                   (260. . ,cyn  )
                                   (280. . ,blu-2)
                                   (300. . ,blu-1)
                                   (320. . ,blu  )
                                   (340. . ,blu+1)
                                   (360. . ,mgn  )))
     (vc-annotate-very-old-color ,mgn)

     ;; External

     ;; fill-column-indicator
     (fci-rule-color ,bg+1)

     ;; pdf-view
     (pdf-view-midnight-colors '(,fg . ,bg-05))))

  (apply
   #'custom-theme-set-faces
   'blc-dark
   `(;; Built-in

     ;; button
     (button ((t :underline t)))

     ;; compile
     (compilation-mode-line-exit ((t :inherit compilation-info)))
     (compilation-mode-line-fail ((t :inherit compilation-error)))

     ;; diff-mode
     (diff-added          ((t :background "#335533" :foreground ,grn)))
     (diff-changed        ((t :background "#555511" :foreground ,ylw-1)))
     (diff-file-header    ((t :background ,bg+2     :foreground ,fg
                              :weight      bold)))
     (diff-header         ((t :background ,bg+2)))
     (diff-refine-added   ((t :background "#338833" :foreground ,grn+4)))
     (diff-refine-change  ((t :background "#888811" :foreground ,ylw)))
     (diff-refine-removed ((t :background "#883333" :foreground ,red)))
     (diff-removed        ((t :background "#553333" :foreground ,red-2)))

     ;; dired-async
     (dired-async-failures     ((t :foreground ,red :weight bold)))
     (dired-async-message      ((t :foreground ,ylw :weight bold)))
     (dired-async-mode-message ((t :foreground ,ylw)))

     ;; ediff-init
     (ediff-current-diff-A        ((t :background ,red-4 :foreground ,fg)))
     (ediff-current-diff-Ancestor ((t :background ,red-4 :foreground ,fg)))
     (ediff-current-diff-B        ((t :background ,grn-1 :foreground ,fg)))
     (ediff-current-diff-C        ((t :background ,blu-5 :foreground ,fg)))
     (ediff-even-diff-A           ((t :background ,bg+1)))
     (ediff-even-diff-Ancestor    ((t :inherit    ediff-even-diff-A)))
     (ediff-even-diff-B           ((t :inherit    ediff-even-diff-A)))
     (ediff-even-diff-C           ((t :inherit    ediff-even-diff-A)))
     (ediff-fine-diff-A           ((t :background ,red-2 :foreground ,fg
                                      :weight     bold)))
     (ediff-fine-diff-Ancestor    ((t :background ,red-2
                                      :inherit    ediff-fine-diff-A)))
     (ediff-fine-diff-B           ((t :background ,grn
                                      :inherit    ediff-fine-diff-A)))
     (ediff-fine-diff-C           ((t :background ,blu-3
                                      :inherit    ediff-fine-diff-A)))
     (ediff-odd-diff-A            ((t :background ,bg+2)))
     (ediff-odd-diff-Ancestor     ((t :inherit    ediff-odd-diff-A)))
     (ediff-odd-diff-B            ((t :inherit    ediff-odd-diff-A)))
     (ediff-odd-diff-C            ((t :inherit    ediff-odd-diff-A)))

     ;; em-ls
     (eshell-ls-archive    ((t :foreground ,red-1 :weight bold)))
     (eshell-ls-backup     ((t :inherit font-lock-comment-face)))
     (eshell-ls-clutter    ((t :inherit font-lock-comment-face)))
     (eshell-ls-directory  ((t :foreground ,blu+1 :weight bold)))
     (eshell-ls-executable ((t :foreground ,red+1 :weight bold)))
     (eshell-ls-missing    ((t :inherit font-lock-warning-face)))
     (eshell-ls-product    ((t :inherit font-lock-doc-face    )))
     (eshell-ls-special    ((t :foreground ,ylw   :weight bold)))
     (eshell-ls-symlink    ((t :foreground ,cyn   :weight bold)))
     (eshell-ls-unreadable ((t :foreground ,fg)))

     ;; em-prompt
     (eshell-prompt ((t :foreground ,ylw :weight bold)))

     ;; ert
     (ert-test-result-expected   ((t :background ,bg :foreground ,grn+4)))
     (ert-test-result-unexpected ((t :background ,bg :foreground ,red)))

     ;; eww
     (eww-invalid-certificate ((t :inherit error)))
     (eww-valid-certificate   ((t :inherit success)))

     ;; faces
     (cursor                     ((t :background ,fg+1)))
     (default                    ((t :background ,bg :foreground ,fg)))
     (error                      ((t :foreground ,red :weight bold)))
     (escape-glyph               ((t :foreground ,ylw :weight bold)))
     (fringe                     ((t :background ,bg+1 :foreground ,fg)))
     (header-line                ((t :background ,bg-1 :foreground ,ylw
                                     ,@box-1)))
     (highlight                  ((t :background ,bg-1)))
     (homoglyph                  ((t :inherit    escape-glyph)))
     (link                       ((t :foreground ,ylw
                                     :underline  t :weight bold)))
     (link-visited               ((t :foreground ,ylw-2 :underline t)))
     (menu                       ((t :inherit    default)))
     (minibuffer-prompt          ((t :foreground ,ylw)))
     (mode-line                  ((t :background ,bg-1 :foreground ,grn+1
                                     ,@box-1)))
     (mode-line-buffer-id        ((t :foreground ,ylw :weight bold)))
     (mode-line-highlight        ((t ,@(funcall box -2))))
     (mode-line-inactive         ((t :background ,bg-05 :foreground ,grn-1
                                     ,@box-1)))
     (nobreak-hyphen             ((t :inherit    escape-glyph)))
     (region                     ((t :background ,bg-1)))
     (secondary-selection        ((t :background ,bg+2)))
     (show-paren-match           ((t :background ,bg+3 :weight bold)))
     (show-paren-mismatch        ((t :foreground ,red+1
                                     :inherit    show-paren-match)))
     (success                    ((t :foreground ,grn :weight bold)))
     (trailing-whitespace        ((t :background ,red)))
     (tty-menu-disabled-face     ((t :foreground ,bg+3
                                     :inherit    tty-menu-enabled-face)))
     (tty-menu-enabled-face      ((t :background ,bg+1)))
     (tty-menu-selected-face     ((t :background ,bg)))
     (vertical-border            ((t :foreground ,fg)))
     (warning                    ((t :foreground ,orn :weight bold)))
     (window-divider             ((t :foreground ,bg+3)))
     (window-divider-first-pixel ((t :inherit    window-divider)))
     (window-divider-last-pixel  ((t :inherit    window-divider)))

     ;; font-lock
     (font-lock-builtin-face              ((t :foreground ,fg    :weight bold)))
     (font-lock-comment-delimiter-face    ((t :foreground ,grn-1             )))
     (font-lock-comment-face              ((t :foreground ,grn               )))
     (font-lock-constant-face             ((t :foreground ,grn+4             )))
     (font-lock-doc-face                  ((t :foreground ,grn+2             )))
     (font-lock-function-name-face        ((t :foreground ,cyn               )))
     (font-lock-keyword-face              ((t :foreground ,ylw   :weight bold)))
     (font-lock-negation-char-face        ((t :foreground ,ylw   :weight bold)))
     (font-lock-preprocessor-face         ((t :foreground ,blu+1             )))
     (font-lock-regexp-grouping-backslash ((t :foreground ,grn   :weight bold)))
     (font-lock-regexp-grouping-construct ((t :foreground ,ylw   :weight bold)))
     (font-lock-string-face               ((t :foreground ,red               )))
     (font-lock-type-face                 ((t :foreground ,blu-1             )))
     (font-lock-variable-name-face        ((t :foreground ,orn               )))
     (font-lock-warning-face              ((t :foreground ,ylw-2 :weight bold)))

     ;; gnus
     (gnus-group-mail-1           ((t :inherit    gnus-group-mail-1-empty
                                      :weight     bold)))
     (gnus-group-mail-1-empty     ((t :inherit    gnus-group-news-1-empty)))
     (gnus-group-mail-2           ((t :inherit    gnus-group-mail-2-empty
                                      :weight     bold)))
     (gnus-group-mail-2-empty     ((t :inherit    gnus-group-news-2-empty)))
     (gnus-group-mail-3           ((t :inherit    gnus-group-mail-3-empty
                                      :weight     bold)))
     (gnus-group-mail-3-empty     ((t :inherit    gnus-group-news-3-empty)))
     (gnus-group-mail-4           ((t :inherit    gnus-group-mail-4-empty
                                      :weight     bold)))
     (gnus-group-mail-4-empty     ((t :inherit    gnus-group-news-4-empty)))
     (gnus-group-mail-5           ((t :inherit    gnus-group-mail-5-empty
                                      :weight     bold)))
     (gnus-group-mail-5-empty     ((t :inherit    gnus-group-news-5-empty)))
     (gnus-group-mail-6           ((t :inherit    gnus-group-mail-6-empty
                                      :weight     bold)))
     (gnus-group-mail-6-empty     ((t :inherit    gnus-group-news-6-empty)))
     (gnus-group-mail-low         ((t :inherit    gnus-group-mail-low-empty
                                      :weight     bold)))
     (gnus-group-mail-low-empty   ((t :inherit    gnus-group-news-low-empty)))
     (gnus-group-news-1           ((t :inherit    gnus-group-news-1-empty
                                      :weight     bold)))
     (gnus-group-news-1-empty     ((t :foreground ,ylw)))
     (gnus-group-news-2           ((t :inherit    gnus-group-news-2-empty
                                      :weight     bold)))
     (gnus-group-news-2-empty     ((t :foreground ,grn+3)))
     (gnus-group-news-3           ((t :inherit    gnus-group-news-3-empty
                                      :weight     bold)))
     (gnus-group-news-3-empty     ((t :foreground ,grn+1)))
     (gnus-group-news-4           ((t :inherit    gnus-group-news-4-empty
                                      :weight     bold)))
     (gnus-group-news-4-empty     ((t :foreground ,blu-2)))
     (gnus-group-news-5           ((t :inherit    gnus-group-news-5-empty
                                      :weight     bold)))
     (gnus-group-news-5-empty     ((t :foreground ,blu-3)))
     (gnus-group-news-6           ((t :inherit    gnus-group-news-6-empty
                                      :weight     bold)))
     (gnus-group-news-6-empty     ((t :foreground ,bg+2)))
     (gnus-group-news-low         ((t :inherit    gnus-group-news-low-empty
                                      :weight     bold)))
     (gnus-group-news-low-empty   ((t :foreground ,bg+2)))
     (gnus-summary-cancelled      ((t :foreground ,orn)))
     (gnus-summary-high-ancient   ((t :foreground ,blu)))
     (gnus-summary-high-read      ((t :foreground ,grn :weight bold)))
     (gnus-summary-high-ticked    ((t :foreground ,orn :weight bold)))
     (gnus-summary-high-unread    ((t :foreground ,fg  :weight bold)))
     (gnus-summary-low-ancient    ((t :foreground ,blu)))
     (gnus-summary-low-read       ((t :foreground ,grn)))
     (gnus-summary-low-ticked     ((t :foreground ,orn :weight bold)))
     (gnus-summary-low-unread     ((t :foreground ,fg)))
     (gnus-summary-normal-ancient ((t :foreground ,blu)))
     (gnus-summary-normal-read    ((t :foreground ,grn)))
     (gnus-summary-normal-ticked  ((t :foreground ,orn :weight bold)))
     (gnus-summary-normal-unread  ((t :foreground ,fg)))
     (gnus-summary-selected       ((t :foreground ,ylw :weight bold)))

     ;; gnus-art
     (gnus-header-content    ((t :inherit    message-header-other  )))
     (gnus-header-from       ((t :inherit    message-header-to     )))
     (gnus-header-name       ((t :inherit    message-header-name   )))
     (gnus-header-newsgroups ((t :inherit    message-header-other  )))
     (gnus-header-subject    ((t :inherit    message-header-subject)))
     (gnus-signature         ((t :foreground ,ylw)))

     ;; gnus-cite
     (gnus-cite-1  ((t :foreground ,blu  )))
     (gnus-cite-10 ((t :foreground ,ylw-1)))
     (gnus-cite-11 ((t :foreground ,ylw  )))
     (gnus-cite-2  ((t :foreground ,blu-1)))
     (gnus-cite-3  ((t :foreground ,blu-2)))
     (gnus-cite-4  ((t :foreground ,grn+2)))
     (gnus-cite-5  ((t :foreground ,grn+1)))
     (gnus-cite-6  ((t :foreground ,grn  )))
     (gnus-cite-7  ((t :foreground ,red  )))
     (gnus-cite-8  ((t :foreground ,red-1)))
     (gnus-cite-9  ((t :foreground ,red-2)))

     ;; gnus-fun
     (gnus-x-face ((t :background ,fg :foreground ,bg)))

     ;; gnus-srvr
     (gnus-server-opened  ((t :foreground ,grn+2 :weight bold)))
     (gnus-server-denied  ((t :foreground ,red+1 :weight bold)))
     (gnus-server-closed  ((t :foreground ,blu   :slant  italic)))
     (gnus-server-offline ((t :foreground ,ylw   :weight bold)))
     (gnus-server-agent   ((t :foreground ,blu   :weight bold)))

     ;; hi-lock
     (hi-blue    ((t :background ,cyn   :foreground ,bg-1)))
     (hi-blue-b  ((t :foreground ,blu   :weight      bold)))
     (hi-green   ((t :background ,grn+4 :foreground ,bg-1)))
     (hi-green-b ((t :foreground ,grn+2 :weight      bold)))
     (hi-pink    ((t :background ,mgn   :foreground ,bg-1)))
     (hi-red-b   ((t :foreground ,red   :weight      bold)))
     (hi-yellow  ((t :background ,ylw   :foreground ,bg-1)))

     ;; ido
     (ido-first-match ((t :foreground ,ylw   :weight     bold)))
     (ido-indicator   ((t :background ,red-4 :foreground ,ylw)))
     (ido-only-match  ((t :foreground ,orn   :weight     bold)))
     (ido-subdir      ((t :foreground ,ylw)))

     ;; info
     (Info-quoted    ((t :inherit font-lock-function-name-face)))
     (info-menu-star ((t :foreground ,fg)))

     ;; isearch
     (isearch        ((t :background ,bg+2  :foreground ,ylw-2 :weight bold)))
     (isearch-fail   ((t :background ,red-4 :foreground ,fg)))
     (lazy-highlight ((t :background ,bg-05 :inherit isearch)))

     ;; man
     (Man-overstrike ((t :inherit font-lock-keyword-face)))
     (Man-underline  ((t :inherit font-lock-string-face :underline t)))

     ;; message
     (message-cited-text        ((t :inherit    font-lock-comment-face)))
     (message-header-cc         ((t :inherit    message-header-to)))
     (message-header-name       ((t :inherit    font-lock-builtin-face)))
     (message-header-newsgroups ((t :inherit    message-header-to)))
     (message-header-other      ((t :foreground ,grn)))
     (message-header-subject    ((t :foreground ,orn :weight bold)))
     (message-header-to         ((t :foreground ,ylw :weight bold)))
     (message-header-xheader    ((t :inherit    message-header-other)))
     (message-mml               ((t :inherit    message-header-to)))
     (message-separator         ((t :inherit    message-cited-text)))

     ;; minibuffer
     (completions-annotations ((t :foreground ,fg-1)))

     ;; mm-uu
     (mm-uu-extract ((t :background ,bg-05 :foreground ,grn+1)))

     ;; newst-reader
     (newsticker-enclosure-face ((t :foreground ,grn+3)))
     (newsticker-extra-face     ((t :foreground ,bg+2 :height 0.8)))
     (newsticker-feed-face      ((t :foreground ,fg)))

     ;; newst-plainview
     (newsticker-date-face          ((t :foreground ,fg  )))
     (newsticker-default-face       ((t :foreground ,fg  )))
     (newsticker-immortal-item-face ((t :foreground ,grn )))
     (newsticker-new-item-face      ((t :foreground ,blu )))
     (newsticker-obsolete-item-face ((t :foreground ,red )))
     (newsticker-old-item-face      ((t :foreground ,bg+3)))
     (newsticker-statistics-face    ((t :foreground ,fg  )))

     ;; newst-treeview
     (newsticker-treeview-face            ((t :foreground ,fg)))
     (newsticker-treeview-immortal-face   ((t :foreground ,grn)))
     (newsticker-treeview-listwindow-face ((t :foreground ,fg)))
     (newsticker-treeview-new-face        ((t :foreground ,blu :weight bold)))
     (newsticker-treeview-obsolete-face   ((t :foreground ,red)))
     (newsticker-treeview-old-face        ((t :foreground ,bg+3)))
     (newsticker-treeview-selection-face  ((t :foreground ,ylw
                                              :background ,bg-1)))

     ;; org-faces
     (org-agenda-date-today       ((t :foreground ,fg+1 :inherit bold-italic)))
     (org-agenda-dimmed-todo-face ((t :inherit shadow)))
     (org-agenda-done             ((t :inherit org-done :weight normal)))
     (org-agenda-restriction-lock ((t :inverse-video t)))
     (org-agenda-structure        ((t :inherit font-lock-comment-face)))
     (org-checkbox                ((t :background ,bg+2 :foreground ,fg+1
                                      ,@(funcall box 1))))
     (org-clock-overlay           ((t :inherit secondary-selection)))
     (org-column                  ((t :background ,bg-1)))
     (org-column-title            ((t :background ,bg-1 :underline t
                                      :weight bold)))
     (org-date                    ((t :foreground ,blu :underline t)))
     (org-date-selected           ((t :foreground ,red :inverse-video t)))
     (org-document-info           ((t :foreground ,blu)))
     (org-document-title          ((t :inherit org-document-info)))
     (org-done                    ((t :foreground ,grn+3 :weight bold)))
     (org-drawer                  ((t :inherit font-lock-function-name-face)))
     (org-ellipsis                ((t :foreground ,ylw-1 :underline t)))
     (org-footnote                ((t :foreground ,cyn :underline t)))
     (org-formula                 ((t :foreground ,ylw-2)))
     (org-headline-done           ((t :inherit org-agenda-done)))
     (org-hide                    ((t :foreground ,bg-1)))
     (org-latex-and-related       ((t :foreground ,orn)))
     (org-mode-line-clock         ((t :background ,bg-1 :foreground ,fg)))
     (org-mode-line-clock-overrun ((t :inherit    mode-line)))
     (org-scheduled               ((t :foreground ,grn+4)))
     (org-scheduled-previously    ((t :foreground ,red)))
     (org-scheduled-today         ((t :foreground ,blu+1)))
     (org-sexp-date               ((t :inherit org-date)))
     (org-special-keyword         ((t :inherit font-lock-comment-face)))
     (org-table                   ((t :foreground ,grn+2)))
     (org-time-grid               ((t :foreground ,ylw-2)))
     (org-todo                    ((t :foreground ,red :weight bold)))
     (org-upcoming-deadline       ((t :inherit font-lock-keyword-face)))

     ;; org-habit
     (org-habit-alert-face          ((t :background ,ylw-1 :foreground ,bg)))
     (org-habit-alert-future-face   ((t :background ,ylw-2 :foreground ,bg)))
     (org-habit-clear-face          ((t :background ,blu-3)))
     (org-habit-clear-future-face   ((t :background ,blu-4)))
     (org-habit-overdue-face        ((t :background ,red-3)))
     (org-habit-overdue-future-face ((t :background ,red-4)))
     (org-habit-ready-face          ((t :background ,grn)))
     (org-habit-ready-future-face   ((t :background ,grn-1)))

     ;; outline
     (outline-1 ((t :foreground ,orn  )))
     (outline-2 ((t :foreground ,grn+4)))
     (outline-3 ((t :foreground ,blu-1)))
     (outline-4 ((t :foreground ,ylw-2)))
     (outline-5 ((t :foreground ,cyn  )))
     (outline-6 ((t :foreground ,grn+2)))
     (outline-7 ((t :foreground ,red-4)))
     (outline-8 ((t :foreground ,blu-4)))

     ;; re-builder
     (reb-match-0 ((t :background ,mgn :foreground ,bg)))
     (reb-match-1 ((t :background ,blu :foreground ,bg)))
     (reb-match-2 ((t :background ,orn :foreground ,bg)))
     (reb-match-3 ((t :background ,red :foreground ,bg)))

     ;; replace
     (match ((t :background ,bg-1 :foreground ,orn :weight bold)))

     ;; rst
     (rst-level-1-face ((t :foreground ,orn  )))
     (rst-level-2-face ((t :foreground ,grn+1)))
     (rst-level-3-face ((t :foreground ,blu-1)))
     (rst-level-4-face ((t :foreground ,ylw-2)))
     (rst-level-5-face ((t :foreground ,cyn  )))
     (rst-level-6-face ((t :foreground ,grn-1)))

     ;; ruler-mode
     (ruler-mode-column-number  ((t :foreground ,fg
                                    :inherit    ruler-mode-default)))
     (ruler-mode-comment-column ((t :inherit    ruler-mode-fill-column)))
     (ruler-mode-current-column ((t :foreground ,ylw :box t)))
     (ruler-mode-default        ((t :background ,bg  :foreground ,grn+2)))
     (ruler-mode-fill-column    ((t :foreground ,ylw
                                    :inherit    ruler-mode-default)))
     (ruler-mode-goal-column    ((t :inherit    ruler-mode-fill-column)))
     (ruler-mode-tab-stop       ((t :inherit    ruler-mode-fill-column)))

     ;; sh-script
     (sh-heredoc     ((t :foreground ,ylw :weight bold)))
     (sh-quoted-exec ((t :foreground ,red)))

     ;; speedbar
     (speedbar-button-face    ((t :foreground ,grn+2)))
     (speedbar-directory-face ((t :foreground ,cyn)))
     (speedbar-file-face      ((t :foreground ,fg)))
     (speedbar-highlight-face ((t :foreground ,bg :background ,grn+2)))
     (speedbar-selected-face  ((t :foreground ,red)))
     (speedbar-separator-face ((t :foreground ,bg :background ,blu-1)))
     (speedbar-tag-face       ((t :foreground ,ylw)))

     ;; term
     (term-color-black      ((t :background ,bg-1  :foreground ,bg)))
     (term-color-blue       ((t :background ,blu-4 :foreground ,blu-1)))
     (term-color-cyan       ((t :background ,blu   :foreground ,cyn)))
     (term-color-green      ((t :background ,grn+2 :foreground ,grn)))
     (term-color-magenta    ((t :background ,red   :foreground ,mgn)))
     (term-color-red        ((t :background ,red-4 :foreground ,red-2)))
     (term-color-white      ((t :background ,fg-1  :foreground ,fg)))
     (term-color-yellow     ((t :background ,ylw   :foreground ,orn)))
     (term-default-bg-color ((t :inherit    term-color-black)))
     (term-default-fg-color ((t :inherit    term-color-white)))

     ;; tooltip
     (tooltip ((t :background ,bg+1 :foreground ,fg)))

     ;; which-func
     (which-func ((t :foreground ,grn+4)))

     ;; whitespace
     (whitespace-empty            ((t :background ,ylw)))
     (whitespace-hspace           ((t :background ,bg+1 :foreground ,bg+1)))
     (whitespace-indentation      ((t :background ,ylw  :foreground ,red)))
     (whitespace-line             ((t :background ,bg   :foreground ,mgn)))
     (whitespace-newline          ((t :foreground ,bg+1)))
     (whitespace-space            ((t :background ,bg+1 :foreground ,bg+1)))
     (whitespace-space-after-tab  ((t :background ,ylw  :foreground ,red)))
     (whitespace-space-before-tab ((t :background ,orn  :foreground ,orn)))
     (whitespace-tab              ((t :background ,red-1)))
     (whitespace-trailing         ((t :background ,red)))

     ;; woman
     (woman-bold   ((t :inherit Man-overstrike)))
     (woman-italic ((t :inherit font-lock-string-face :slant italic)))

     ;; External

     ;; ace-window
     (aw-background-face   ((t :background ,bg :foreground ,fg-1)))
     (aw-leading-char-face ((t :inherit    aw-mode-line-face)))

     ;; avy
     (avy-background-face ((t :background ,bg :foreground ,fg-1)))
     (avy-lead-face       ((t :background ,bg :foreground ,cyn :weight bold)))
     (avy-lead-face-0     ((t :foreground ,grn+3 :inherit avy-lead-face)))
     (avy-lead-face-1     ((t :foreground ,ylw   :inherit avy-lead-face)))
     (avy-lead-face-2     ((t :foreground ,red+1 :inherit avy-lead-face)))

     ;; company
     (company-preview                      ((t :background ,grn+2)))
     (company-preview-common               ((t :background ,bg-1
                                               :foreground ,grn+2)))
     (company-scrollbar-fg                 ((t :background ,bg-1)))
     (company-scrollbar-bg                 ((t :background ,bg+2)))
     (company-tooltip                      ((t :background ,bg+1
                                               :foreground ,fg)))
     (company-tooltip-annotation           ((t :background ,bg+1
                                               :foreground ,orn)))
     (company-tooltip-annotation-selection ((t :background ,bg-1
                                               :foreground ,orn)))
     (company-tooltip-common               ((t :foreground ,grn+2)))
     (company-tooltip-common-selection     ((t :foreground ,grn+2)))
     (company-tooltip-mouse                ((t :background ,bg-1)))
     (company-tooltip-selection            ((t :background ,bg-1
                                               :foreground ,fg)))

     ;; debbugs-gnu
     (debbugs-gnu-done    ((t :foreground ,fg-1)))
     (debbugs-gnu-handled ((t :foreground ,grn )))
     (debbugs-gnu-new     ((t :foreground ,red )))
     (debbugs-gnu-pending ((t :foreground ,blu )))
     (debbugs-gnu-stale   ((t :foreground ,orn )))
     (debbugs-gnu-tagged  ((t :foreground ,red )))

     ;; font-latex
     (font-latex-bold-face         ((t :inherit    bold)))
     (font-latex-italic-face       ((t :foreground ,cyn :slant italic)))
     (font-latex-math-face         ((t :foreground ,orn)))
     (font-latex-script-char-face  ((t :foreground ,orn)))
     (font-latex-sectioning-5-face ((t :foreground ,red :weight bold)))
     (font-latex-sedate-face       ((t :foreground ,ylw)))
     (font-latex-string-face       ((t :inherit    font-lock-string-face)))
     (font-latex-warning-face      ((t :inherit    font-lock-warning-face)))

     ;; git-annex
     (git-annex-dired-annexed-available   ((t :inherit success :weight normal)))
     (git-annex-dired-annexed-unavailable ((t :inherit error   :weight normal)))

     ;; google-contacts
     (google-contacts-familyname ((t :inherit font-lock-keyword-face)))
     (google-contacts-givenname  ((t :inherit google-contacts-familyname)))
     (google-contacts-header     ((t :inherit font-lock-string-face)))

     ;; hackernews
     (hackernews-comment-count ((t :inherit link-visited :underline nil)))
     (hackernews-link          ((t :inherit link         :underline nil)))

     ;; helm
     (helm-candidate-number ((t :background ,bg-1  :foreground ,grn+4)))
     (helm-header           ((t :background ,bg    :foreground ,grn)))
     (helm-match            ((t :background ,bg-1  :foreground ,orn
                                :weight     bold)))
     (helm-selection        ((t :background ,bg+1)))
     (helm-separator        ((t :background ,bg    :foreground ,red)))
     (helm-source-header    ((t :background ,bg-1  :foreground ,ylw :weight bold
                                ,@box-1)))
     (helm-visible-mark     ((t :background ,ylw-2 :foreground ,bg)))

     ;; helm-bookmark
     (helm-bookmark-addressbook ((t :background ,bg :foreground ,orn)))
     (helm-bookmark-directory   ((t :inherit    helm-ff-directory)))
     (helm-bookmark-file        ((t :inherit    helm-ff-file)))
     (helm-bookmark-gnus        ((t :background ,bg :foreground ,mgn)))
     (helm-bookmark-info        ((t :background ,bg :foreground ,grn+2)))
     (helm-bookmark-man         ((t :background ,bg :foreground ,ylw)))
     (helm-bookmark-w3m         ((t :background ,bg :foreground ,mgn)))

     ;; helm-buffers
     (helm-buffer-not-saved ((t :background ,bg :foreground ,red )))
     (helm-buffer-process   ((t :background ,bg :foreground ,cyn )))
     (helm-buffer-saved-out ((t :background ,bg :foreground ,fg  )))
     (helm-buffer-size      ((t :background ,bg :foreground ,fg-1)))

     ;; helm-files
     (helm-ff-directory       ((t :background ,bg :foreground ,cyn
                                  :weight     bold)))
     (helm-ff-file            ((t :background ,bg :foreground ,fg)))
     (helm-ff-executable      ((t :background ,bg :foreground ,grn+2)))
     (helm-ff-invalid-symlink ((t :background ,bg :foreground ,red
                                  :weight     bold)))
     (helm-ff-symlink         ((t :background ,bg :foreground ,ylw
                                  :weight     bold)))
     (helm-ff-prefix          ((t :foreground ,bg :background ,ylw)))

     ;; helm-grep
     (helm-grep-cmd-line ((t :background ,bg :foreground ,cyn)))
     (helm-grep-file     ((t :background ,bg :foreground ,fg)))
     (helm-grep-finish   ((t :background ,bg :foreground ,grn+2)))
     (helm-grep-lineno   ((t :background ,bg :foreground ,fg-1)))
     (helm-grep-match    ((t :inherit    helm-match)))
     (helm-grep-running  ((t :background ,bg :foreground ,red)))

     ;; helm-misc
     (helm-time-zone-current ((t :background ,bg :foreground ,grn+2)))
     (helm-time-zone-home    ((t :background ,bg :foreground ,red)))

     ;; helm-regexp
     (helm-moccur-buffer ((t :background ,bg :foreground ,cyn)))

     ;; helm-utils
     (helm-selection-line ((t :background ,bg+1)))

     ;; hydra
     (hydra-face-amaranth ((t :background ,bg :foreground ,red-3)))
     (hydra-face-blue     ((t :background ,bg :foreground ,blu  )))
     (hydra-face-pink     ((t :background ,bg :foreground ,mgn  )))
     (hydra-face-red      ((t :background ,bg :foreground ,red-1)))
     (hydra-face-teal     ((t :background ,bg :foreground ,cyn  )))

     ;; irfc
     (irfc-head-name-face           ((t :foreground ,red   :weight bold)))
     (irfc-head-number-face         ((t :foreground ,red   :weight bold)))
     (irfc-reference-face           ((t :foreground ,blu-1 :weight bold)))
     (irfc-requirement-keyword-face ((t :inherit    font-lock-keyword-face)))
     (irfc-rfc-link-face            ((t :inherit    link)))
     (irfc-rfc-number-face          ((t :foreground ,cyn   :weight bold)))
     (irfc-std-number-face          ((t :foreground ,grn+4 :weight bold)))
     (irfc-table-item-face          ((t :foreground ,grn+3)))
     (irfc-title-face               ((t :foreground ,ylw :underline t
                                        :weight     bold)))

     ;; ivy
     (ivy-action                  ((t :inherit    font-lock-keyword-face)))
     (ivy-confirm-face            ((t :background ,bg  :foreground ,grn)))
     (ivy-current-match           ((t :foreground ,ylw :underline  t
                                      :weight     bold)))
     (ivy-match-required-face     ((t :background ,bg  :foreground ,red)))
     (ivy-minibuffer-match-face-1 ((t :background ,bg+1)))
     (ivy-minibuffer-match-face-2 ((t :background ,red-4)))
     (ivy-minibuffer-match-face-3 ((t :background ,blu-4)))
     (ivy-minibuffer-match-face-4 ((t :background ,grn-1)))
     (ivy-modified-buffer         ((t :foreground ,orn)))
     (ivy-remote                  ((t :background ,bg :foreground ,blu)))
     (ivy-subdir                  ((t :background ,bg :foreground ,ylw)))
     (ivy-virtual                 ((t :inherit    shadow)))

     ;; ivy-overlay
     (ivy-cursor                  ((t :background ,fg :foreground ,bg)))

     ;; js2-mode
     (js2-error                    ((t :inherit     error)))
     (js2-external-variable        ((t :foreground ,orn  )))
     (js2-function-call            ((t :foreground ,cyn  )))
     (js2-function-param           ((t :foreground ,orn  )))
     (js2-instance-member          ((t :foreground ,grn-1)))
     (js2-jsdoc-html-tag-delimiter ((t :foreground ,orn  )))
     (js2-jsdoc-html-tag-name      ((t :foreground ,red-1)))
     (js2-jsdoc-tag                ((t :foreground ,grn-1)))
     (js2-jsdoc-type               ((t :foreground ,grn+2)))
     (js2-jsdoc-value              ((t :foreground ,grn+3)))
     (js2-object-property          ((t :foreground ,blu+1)))
     (js2-private-function-call    ((t :foreground ,cyn  )))
     (js2-private-member           ((t :foreground ,blu-1)))
     (js2-warning                  ((t :underline  ,orn  )))

     ;; ledger-fonts
     (ledger-font-auto-xact-face               ((t :foreground ,ylw-1)))
     (ledger-font-comment-face                 ((t :foreground ,grn  )))
     (ledger-font-other-face                   ((t :foreground ,fg   )))
     (ledger-font-payee-cleared-face           ((t :foreground ,fg   )))
     (ledger-font-payee-pending-face           ((t :foreground ,red  )))
     (ledger-font-payee-uncleared-face         ((t :foreground ,red-1
                                                   :weight     bold  )))
     (ledger-font-pending-face                 ((t :foreground ,orn  )))
     (ledger-font-periodic-xact-face           ((t :foreground ,grn  )))
     (ledger-font-posting-account-cleared-face ((t :foreground ,fg   )))
     (ledger-font-posting-account-face         ((t :foreground ,blu-1)))
     (ledger-font-posting-account-pending-face ((t :foreground ,orn  )))
     (ledger-font-posting-amount-face          ((t :foreground ,orn  )))
     (ledger-font-posting-date-face            ((t :foreground ,orn  )))
     (ledger-font-reconciler-cleared-face      ((t :foreground ,fg   )))
     (ledger-font-reconciler-pending-face      ((t :foreground ,orn  )))
     (ledger-font-reconciler-uncleared-face    ((t :foreground ,red-1
                                                   :weight     bold  )))
     (ledger-font-report-clickable-face        ((t :foreground ,orn  )))
     (ledger-font-xact-highlight-face          ((t :background ,bg+1 )))
     (ledger-occur-narrowed-face               ((t :foreground ,fg-1 )))
     (ledger-occur-xact-face                   ((t :background ,bg+1 )))

     ;; macrostep
     (macrostep-expansion-highlight-face ((t :inherit    highlight)))
     (macrostep-gensym-1                 ((t :background ,bg-1
                                             :foreground ,grn+2)))
     (macrostep-gensym-2                 ((t :background ,bg-1
                                             :foreground ,red+1)))
     (macrostep-gensym-3                 ((t :background ,bg-1
                                             :foreground ,blu+1)))
     (macrostep-gensym-4                 ((t :background ,bg-1
                                             :foreground ,mgn)))
     (macrostep-gensym-5                 ((t :background ,bg-1
                                             :foreground ,ylw)))
     (macrostep-macro-face               ((t :underline  t)))

     ;; magit
     (magit-branch-local        ((t :foreground ,blu :weight bold)))
     (magit-branch-remote       ((t :foreground ,grn :weight bold)))
     (magit-cherry-equivalent   ((t :foreground ,mgn)))
     (magit-cherry-unmatched    ((t :foreground ,cyn)))
     (magit-dimmed              ((t :foreground ,bg+3)))
     (magit-hash                ((t :foreground ,bg+3)))
     (magit-refname             ((t :background ,bg+2 :foreground ,fg
                                    :weight     bold)))
     (magit-signature-bad       ((t :foreground ,red)))
     (magit-signature-error     ((t :inherit    magit-signature-bad)))
     (magit-signature-expired   ((t :foreground ,orn)))
     (magit-signature-good      ((t :foreground ,grn)))
     (magit-signature-revoked   ((t :foreground ,mgn)))
     (magit-signature-untrusted ((t :foreground ,ylw)))
     (magit-tag                 ((t :foreground ,orn :weight bold)))

     ;; magit-bisect
     (magit-bisect-bad  ((t :foreground ,red)))
     (magit-bisect-good ((t :foreground ,grn)))
     (magit-bisect-skip ((t :foreground ,ylw)))

     ;; magit-blame
     (magit-blame-heading ((t :background ,bg-1 :foreground ,blu-2)))
     (magit-blame-date    ((t :background ,bg-1 :foreground ,orn)))
     (magit-blame-name    ((t :inherit    magit-blame-date)))
     (magit-blame-summary ((t :background ,bg-1 :foreground ,blu-2
                              :weight     bold)))

     ;; magit-diff
     (magit-diff-context-highlight      ((t :background ,bg+05
                                            :foreground "grey70")))
     (magit-diff-file-heading-selection
      ((t :foreground ,orn :inherit magit-diff-file-heading-highlight)))
     (magit-diff-hunk-heading           ((t :background ,bg+1)))
     (magit-diff-hunk-heading-highlight ((t :background ,bg+2)))
     (magit-diff-hunk-heading-selection ((t :inherit magit-diff-lines-heading)))
     (magit-diff-lines-heading
      ((t :background ,orn :inherit magit-diff-hunk-heading-highlight)))
     (magit-diffstat-added              ((t :foreground ,grn+4)))
     (magit-diffstat-removed            ((t :foreground ,red)))

     ;; magit-log
     (magit-log-author         ((t :foreground ,orn )))
     (magit-log-date           ((t :foreground ,fg-1)))
     (magit-log-graph          ((t :foreground ,fg+1)))
     (magit-reflog-amend       ((t :foreground ,mgn )))
     (magit-reflog-checkout    ((t :foreground ,blu )))
     (magit-reflog-cherry-pick ((t :foreground ,grn )))
     (magit-reflog-commit      ((t :foreground ,grn )))
     (magit-reflog-merge       ((t :foreground ,grn )))
     (magit-reflog-other       ((t :foreground ,cyn )))
     (magit-reflog-rebase      ((t :foreground ,mgn )))
     (magit-reflog-remote      ((t :foreground ,cyn )))
     (magit-reflog-reset       ((t :foreground ,red )))

     ;; magit-popup
     (magit-popup-argument ((t :inherit font-lock-function-name-face
                               :weight  bold)))
     (magit-popup-key      ((t :inherit font-lock-type-face)))

     ;; magit-process
     (magit-process-ng ((t :foreground ,red :inherit magit-section-heading)))
     (magit-process-ok ((t :foreground ,grn :inherit magit-section-heading)))

     ;; magit-section
     (magit-section-heading           ((t :foreground ,ylw :weight bold)))
     (magit-section-heading-selection ((t :foreground ,orn :weight bold)))
     (magit-section-highlight         ((t :background ,bg+05)))

     ;; magit-sequence
     (magit-sequence-done ((t :foreground ,fg-1 )))
     (magit-sequence-drop ((t :foreground ,red  )))
     (magit-sequence-head ((t :foreground ,blu  )))
     (magit-sequence-part ((t :foreground ,ylw  )))
     (magit-sequence-pick ((t :foreground ,ylw-2)))
     (magit-sequence-stop ((t :foreground ,grn  )))

     ;; minimap
     (minimap-active-region-background ((t :background ,fg-1)))
     (minimap-font-face                ((t :height     10)))

     ;; popup
     (popup-isearch-match              ((t :inherit    default)))
     (popup-menu-mouse-face            ((t :background ,ylw-2
                                           :foreground ,bg-2)))
     (popup-scroll-bar-background-face ((t :background ,bg-1)))
     (popup-scroll-bar-foreground-face ((t :background ,blu-5)))
     (popup-summary-face               ((t :background ,bg+3
                                           :foreground ,bg-2)))
     (popup-tip-face                   ((t :background ,ylw-2
                                           :foreground ,bg-2)))

     ;; regex-tool
     (regex-tool-matched-face ((t :background ,blu-4 :weight bold)))

     ;; sx-question-list
     (sx-custom-button                 ((t :background ,fg :foreground ,bg-1
                                           :height 0.9 ,@(funcall box 3))))
     (sx-question-list-answers         ((t :foreground ,grn+3 :height 1.0
                                           :inherit sx-question-list-parent)))
     (sx-question-list-read-question   ((t :inherit link-visited
                                           :underline nil)))
     (sx-question-list-unread-question ((t :inherit link :underline nil)))
     (sx-question-mode-accepted        ((t :foreground ,grn+3 :height 1.3
                                           :inherit sx-question-mode-title)))
     (sx-question-mode-content-face    ((t :inherit highlight)))
     (sx-question-mode-kbd-tag
      ((t :box (:color ,bg-1 :line-width 3 :style released-button)
          :height 0.9 :weight semi-bold)))

     ;; visual-regexp
     (vr/group-0              ((t :background ,grn :foreground ,bg
                                  :weight     bold)))
     (vr/group-1              ((t :background ,orn :inherit vr/group-0)))
     (vr/group-2              ((t :background ,blu :inherit vr/group-0)))
     (vr/match-0              ((t :inherit    isearch)))
     (vr/match-1              ((t :background ,bg-1 :foreground ,ylw-2
                                  :weight     bold)))
     (vr/match-separator-face ((t :foreground ,red :weight bold)))

     ;; w3m
     (w3m-anchor                     ((t :foreground ,ylw   :underline t
                                         :weight     bold)))
     (w3m-arrived-anchor             ((t :foreground ,ylw-2 :underline t)))
     (w3m-form                       ((t :foreground ,red-1 :underline t)))
     (w3m-header-line-location-title ((t :inherit    w3m-anchor)))
     (w3m-history-current-url        ((t :inherit    match)))
     (w3m-lnum                       ((t :background ,bg   :foreground ,grn+2)))
     (w3m-lnum-match                 ((t :background ,bg-1 :foreground ,orn
                                         :weight     bold)))
     (w3m-lnum-minibuffer-prompt     ((t :foreground ,ylw)))

     ;; web-mode
     (web-mode-comment-face           ((t :inherit    font-lock-comment-face)))
     (web-mode-constant-face          ((t :inherit    font-lock-constant-face)))
     (web-mode-doctype-face           ((t :inherit    font-lock-comment-face)))
     (web-mode-error-face             ((t :inherit    error)))
     (web-mode-folded-face            ((t :underline  t)))
     (web-mode-html-attr-name-face    ((t :foreground ,orn)))
     (web-mode-html-tag-bracket-face  ((t :inherit
                                          web-mode-html-attr-name-face)))
     (web-mode-html-tag-face          ((t :foreground ,cyn)))
     (web-mode-symbol-face            ((t :inherit    font-lock-constant-face)))
     (web-mode-whitespaces-face       ((t :background ,red))))))

(provide-theme 'blc-dark)

;;; blc-dark-theme.el ends here
