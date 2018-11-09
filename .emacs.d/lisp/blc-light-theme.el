;;; blc-light-theme.el --- Light theme for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://gitlab.com/basil-conto/dotfiles

;;; Commentary:

;;; Code:

(deftheme blc-light
  "Light theme for blc.")

(apply
 #'custom-theme-set-variables
 'blc-light
 '(;; Built-in

   ;; gnus-spec
   (gnus-face-4 'shadow)

   ;; org-faces
   (org-priority-faces
    '((?A . error)
      (?C . shadow)))
   (org-todo-keyword-faces
    '(("PROJ" :inherit (font-lock-comment-face       org-todo))
      ("EXEC" :inherit (font-lock-variable-name-face org-todo))
      ("MEET" :inherit (font-lock-constant-face      org-todo))
      ("WAIT" :inherit (font-lock-preprocessor-face  org-todo))
      ("BALK" . bold)))))

(apply
 #'custom-theme-set-faces
 'blc-light
 `(;; Built-in

   ;; diff-mode
   (diff-refine-added   ((t :background "#7763fe697763" ; 10% darker
                            :inherit    diff-refine-changed)))
   (diff-refine-removed ((t :background "#fea2a1c3a1c3" ;  5% darker
                            :inherit    diff-refine-changed)))

   ;; faces
   (mode-line-inactive ((t :background "whitesmoke" :foreground "grey20"
                           :inherit mode-line :weight light
                           :box (:line-width -1 :color "grey75" :style nil))))

   ;; org-faces
   (org-agenda-date-today ((t :foreground "darkblue" :inherit org-agenda-date
                              :slant      italic     :weight  bold)))
   (org-upcoming-deadline ((t :foreground "purple")))

   ;; whitespace
   (whitespace-line ((t :underline t)))

   ;; External

   ;; gscholar-bibtex
   (gscholar-bibtex-title ((t :inherit bold)))

   ;; ledger-fonts
   (ledger-font-payee-uncleared-face
    ((t :inherit (font-lock-comment-face bold))))

   ;; magit
   (magit-signature-good ((t :foreground "green4")))

   ;; sx-question-list
   (sx-question-list-read-question   ((t :inherit   link-visited
                                         :underline nil)))
   (sx-question-list-unread-question ((t :inherit   link :underline nil)))))


(provide-theme 'blc-light)

;;; blc-light-theme.el ends here
