;;; blc-light-theme.el --- Light theme for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://github.com/basil-conto/dotfiles

;;; Commentary:

;;; Code:

(deftheme blc-light
  "Light theme for blc.")

(apply
 #'custom-theme-set-variables
 'blc-light
 '(;; Built-in

   ;; gnus-spec
   (gnus-face-4 'shadow)))

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

   ;; whitespace
   (whitespace-line ((t :underline t)))

   ;; External

   ;; magit
   (magit-signature-good ((t :foreground "green4")))

   ;; sx-question-list
   (sx-question-list-read-question   ((t :inherit   link-visited
                                         :underline nil)))
   (sx-question-list-unread-question ((t :inherit   link :underline nil)))))


(provide-theme 'blc-light)

;;; blc-light-theme.el ends here
