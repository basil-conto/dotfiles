;;; blc-light-theme.el --- Light theme for blc -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <basil.conto@gmail.com>
;; Homepage: https://github.com/basil-conto/dotfiles

;;; Commentary:

;;; Code:

(deftheme blc-light
  "Light theme for blc.")

(apply
 #'custom-theme-set-faces
 'blc-light
 `(;; Built-in

   ;; External

   ;; hackernews
   (hackernews-comment-count ((t :inherit link-visited :underline nil)))
   (hackernews-link          ((t :inherit link         :underline nil)))

   ;; sx-question-list
   (sx-question-list-read-question   ((t :inherit   link-visited
                                         :underline nil)))
   (sx-question-list-unread-question ((t :inherit   link :underline nil)))))


(provide-theme 'blc-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; blc-light-theme.el ends here
