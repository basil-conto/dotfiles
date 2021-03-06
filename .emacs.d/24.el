;;; Theme
(load-theme 'tango-dark)

;;; C source code
(setq-default indicate-buffer-boundaries t)

;;; delsel
(delete-selection-mode)

;;; frame
(blink-cursor-mode 0)

;;; ielm
(with-eval-after-load 'ielm
  (defun ielm-indent-line ()
    "Indent the current line as Lisp code if it is not a prompt line."
    (when (save-excursion (comint-bol t) (bolp))
      (lisp-indent-line))))

;;; menu-bar
(menu-bar-mode 0)

;;; paren
(show-paren-mode)

;;; scroll-bar
(scroll-bar-mode 0)

;;; simple
(column-number-mode)

;;; tool-bar
(tool-bar-mode 0)

;;; winner
(winner-mode)
