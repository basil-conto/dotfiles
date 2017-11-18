;;; Theme
(set-face-attribute
 'default nil :background "#2B2B2B" :foreground "#DCDCCC")

;;; C source code
(setq-default
 indent-tabs-mode           nil
 indicate-buffer-boundaries t)

;;; delsel
(delete-selection-mode)

;;; menu-bar
(menu-bar-mode 0)

;;; paren
(show-paren-mode)

;;; scroll-bar
(scroll-bar-mode 0)

;;; simple
(column-number-mode)

;;; time
(setq-default display-time-24hr-format t)
(display-battery-mode)
(display-time-mode)

;;; tool-bar
(tool-bar-mode 0)

;;; winner
(winner-mode)
