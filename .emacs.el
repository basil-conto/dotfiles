(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("132ccc75b7fdcd9f5979329620a1151953a8f65efad06b988deed7cba9338eab" default)))
 '(doc-view-continuous t)
 '(ess-default-style (quote DEFAULT))
 '(font-lock-maximum-decoration 2)
 '(ido-enable-flex-matching t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(jit-lock-stealth-time 16)
 '(js2-basic-offset 4)
 '(minimap-highlight-line nil)
 '(minimap-recenter-type (quote relative))
 '(minimap-width-fraction 0.05)
 '(minimap-window-location (quote right))
 '(scroll-conservatively 10000)
 '(scroll-error-top-bottom t)
 '(scroll-preserve-screen-position t)
 '(scroll-step 1)
 '(server-kill-new-buffers nil)
 '(show-paren-mode t)
 '(speedbar-show-unknown-files t)
 '(speedbar-update-flag nil)
 '(speedbar-use-images t)
 '(speedbar-vc-do-check nil)
 '(sr-speedbar-auto-refresh nil)
 '(tab-always-indent t)
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76)))
 '(todoo-indent-column 2)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 90 :width normal))))
 '(minimap-active-region-background ((t (:background "dim gray"))))
 '(minimap-font-face ((t (:height 8 :family "DejaVu Sans Mono")))))

(defconst dot-emacs
  (concat (getenv "HOME") "/" ".emacs.blc.el")
  "My dot emacs file")

(require 'bytecomp)
(setq compiled-dot-emacs (byte-compile-dest-file dot-emacs))

(if (or (not (file-exists-p compiled-dot-emacs))
        (file-newer-than-file-p dot-emacs compiled-dot-emacs)
        (equal (nth 4 (file-attributes dot-emacs)) (list 0 0)))
    (load dot-emacs)
  (load compiled-dot-emacs))

(add-hook 'kill-emacs-hook
          '(lambda () (and (file-newer-than-file-p dot-emacs compiled-dot-emacs)
                           (byte-compile-file dot-emacs))))
