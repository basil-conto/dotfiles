#!/home/blc/.local/bin/emacs -x

(defun blc-lux-bright ()
  "Return \"lux -G\" percentage as a number."
  (call-process "lux" nil t nil "-G")
  (backward-char)
  (delete-char -1)
  (forward-line 0)
  (prog1 (read (current-buffer))
    (goto-char (point-max))))

(defun blc-lux-bounds ()
  "Return list of (MIN MAX) thresholds that \"lux\" reports."
  (call-process "lux" nil t)
  (re-search-backward
   (rx (group (+ digit)) ?\; (group (+ digit)) ?\; (group (+ digit))))
  (prog1 (mapcar (lambda (n) (string-to-number (match-string n))) '(1 3))
    (goto-char (point-max))))

(with-temp-buffer
  (let* ((sub (string-equal (pop command-line-args-left) "-s"))
         (inc (string-to-number (pop command-line-args-left)))
         (pct (blc-lux-bright))
         (bnd (blc-lux-bounds))
         (min (car bnd))
         (max (cadr bnd))
         (new (* 0.01 (- max min) (+ pct (if sub (- inc) inc))))
         (new (min max (max min (+ min (ceiling new))))))
    (call-process "lux" nil nil nil "-S" (number-to-string new))
    (setq pct (blc-lux-bright))
    (call-process "dunstify" nil 0 nil (format "Brightness %d%%" pct)
                  "-h" "string:x-dunst-stack-tag:lux"
                  "-h" (format "int:value:%d" pct))))
