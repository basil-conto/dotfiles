#!/home/blc/.local/bin/emacs -x

(require 'blc-randr
         (expand-file-name "blc-randr" (file-name-directory load-file-name)))

(blc-randr-with-proc call
  (when-let ((outs (blc-randr-resolutions #'call))
             (edp (assoc "eDP" outs))
             (fst-mode '(2560 1440))
             (snd-mode '(3840 2160))
             (thd-mode (cadr edp))
             (pred (lambda (width height)
                     (let ((mode (blc-randr-mode width height)))
                       (pcase-lambda (`(,out . ,res))
                         (and (string-prefix-p "DisplayPort-" out)
                              (equal (car res) mode))))))
             (fst (seq-find (apply pred fst-mode) outs))
             (snd (seq-find (apply pred snd-mode) outs))
             ((member thd-mode (cdr snd))))
    (apply #'call "xrandr" "--output" (car edp) "--off" "--output" (car fst)
           "--primary" "--preferred" "--pos" "0x0" "--rotate" "normal"
           "--output" (car snd) "--mode" thd-mode
           "--right-of" (car fst) "--rotate" "left"
           (mapcan (lambda (out)
                     (unless (memq out (list fst snd))
                       (list "--output" (car out) "--off")))
                   outs))
    (call "xrandr" "--output" (car fst)
          "--pos" (blc-randr-mode 0 (- (string-to-number thd-mode)
                                       (cadr fst-mode))))
    (call "blc-xkb")))
