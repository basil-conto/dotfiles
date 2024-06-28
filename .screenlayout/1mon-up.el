#!/home/blc/.local/bin/emacs -x

(require 'blc-randr
         (expand-file-name "blc-randr" (file-name-directory load-file-name)))

(blc-randr-with-proc call
  (when-let ((outs (blc-randr-resolutions #'call))
             (edp (assoc "eDP" outs))
             (out (seq-some (lambda (out)
                              (and (not (eq out edp))
                                   (cdr out)
                                   (car out)))
                            outs)))
    (apply #'call "xrandr" "--output" out
           "--primary" "--preferred" "--pos" "0x0" "--rotate" "normal"
           "--output" (car edp)
           "--preferred" "--below" out "--rotate" "normal"
           (mapcan (pcase-lambda (`(,o . ,_))
                     (unless (member o (list (car edp) out))
                       (list "--output" o "--off")))
                   outs))
    (call "blc-xkb")))
