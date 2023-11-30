#!/home/blc/.local/bin/emacs -x

(require 'blc-randr
         (expand-file-name "blc-randr" (file-name-directory load-file-name)))

(blc-randr-with-proc call
  (when-let ((outs (blc-randr-resolutions #'call))
             (edp (assoc "eDP" outs))
             (out (seq-some (lambda (out)
                              (and (not (eq out edp))
                                   (member (cadr edp) (cdr out))
                                   (car out)))
                            outs)))
    (apply #'call "xrandr" "--output" (car edp)
           "--primary" "--preferred" "--pos" "0x0" "--rotate" "normal"
           "--output" out
           "--mode" (cadr edp) "--same-as" (car edp) "--rotate" "normal"
           (mapcan (pcase-lambda (`(,o . ,_))
                     (unless (member o (list (car edp) out))
                       (list "--output" o "--off")))
                   outs))))
