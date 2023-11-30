#!/home/blc/.local/bin/emacs -x

(require 'blc-randr
         (expand-file-name "blc-randr" (file-name-directory load-file-name)))

(blc-randr-with-proc call
  (let ((edp "eDP"))
    (apply #'call "xrandr" "--output" edp
           "--primary" "--preferred" "--pos" "0x0" "--rotate" "normal"
           (mapcan (pcase-lambda (`(,out . ,_))
                     (unless (equal out edp)
                       (list "--output" out "--off")))
                   (blc-randr-resolutions #'call)))))
