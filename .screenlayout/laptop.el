#!/home/blc/.local/bin/emacs -x

(require 'blc-randr
         (expand-file-name "blc-randr" (file-name-directory load-file-name)))

(blc-randr-with-proc call
  (apply #'call "xrandr" "--output" "eDP"
         "--primary" "--preferred" "--pos" "0x0" "--rotate" "normal"
         (mapcan (lambda (out) (list "--output" (car out) "--off"))
                 (blc-randr-resolutions #'call))))
