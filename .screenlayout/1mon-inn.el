#!/home/blc/.local/bin/emacs -x

(require 'blc-randr
         (expand-file-name "blc-randr" (file-name-directory load-file-name)))
(require 'map)

(blc-randr-with-proc call
  (when-let ((outs (blc-randr-resolutions #'call))
             (out (map-some (lambda (out res)
                              (and (string-prefix-p "DisplayPort-" out)
                                   (equal "2560x1440" res)
                                   out))
                            outs)))
    (apply #'call "xrandr" "--output" "eDP" "--off" "--output" out
           "--primary" "--preferred" "--pos" "0x0" "--rotate" "normal"
           (mapcan (lambda (out) (list "--output" (car out) "--off"))
                   (assoc-delete-all out outs)))
    (call "blc-xkb")))
