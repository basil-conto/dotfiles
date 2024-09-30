#!/home/blc/.local/bin/emacs -x

(require 'blc-randr
         (expand-file-name "blc-randr" (file-name-directory load-file-name)))
(require 'map)

(blc-randr-with-proc call
  (when-let ((outs (blc-randr-resolutions #'call))
             (out (map-some (lambda (out res)
                              (and (string-prefix-p "DisplayPort-" out)
                                   (equal (car res) "1920x1080")
                                   out))
                            outs)))
    (apply #'call "xrandr" "--output" out
           "--primary" "--preferred" "--pos" "0x0" "--rotate" "normal"
           (mapcan (pcase-lambda (`(,o . ,_))
                     (unless (equal o out) (list "--output" o "--off")))
                   outs))))
