#!/home/blc/.local/bin/emacs -x

(require 'map)
(eval-and-compile
  (let* ((me (or (macroexp-file-name) load-file-name))
         (load-path (cons (file-name-directory me) load-path)))
    (require 'blc-randr)))

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
