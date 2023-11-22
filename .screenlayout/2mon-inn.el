#!/home/blc/.local/bin/emacs -x

(require 'blc-randr
         (expand-file-name "blc-randr" (file-name-directory load-file-name)))
(require 'map)

(blc-randr-with-proc call
  (when-let ((fst-mode '(2560 1440))
             (snd-mode '(3840 2160))
             (thd-mode '(1920 1080))
             (outs (blc-randr-resolutions #'call))
             (pred (lambda (width height)
                     (let ((mode (blc-randr-mode width height)))
                       (lambda (out res)
                         (and (string-prefix-p "DisplayPort-" out)
                              (equal res mode)
                              out)))))
             (fst (map-some (apply pred fst-mode) outs))
             (snd (map-some (apply pred snd-mode) outs)))
    (apply #'call "xrandr" "--output" "eDP" "--off" "--output" fst
           "--primary" "--preferred" "--pos" "0x0" "--rotate" "normal"
           "--output" snd "--mode" (apply #'blc-randr-mode thd-mode)
           "--right-of" fst "--rotate" "left"
           (mapcan (lambda (out) (list "--output" (car out) "--off"))
                   (assoc-delete-all snd (assoc-delete-all fst outs))))
    (call "xrandr" "--output" fst
          "--pos" (blc-randr-mode 0 (- (car thd-mode) (cadr fst-mode))))
    (call "blc-xkb")))
