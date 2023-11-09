#!/home/blc/.local/bin/emacs -x

(let ((me (or (macroexp-file-name) load-file-name)))
  (load (expand-file-name "blc" (file-name-directory me)) nil t nil t))

(blc-with-call call
  (call "xrandr" "-q")
  (let ((outre (rx bol (group (+ (in alnum ?-)) ?- digit) " connected"))
        outs)
    (while (re-search-forward outre nil 'move)
      (setq outs (nconc (list "--output" (match-string 1) "--off") outs)))
    (apply #'call "xrandr" "--output" "eDP" "--primary" "--mode" "1920x1080"
           "--pos" "0x0" "--rotate" "normal" outs))
  (call "~/.fehbg"))
