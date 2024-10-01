#!/home/blc/.local/bin/emacs -x

(let* ((delta (format (if command-line-args-left "%d%%-" "+%d%%") 5))
       (new (with-temp-buffer
              (call-process "brightnessctl" nil t nil "--machine-readable"
                            "--exponent=2" "set" delta)
              (skip-chars-backward "^,")
              (backward-char)
              (delete-char -1)
              (skip-chars-backward "^,")
              (read (current-buffer)))))
  (call-process "dunstify" nil 0 nil (format "Brightness %d%%" new)
                "-h" "string:x-dunst-stack-tag:lux"
                "-h" (format "int:value:%d" new)))
