#!/usr/bin/env sh

#          ┌────┐
#          │    │
# ┌──────┐ │HDM1│
# │LVDS1*│ │    │
# └──────┘ └────┘
xrandr --output LVDS1 --mode 1600x900  --pos 0x780  --rotate normal --primary \
       --output HDMI1 --mode 1680x1050 --pos 1600x0 --rotate left

# Reset background
[ -x "${HOME}/.fehbg" ] && "${HOME}/.fehbg"
