# -*- sh -*-
# ~/.bash_aliases

# nano with smart home and 2-space tabs
alias nano='nano -A -E -T2'

# safety
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -I'

# ls
alias ls='ls --color=auto --group-directories-first'
alias l='ls -F'
alias ll='l -lh'
alias la='ll -A'

# apt
alias uu='sudo apt-get update && sudo apt-get upgrade'
alias uuu='uu && sudo apt-get dist-upgrade'
alias uuuu='uuu && sudo apt-get autoremove && sudo apt-get autoclean'

# wajig
alias list='wajig listinstalled'

# xdg
alias dis='xdg-open'

# xbacklight
alias dark='xbacklight -set 0'
alias half='xbacklight -set 50'
alias bright='xbacklight -set 100'
alias dim='xbacklight -dec 10'
alias brighten='xbacklight -inc 10'

# GNU compilers
alias gcc='gcc -fdiagnostics-color'
alias g++='g++ -fdiagnostics-color'
alias gcc99='gcc -std=gnu99'
alias gcc11='gcc -std=gnu11'
alias g++11='g++ -std=gnu++11'

# emacsen
alias em='emacs -nw'
alias ec='emacsclient -a "" -t'
alias ecc='emacsclient -a "" -c -n'

# colordiff
alias ciff='colordiff -y -W 160'

# mp3blaster
alias blaster='padsp mp3blaster'

# virtualbox
alias startvm='vboxmanage startvm'

# youtube-dl
alias ydl-mp3='ydl -x --audio-format mp3 --audio-quality 256K'
alias ydl-vorbis='ydl -x --audio-format vorbis --audio-quality 128K'

# chromium
alias chromium-proxy='chromium --proxy-auto-detect &> /dev/null &'

# notify-send
alias apprise='notify-send -u low'
alias error='notify-send -u critical -i error'

# gcal
alias gcal='gcal -s 1'

# cpufreq
alias policy='cpufreq-info -p'
alias powersave='sudo cpufreq-set -rg powersave'
alias performance='sudo cpufreq-set -rg performance'

# fun fun fun
alias hi='printf "%b%s%b\n" "\e[1;31m" "I Love You" "\e[0m"'
