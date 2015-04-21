# -*- sh -*-
# ~/.bash_aliases

# nano with smart home and tabs = 4 spaces
alias nano='nano -A -E -T4'

# ls
alias l='ls -Fh --group-directories-first'
alias ll='l -l'
alias la='ll -A'

# apt
alias uu='sudo apt-get update && sudo apt-get upgrade'

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
alias g++11='g++ -std=gnu++11'
alias gcc99='gcc -std=gnu99'
alias gcc11='gcc -std=gnu11'

# tmux, emacs colours
alias tmux='TERM=xterm-256color tmux'
alias emacs='TERM=xterm-256color emacs -nw'

# youtube-dl
alias youtube-mp3='youtube-dl -x --audio-format mp3 --audio-quality 256K'
alias youtube-vorbis='youtube-dl -x --audio-format vorbis --audio-quality 128K'

# chromium
alias chrome-proxy='google-chrome --proxy-auto-detect &> /dev/null &'
alias chromium-proxy='chromium --proxy-auto-detect &> /dev/null &'

# fun fun fun
alias hi='echo -e "\e[1;31mI Love You\e[0m"'
