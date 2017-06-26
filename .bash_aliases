# -*- sh -*-
# ~/.bash_aliases

# safety
alias cp='cp --interactive'
alias mv='mv --interactive'
alias rm='rm -I'

alias forget='echo RELOADAGENT | gpg-connect-agent'

# ls
alias ls='ls --color=auto --group-directories-first'
alias l='ls --classify'
alias ll='l --human-readable -l'
alias la='ll --almost-all'

# grep
alias grep='grep --color=auto'
alias egrep='grep --extended-regexp'
alias fgrep='grep --fixed-strings'
alias rgrep='grep --recursive'
alias psgrep='ps -A --no-headers \
--format="pid,euser,s,%cpu,%mem,start_time,args" | grep'

# directories
alias mkdir='mkdir --parents --verbose'
alias cdtemp='cd $(mktemp --directory)'
alias venture='pushd .; cd'

# make
alias make='make --jobs=$(nproc)'

# apt
alias uu='sudo apt-get update && sudo apt-get upgrade'
alias uuu='uu && sudo apt-get dist-upgrade'
alias uuuu='uuu && sudo apt-get autoremove && sudo apt-get autoclean'

# wajig
alias list='wajig listinstalled'

# xdg
alias dis='xdg-open'

# feh
# TODO: RC file?
alias feh='feh   \
--auto-rotate    \
--borderless     \
--draw-filename  \
--image-bg black \
--no-fehbg       \
--scale-down     \
--sort filename'

# xbacklight
alias dark='xbacklight -set 0'
alias half='xbacklight -set 50'
alias bright='xbacklight -set 100'
alias dim='xbacklight -dec 10'
alias brighten='xbacklight -inc 10'

# GNU compilers
alias gcc='gcc -fdiagnostics-color'
alias g++='g++ -fdiagnostics-color'
alias gcc99='gcc -std=c99'
alias gcc11='gcc -std=c11'
alias g++03='g++ -std=c++03'
alias g++11='g++ -std=c++11'

# emacsen
alias ec='TERM=xterm-24bits emacsclient --tty'
alias em='TERM=xterm-24bits emacs -nw'

# colordiff
alias ciff='colordiff --side-by-side --width=160'

# mp3blaster
alias blaster='padsp mp3blaster'

# virtualbox
alias startvm='vboxmanage startvm'

# youtube-dl
alias ydl='youtube-dl'
alias ydl-opus='ydl --extract-audio --audio-format opus --audio-quality 128K'

# notify-send
alias apprise='notify-send --urgency=low'
alias error='notify-send --icon=error --urgency=critical'

# gcal
alias gcal='gcal --starting-day=Monday'

# vlc
alias nvlc='nvlc --browse-dir .'

# cpufreq
alias policy='cpufreq-info --policy'
alias powersave='sudo cpufreq-set --governor powersave --related'
alias performance='sudo cpufreq-set --governor performance --related'

# fun fun fun
alias hi='printf "%b%s%b\n" "\e[1;31m" "I Love You" "\e[0m"'
