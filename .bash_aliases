# -*- sh -*-
# ~/.bash_aliases

# safety
alias cp='cp --interactive'
alias mv='mv --interactive'
alias rm='rm -I'

alias forget='echo RELOADAGENT | gpg-connect-agent'

# ls
alias ls='ls --color=auto --group-directories-first -v'
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
alias cdtemp='cd "$(mktemp --directory)"'
alias venture='pushd .; cd'

# apt
alias uu='sudo apt update && \
sudo apt full-upgrade     && \
sudo apt autoremove       && \
sudo apt-get autoclean'

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
alias dark='lux -S 0%'
alias half='lux -S 50%'
alias bright='lux -S 100%'
alias dim='lux -s 10%'
alias brighten='lux -a 10%'

# GNU compilers
alias gcc='gcc -fdiagnostics-color'
alias g++='g++ -fdiagnostics-color'

# emacsen
if [[ "${TERM}" =~ tmux ]]; then
  alias ec='TERM=xterm-direct emacsclient --tty'
  alias em='TERM=xterm-direct emacs -nw'
else
  alias ec='emacsclient --tty'
  alias em='emacs -nw'
fi

# yt-dlp
alias yt-opus='yt-dlp --extract-audio --audio-format opus --audio-quality 96K'

# gcal
alias gcal='gcal --starting-day=Monday'

# vlc
alias nvlc='nvlc --browse-dir .'

# man
alias man='TERM=xterm-man man'

# cpupower
alias policy='sudo cpupower info; cpupower frequency-info --policy'
alias powersave='sudo cpupower frequency-set --governor powersave && \
sudo cpupower set --perf-bias 15'
alias performance='sudo cpupower frequency-set --governor performance && \
sudo cpupower set --perf-bias 0'

# fun fun fun
alias hi='printf "%b%s%b\n" "\e[1;31m" "I Love You" "\e[0m"'
