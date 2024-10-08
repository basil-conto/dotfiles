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
--borderless     \
--draw-filename  \
--image-bg black \
--no-fehbg       \
--scale-down     \
--sort filename'

# xbacklight
alias dark='blight dec 100 --sweep'
alias half='brightnessctl set 50%'
alias bright='blight inc 100 --sweep'
alias dim='brightnessctl set 10%'
alias brighten='blight inc 10 --sweep'

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

# fun fun fun
alias hi='printf "%b%s%b\n" "\e[1;31m" "I Love You" "\e[0m"'
