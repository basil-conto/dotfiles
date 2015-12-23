# ~/.bashrc: executed by bash(1) for non-login shells.

# Note: PS1 and umask are already set in /etc/profile. You should not
# need this unless you want different defaults for root.
# PS1='${debian_chroot:+($debian_chroot)}\h:\w\$ '
# umask 022

eval "`dircolors`"

export GREP_OPTIONS='--color=auto'

alias ls='ls --color=auto --group-directories-first'
alias l='ls -F'
alias ll='l -lh'
alias la='ll -A'

# Some more alias to avoid making mistakes:
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

# nano with smart home and 2-space tabs
alias nano='nano -A -E -T2'

alias em='emacs -nw'
