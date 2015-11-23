# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
  *i*) ;;
    *) return ;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# # If set, the pattern "**" used in a pathname expansion context will
# # match all files and zero or more directories and subdirectories.
# shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# # If this is an xterm set the title to user@host:dir
# case "$TERM" in
#   xterm*|rxvt*)
#     PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
#     ;;
#   *)
#     ;;
# esac

# # set variable identifying the chroot you work in (used in the prompt below)
# if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
#   debian_chroot=$(cat /etc/debian_chroot)
# fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolors                     \
    && eval "$(dircolors -b ~/.dircolors)" \
    || eval "$(dircolors -b)"
fi

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f ~/.bash_aliases ]; then
  . ~/.bash_aliases
fi

# Colour definitions
if [ -f ~/.bash_colours ]; then
  . ~/.bash_colours
fi

# User completions
if [ -f ~/.bash_completion ]; then
  . ~/.bash_completion
fi

# Colour man pages
man() {
  env LESS_TERMCAP_mb=$'\E[01;31m'       \
      LESS_TERMCAP_md=$'\E[01;38;5;74m'  \
      LESS_TERMCAP_me=$'\E[0m'           \
      LESS_TERMCAP_se=$'\E[0m'           \
      LESS_TERMCAP_so=$'\E[38;5;246m'    \
      LESS_TERMCAP_ue=$'\E[0m'           \
      LESS_TERMCAP_us=$'\E[04;38;5;146m' \
      man "$@"
}

# Prompt

# ┌[blc@t430-mint] ~/Documents
# └$

brack_hi='\342\224\214'
brack_lo='\342\224\224'

PS1="\[${WHT_BF}\]${brack_hi}[\[${GRN_BF}\]\u@\h${WHT_BF}]\[${BLU_BF}\] \w "
PS1+="\[${PRP}\]${debian_chroot:+($debian_chroot)}"
PS1+="\[${RED}\]\$(RC=\$?; [[ \$RC != 0 ]] && echo -n [\$RC])"
PS1+="\n\[${WHT_BF}\]${brack_lo}\[${BLU_BF}\]\$ \[${RESET}\]"

# Don't pollute the environment
bash_colours_unset
unset brack_hi brack_lo
unset -f bash_colours_unset

# Helps with colours and special keys in tmux
if [ -z "${TMUX}" ]; then
  export TERM=xterm-256color
fi
