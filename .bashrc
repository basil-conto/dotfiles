# ~/.bashrc: executed by bash(1) for non-login shells.
# See /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
  *i*) ;;
    *) return ;;
esac

# Don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# Append to the history file, don't overwrite it
shopt -s histappend

# For setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# # If set, the pattern "**" used in a pathname expansion context will
# # match all files and zero or more directories and subdirectories.
# shopt -s globstar

# Make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Enable ls colour support
[ -x /usr/bin/dircolors ]                \
  && [ -r ~/.dircolors  ]                \
  && eval "$(dircolors -b ~/.dircolors)" \
  || eval "$(dircolors -b)"

# Alias definitions.
[ -f ~/.bash_aliases    ] && . ~/.bash_aliases
# Colour definitions
[ -f ~/.bash_colours    ] && . ~/.bash_colours
# User completions
[ -f ~/.bash_completion ] && . ~/.bash_completion

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
#
# ┌[blc@t430-mint] (master) ~/dotfiles [1]
# └$

# Set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
  debian_chroot=$(cat /etc/debian_chroot)
fi

git_ps1() {
  local rc=$?
  printf "$(__git_ps1)"
  return $rc
}

brack_hi='\342\224\214'
brack_lo='\342\224\224'

PS1="\[${WHT_BF}\]${brack_hi}[\[${GRN_BF}\]\u@\h${WHT_BF}]"
PS1+="\[${YLW_BF}\]\$(git_ps1)"
PS1+="\[${BLU_BF}\] \w "
PS1+="\[${PRP}\]${debian_chroot:+($debian_chroot)}"
PS1+="\[${RED}\]\$(RC=\$?; [ \$RC -ne 0 ] && printf [\$RC])\n"
PS1+="\[${WHT_BF}\]${brack_lo}\[${BLU_BF}\]\$ \[${RESET}\]"

# Don't pollute the environment
bash_colours_unset
unset brack_hi brack_lo
unset -f bash_colours_unset

# Enable colour and special key support in tmux
[ -z "${TMUX}" ] && export TERM=xterm-256color

# Allow safe usage of boolean expressions without spamming error return codes;
# actual errors should (hopefully) manifest by other means
true
