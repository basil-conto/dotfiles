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
[ -r ~/.bash_aliases    ] && . ~/.bash_aliases
# Colour definitions
[ -r ~/.bash_colours    ] && . ~/.bash_colours
# User completions
[ -r ~/.bash_completion ] && . ~/.bash_completion

# Colour man pages
# FIXME: Use ${BLC_COLOURS}?
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
# ┌[blc@thunk] (master) ~/dotfiles [1]
# └$

blc_prompt() {
  local rc=$?
  [ "${rc}" -eq 0 ] && rc='' || rc=" \\[${BLC_COLOURS[red]}\\][${rc}]"
  [ "${BLC_DARK}" -eq 0 ] && fg=blk_bf || fg=wht_bf
  fg="${BLC_COLOURS[${fg}]}"

  PS1="\\[${fg}\\]\342\224\214[\\[${BLC_COLOURS[grn_bf]}\\]\\u@\\h${fg}]"
  PS1+="\\[${BLC_COLOURS[ylw_bf]}\\]\$(__git_ps1)"
  PS1+="\\[${BLC_COLOURS[blu_bf]}\\] \\w${rc}\n\\[${fg}\\]\342\224\224"
  PS1+="\\[${BLC_COLOURS[blu_bf]}\\]\\$ \\[${BLC_COLOURS[reset]}\\]"
}

PROMPT_COMMAND=blc_prompt

# Git sh-prompt options
export GIT_PS1_SHOW{DIRTYSTATE,STASHSTATE,UPSTREAM}=auto

# For gpg-agent
export GPG_TTY="$(tty)"

# Allow safe usage of boolean expressions without spamming error return codes;
# actual errors should (hopefully) manifest by other means
true
