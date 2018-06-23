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
# User completions
[ -r ~/.bash_completion ] && . ~/.bash_completion

# Prompt
#
# ┌[blc@thunk] (master) ~/dotfiles [1]
# └$

blc_prompt() {
  local rc=$?
  [ "${rc}" -eq 0 ] && rc='' || rc="\[\e[31m\] [${rc}]"

  PS1='\[\e[0m\]\342\224\214[\[\e[1;32m\]\u@\h\[\e[0m\]]'
  PS1+="\[\e[1;33m\]\$(__git_ps1) \[\e[34m\]\w${rc}\n"
  PS1+='\[\e[0m\]\342\224\224\[\e[1;34m\]\$\[\e[0m\] '
}

PROMPT_COMMAND=blc_prompt

# Git sh-prompt options
export GIT_PS1_SHOW{DIRTYSTATE,STASHSTATE,UPSTREAM}=auto

# For gpg-agent
export GPG_TTY="$(tty)"

# Allow safe usage of boolean expressions without spamming error return codes;
# actual errors should (hopefully) manifest by other means
true
