# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
[ -z "${PS1}" ] && return

# Shell options & history
shopt -s checkwinsize histappend
HISTCONTROL=ignoreboth:erasedups
HISTSIZE=4096

# Make less more friendly for non-text input files, see lesspipe(1)
[ -x "$(command -v lesspipe)" ] && eval "$(SHELL=/bin/sh lesspipe)"

# Enable ls colour support
[ -x "$(command -v dircolors)" ]         \
  && [ -r "${HOME}/.dircolors" ]         \
  && eval "$(dircolors -b ~/.dircolors)" \
  || eval "$(dircolors -b)"

# Alias definitions
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
