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

# Pyenv
export PYENV_SHELL=bash
export PYENV_VIRTUALENV_INIT=1
command pyenv rehash 2>/dev/null

pyenv() {
  local command
  command="${1:-}"
  if [ "$#" -gt 0 ]; then
    shift
  fi

  case "$command" in
  activate|deactivate|rehash|shell)
    eval "$(pyenv "sh-$command" "$@")";;
  *)
    command pyenv "$command" "$@";;
  esac
}

_pyenv_virtualenv_hook() {
  local ret=$?
  if [ -n "$VIRTUAL_ENV" ]; then
    eval "$(pyenv sh-activate --quiet || pyenv sh-deactivate --quiet || true)" \
      || true
  else
    eval "$(pyenv sh-activate --quiet || true)" || true
  fi
  return $ret
}

if ! [[ "$PROMPT_COMMAND" =~ _pyenv_virtualenv_hook ]]; then
  PROMPT_COMMAND="_pyenv_virtualenv_hook;$PROMPT_COMMAND";
fi

# Alias definitions
[ -r ~/.bash_aliases    ] && . ~/.bash_aliases
# User completions
[ -r ~/.bash_completion ] && . ~/.bash_completion

# Allow safe usage of boolean expressions without spamming error return codes;
# actual errors should (hopefully) manifest by other means
true
