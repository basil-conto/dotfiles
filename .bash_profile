# -*- sh -*-
# ~/.bash_profile

#
# Startup files
#

[ -f ~/.profile ] && . ~/.profile
[ -f ~/.bashrc  ] && . ~/.bashrc

#
# Env variables
#

# Used, for example, by crontab
EDITOR='emacs -nw'

for formula in core find; do
  PATH="/usr/local/opt/${formula}utils/libexec/gnubin:${PATH}"
  MANPATH="/usr/local/opt/${formula}utils/libexec/gnuman:${MANPATH}"
done

PATH="${HOME}/.pyenv/versions/2.7.8/bin:${PATH}"

[ -d "${HOME}/bin"        ] && PATH="${HOME}/bin:${PATH}"
[ -d "${HOME}/.cabal/bin" ] && PATH="${HOME}/.cabal/bin:${PATH}"

export EDITOR PATH MANPATH

#
# brew bash completion
#

if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

true
