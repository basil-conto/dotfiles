# ~/.bash_profile

# Startup files
[ -f ~/.profile ] && . ~/.profile
[ -f ~/.bashrc  ] && . ~/.bashrc

# brew bash completion
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

true
