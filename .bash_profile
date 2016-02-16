# ~/.bash_profile

# Startup files
[ -f ~/.profile ] && . ~/.profile
[ -f ~/.bashrc  ] && . ~/.bashrc

if [ -f "${BREW_PREFIX}/etc/bash_completion" ]; then
  . "${BREW_PREFIX}/etc/bash_completion"
fi

true
