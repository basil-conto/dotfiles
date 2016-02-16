# ~/.bash_profile

# Startup files
[ -f ~/.profile ] && . ~/.profile
[ -f ~/.bashrc  ] && . ~/.bashrc

if [ -f "${BREW_PREFIX}/etc/bash_completion" ]; then
  . "${BREW_PREFIX}/etc/bash_completion"
fi

# Fit sh-prompt options
for flag in DIRTYSTATE STASHSTATE UPSTREAM; do
  export "GIT_PS1_SHOW${flag}"=auto
done

true
