# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

if [ -z "${BASH_VERSION}" ]; then
  opt=p
else
  opt=u
  [ -r "${HOME}/.bashrc" ] && . "${HOME}/.bashrc"
fi
ulimit -${opt} 1024
unset opt

# Rubbish (bin)
[ -d "${HOME}/.cabal/bin" ] && PATH="${HOME}/.cabal/bin:${PATH}"
[ -d "${HOME}/.local/bin" ] && PATH="${HOME}/.local/bin:${PATH}"
[ -d "${HOME}/bin"        ] && PATH="${HOME}/bin:${PATH}"
export PATH

export LOCAL_DIR="${HOME}/.local" # Local installations
export N_PREFIX="${LOCAL_DIR}"    # https://github.com/tj/n

export EDITOR='emacs -nw'         # Used by crontab et al.
export CACA_DRIVER='ncurses'      # libcaca

# Pyenv
if [ -d "${HOME}/.pyenv" ]; then
  export PYENV_ROOT="${HOME}/.pyenv"
  export PATH="${PYENV_ROOT}/bin:${PATH}"
  eval "$(pyenv init -)"
fi

# Allow safe usage of boolean expressions without spamming error return codes;
# actual errors should (hopefully) manifest by other means
true
