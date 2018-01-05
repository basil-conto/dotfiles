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

export ALTERNATE_EDITOR=              # Lazy emacs daemonisation
export CACA_DRIVER=ncurses            # libcaca
export EDITOR=ecc                     # Used by crontab et al.
export GCALCLI_DIR="${HOME}/.gcalcli" # gcalcli
export LOCAL_DIR="${HOME}/.local"     # Local installations
export N_PREFIX="${LOCAL_DIR}"        # https://github.com/tj/n
export TERM=xterm-256color            # Colours!
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-${HOME}/.config}"

# Rubbish (bin)
[ -d "${HOME}/go/bin"     ] && PATH="${HOME}/go/bin:${PATH}"
[ -d "${HOME}/.cabal/bin" ] && PATH="${HOME}/.cabal/bin:${PATH}"
[ -d "${LOCAL_DIR}/bin"   ] && PATH="${LOCAL_DIR}/bin:${PATH}"
[ -d "${HOME}/bin"        ] && PATH="${HOME}/bin:${PATH}"
export PATH

# Books (lib)
if [ -d "${LOCAL_DIR}/lib" ]; then
  export LD_LIBRARY_PATH="${LOCAL_DIR}/lib:${LD_LIBRARY_PATH}"
fi

# Pyenv
if [ -d "${HOME}/.pyenv" ]; then
  export PYENV_ROOT="${HOME}/.pyenv"
  export PATH="${PYENV_ROOT}/bin:${PATH}"
  eval "$(pyenv init -)"
fi

# Allow safe usage of boolean expressions without spamming error return codes;
# actual errors should (hopefully) manifest by other means
true
