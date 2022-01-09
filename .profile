# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.

[ -n "${BASH_VERSION}" ]      \
  && [ -r "${HOME}/.bashrc" ] \
  && . "${HOME}/.bashrc"

export ALTERNATE_EDITOR=              # Lazy emacs daemonisation
export CACA_DRIVER=ncurses            # libcaca
export EDITOR=ecc                     # Used by crontab et al.
export GCALCLI_DIR="${HOME}/.gcalcli" # gcalcli
export LOCAL_DIR="${HOME}/.local"     # Local installations
export N_PREFIX="${LOCAL_DIR}"        # https://github.com/tj/n
export PDFVIEWER='ecc --no-wait'      # texdoc
export PSVIEWER='ecc  --no-wait'      # texdoc
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-${HOME}/.config}"

# Rubbish (bin)
[ -d "${HOME}/.luarocks/bin" ] && PATH="${HOME}/.luarocks/bin:${PATH}"
[ -d "${HOME}/go/bin"        ] && PATH="${HOME}/go/bin:${PATH}"
[ -d "${HOME}/.cargo/bin"    ] && PATH="${HOME}/.cargo/bin:${PATH}"
[ -d "${HOME}/.cabal/bin"    ] && PATH="${HOME}/.cabal/bin:${PATH}"
[ -d "${LOCAL_DIR}/bin"      ] && PATH="${LOCAL_DIR}/bin:${PATH}"
[ -d "${HOME}/bin"           ] && PATH="${HOME}/bin:${PATH}"
export PATH

# Hoard (include)
[ -d "${LOCAL_DIR}/include" ] && export CPATH="${LOCAL_DIR}/include:${CPATH}"

# Books (lib)
if [ -d "${LOCAL_DIR}/lib" ]; then
  export LD_LIBRARY_PATH="${LOCAL_DIR}/lib:${LD_LIBRARY_PATH}"
fi

# Books (bib)
if [ -d "${HOME}/.bibs" ]; then
  export BIBINPUTS="$(realpath -- "${HOME}/.bibs"):${BIBINPUTS}"
fi

# Gnulib
if [ -d "${LOCAL_DIR}/src/gnulib" ]; then
  export GNULIB_SRCDIR="${LOCAL_DIR}/src/gnulib"
elif [ -d /usr/share/gnulib ]; then
  export GNULIB_SRCDIR=/usr/share/gnulib
fi

# OPAM (see ~/.opam/opam-init/init.sh)
if [ -r "${HOME}/.opam/opam-init/variables.sh" ]; then
  . "${HOME}/.opam/opam-init/variables.sh" >/dev/null 2>&1
fi

# Pyenv
if [ -d "${HOME}/.pyenv" ]; then
  PYENV_ROOT="${HOME}/.pyenv"
  for d in bin shims plugins/pyenv-virtualenv/shims; do
    [ -d "${PYENV_ROOT}/${d}" ] && PATH="${PYENV_ROOT}/${d}:${PATH}"
  done
  export PATH PYENV_ROOT
fi

# Allow safe usage of boolean expressions without spamming error return codes;
# actual errors should (hopefully) manifest by other means
true
