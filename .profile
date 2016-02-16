# ~/.profile

# Used, for example, by crontab
EDITOR='emacs -nw'

# Homebrew + GNU tools
BREW_PREFIX="$(brew --prefix)"

for formula in coreutils findutils gnu-sed gnu-tar; do
  PATH="${BREW_PREFIX}/opt/${formula}/libexec/gnubin:${PATH}"
  MANPATH="${BREW_PREFIX}/opt/${formula}/libexec/gnuman:${MANPATH}"
done

PATH="${HOME}/.pyenv/versions/2.7.8/bin:${PATH}"

[ -d "${HOME}/bin"        ] && PATH="${HOME}/bin:${PATH}"
[ -d "${HOME}/.cabal/bin" ] && PATH="${HOME}/.cabal/bin:${PATH}"

export EDITOR PATH MANPATH BREW_PREFIX
