# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# When running bash
if [ -n "${BASH_VERSION}" ] && [ -f "${HOME}/.bashrc" ]; then
  . "${HOME}/.bashrc"
fi

# Prepend private and cabal bin directories
[ -d "${HOME}/bin"        ] && PATH="${HOME}/bin:${PATH}"
[ -d "${HOME}/.cabal/bin" ] && PATH="${HOME}/.cabal/bin:${PATH}"
export PATH

# Limit number of user processes
ulimit -u 1024

# Allow safe usage of boolean expressions without spamming error return codes;
# actual errors should (hopefully) manifest by other means
true
