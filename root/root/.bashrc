# ~/.bashrc: executed by bash(1) for non-login shells.

# Note: PS1 and umask are already set in /etc/profile. You should not
# need this unless you want different defaults for root.
# PS1='${debian_chroot:+($debian_chroot)}\h:\w\$ '
# umask 022

eval "`dircolors`"

export GREP_OPTIONS='--color=auto'

[ -r ~/.bash_aliases ] && . ~/.bash_aliases
[ -r ~/.bash_colours ] && . ~/.bash_colours

# Prompt
PS1="\[${PRP}\]${debian_chroot:+($debian_chroot)}\[${RESET}\]"
PS1+="[\[${RED_BF}\]\\h\[${RESET}\]] \[${BLU_BF}\]\\w \\$\[${RESET}\] "

# Don't pollute the environment
bash_colours_unset
unset -f bash_colours_unset

# Allow safe usage of boolean expressions without spamming error return codes;
# actual errors should (hopefully) manifest by other means
true
