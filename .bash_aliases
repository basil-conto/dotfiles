# -*- sh -*-
# ~/.bash_aliases

# safety
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -I'

# ls
alias ls='ls --color=auto --group-directories-first'
alias l='ls -F'
alias ll='l -lh'
alias la='ll -A'

# grep
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# brew
alias uu='brew update && brew upgrade $(brew outdated)'

# GNU compilers
alias gcc='gcc -fdiagnostics-color'
alias g++='g++ -fdiagnostics-color'
alias gcc99='gcc -std=c99'
alias gcc11='gcc -std=c11'
alias g++03='g++ -std=c++03'
alias g++11='g++ -std=c++11'

# emacsen
alias em='emacs -nw'
alias ec='emacsclient -a "" -t'
alias ecc='emacsclient -a "" -c -n'

# colordiff
alias ciff='colordiff -y -W 160'

# virtualbox
alias startvm='/Applications/VirtualBox.app/Contents/MacOS/VBoxManage startvm'

# youtube-dl
alias ydl='youtube-dl --prefer-ffmpeg'
alias ydl-mp3='ydl -x --audio-format mp3 --audio-quality 256K'
alias ydl-vorbis='ydl -x --audio-format vorbis --audio-quality 128K'

# chromium
alias chromium-proxy='chromium --proxy-auto-detect &> /dev/null &'

# vlc
alias vlc='VLC_PLUGIN_PATH=/opt/homebrew-cask/Caskroom/vlc/2.2.1/VLC.app/Contents/MacOS/plugins vlc'
alias nvlc='vlc -I ncurses --browse-dir .'

# gcal
alias gcal='gcal -s 1'

# fun fun fun
alias hi='printf "%b%s%b\n" "\e[1;31m" "I Love You" "\e[0m"'
