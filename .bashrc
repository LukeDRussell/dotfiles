# .bashrc

# Enable pyenv
export PATH="~/.pyenv/bin:$PATH"
 eval "$(pyenv init -)"
 eval "$(pyenv virtualenv-init -)"

# vim mode
set -o vi
bind '"jj":vi-movement-mode'

# Aliases

alias vim='nvim'
alias vi='nvim'
alias ll='ls -Fl'
alias cd..='cd ../'
alias .1='cd ../'
alias .2='cd ../../'
alias .3='cd ../../../'
alias context='date ; whoami ; hostname ; pwd'
alias play='ansible-playbook'
alias gitlog='git log --oneline --decorate --all'
alias jekserve='bundle exec jekyll serve -wIo'
alias fif='findinfile'
alias fifl='findinfileline'

# Functions
findinfile() {
    grep -lir --color --exclude-dir=.git "$1" .
}

findinfileline() {
    grep -ir --color --exclude-dir=.git "$1" .
}

# get confirmation before deleting things
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

# create directories recursively
alias mkdir='mkdir -p'

# MacOS settings
if [[ $(uname -s) == Darwin ]]; then
        export CLICOLOR=1
        export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
        [ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion
fi

# default editor
export VISUAL=nvim
export EDITOR="$VISUAL"

# prompt
source .promptline.sh
