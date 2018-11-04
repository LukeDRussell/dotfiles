# .bashrc

# vim mode bindings
set -o vi

# User specific aliases and functions

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

# get confirmation before deleting things
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

# create directories recursively
alias mkdir='mkdir -p'

# shell prompt

# MacOS colouring
export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx

# default editor
export VISUAL=nvim
export EDITOR="$VISUAL"

powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
. /usr/local/lib/python3.7/site-packages/powerline/bindings/bash/powerline.sh
