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

# get confirmation before deleting things
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

# create directories recursively
alias mkdir='mkdir -p'

# shell prompt
export PS1="\W \\$ \[$(tput sgr0)\]"

# terminal.app colouring
export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
