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
alias play='ansible-playbook -k'
alias gitlog='git log --oneline --decorate --all'

# get confirmation before deleting things
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

# create directories recursively
alias mkdir='mkdir -p'

# shell prompt
# export PS1="\W \\$ \[$(tput sgr0)\]"
export PS1="\[\033[38;5;1m\]\u\[$(tput sgr0)\]\[\033[38;5;15m\] on \[$(tput sgr0)\]\[\033[38;5;3m\]\h\[$(tput sgr0)\]\[\033[38;5;15m\] at \[$(tput sgr0)\]\[\033[38;5;2m\]\w\[$(tput sgr0)\]\[\033[38;5;15m\]\n\\$ \[$(tput sgr0)\]"

# terminal.app colouring
export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx

# default editor
export VISUAL=nvim
export EDITOR="$VISUAL"

