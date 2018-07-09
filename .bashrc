# .bashrc

# User specific aliases and functions

alias vim='nvim'
alias vi='nvim'
alias ll='ls -Fl'
alias cd..='cd ../'
alias .1='cd ../'
alias .2='cd ../../'
alias .3='cd ../../../'
alias context='date ; whoami ; hostname ; pwd'

# Git branch in prompt.
parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}


# exports

export PS1="\w\[\033[32m\]\$(parse_git_branch)\[\033[00m\] $ "
export PATH=/home/4032956/.fwd/bin:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/home/4032956/.local/bin:/home/4032956/bin


# terminal.app colouring
export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
