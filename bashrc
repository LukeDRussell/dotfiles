# .bashrc

# vim mode bindings
set -o vi

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


# Powerline prompt
#if [ -f `which powerline-daemon` ]; then
#        powerline-daemon -q
#        POWERLINE_BASH_CONTINUATION=1
#        POWERLINE_BASH_SELECT=1
#        if [[ $(uname -s) == Linux ]]; then
#                . /usr/lib/python2.7/site-packages/powerline/bindings/bash/powerline.sh
#        elif [[ $(uname -s) == Darwin ]]; then
#                . /usr/local/lib/python3.7/site-packages/powerline/bindings/bash/powerline.sh
#        fi
#fi

# Functions
COLOR_RED="\033[0;31m"
COLOR_YELLOW="\033[0;33m"
COLOR_GREEN="\033[0;32m"
COLOR_OCHRE="\033[38;5;95m"
COLOR_BLUE="\033[0;34m"
COLOR_WHITE="\033[0;37m"
COLOR_RESET="\033[0m"

function git_color {
  local git_status="$(git status 2> /dev/null)"

  if [[ ! $git_status =~ "working tree clean" ]]; then
    echo -e $COLOR_RED
  elif [[ $git_status =~ "Your branch is ahead of" ]]; then
    echo -e $COLOR_YELLOW
  elif [[ $git_status =~ "nothing to commit" ]]; then
    echo -e $COLOR_GREEN
  else
    echo -e $COLOR_OCHRE
  fi
}

function git_branch {
  local git_status="$(git status 2> /dev/null)"
  local on_branch="On branch ([^${IFS}]*)"
  local on_commit="HEAD detached at ([^${IFS}]*)"

  if [[ $git_status =~ $on_branch ]]; then
    local branch=${BASH_REMATCH[1]}
    echo "($branch)"
  elif [[ $git_status =~ $on_commit ]]; then
    local commit=${BASH_REMATCH[1]}
    echo "($commit)"
  fi
}

# Normal bash prompt
export PS1="\[\033[38;5;1m\]\u\[$(tput sgr0)\]\[\033[38;5;15m\] on \[$(tput sgr0)\]\[\033[38;5;3m\]\h\[$(tput sgr0)\]\[\033[38;5;15m\] at \[$(tput sgr0)\]\[\033[38;5;2m\]\w\[$(tput sgr0)\]\[\033[38;5;15m\] \[\$(git_color)\]\$(git_branch) \[\033[38;5;15m\]\n\\$ "

