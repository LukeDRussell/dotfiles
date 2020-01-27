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
#alias poetry_shell='. "$(dirname $(poetry run which python))/activate"'

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


# Functions

COLOR_DEFAULT="\e[39m"
COLOR_BLACK="\e[30m"
COLOR_RED="\e[31m"
COLOR_GREEN="\e[32m"
COLOR_YELLOW="\e[33m"
COLOR_BLUE="\e[34m"
COLOR_MAGENTA="\e[35m"
COLOR_CYAN="\e[36m"
COLOR_LIGHTGRAY="\e[37m"
COLOR_DARKGRAY="\e[90m"
COLOR_LIGHTRED="\e[91m"
COLOR_LIGHTGREEN="\e[92m"
COLOR_LIGHTYELLOW="\e[93m"
COLOR_LIGHTBLUE="\e[94m"
COLOR_LIGHTMAGENTA="\e[95m"
COLOR_LIGHTCYAN="\e[96m"
COLOR_WHITE="\e[97m"

function git_color {
  local git_status="$(git status 2> /dev/null)"

  if [[ ! $git_status =~ "working tree clean" ]]; then
    echo -e $COLOR_RED
  elif [[ $git_status =~ "Your branch is ahead of" ]]; then
    echo -e $COLOR_YELLOW
  elif [[ $git_status =~ "nothing to commit" ]]; then
    echo -e $COLOR_GREEN
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
export PS1="$COLOR_LIGHTRED\u$COLOR_DEFAULT@$COLOR_YELLOW\h$COLOR_DEFAULT in $COLOR_GREEN\w \[\$(git_color)\]\$(git_branch)$COLOR_DEFAULT\n$ "

