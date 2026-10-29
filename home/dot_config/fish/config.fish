# Disable welcome
set -U fish_greeting

set EDITOR nvim
set SUDO_EDITOR $EDITOR

# Enable vim-like 
fish_vi_key_bindings
set fish_cursor_default block
set fish_cursor_insert line
set fish_cursor_replace_one underscore
set fish_cursor_visual block
set fish_vi_force_cursor

fish_add_path ~/.local/bin/

if command -sq nvim
  alias vi=nvim
  alias vim=nvim
end

if command -sq gls
  alias ls="gls"
end

# lsd
if type -q lsd
  alias ls='lsd'
  alias l1="lsd -1"
  alias ll="lsd --long --header --git --classify --group-directories-first"
  alias tree="lsd --tree"
  abbr lla "ll --all"
  abbr llt "ll --timesort"
  abbr lls "ll --sizesort"
else
  alias ll="ls -lh"
  alias lla="ls -lha"
  alias llm="ls --sort=modified"
  alias lls="ls --sort=size"
end

alias play="uv run ansible-playbook"
alias tm="tmux new -A -s main"

# Git Abbreviations
abbr -a gitco git checkout
abbr -a gita git add
abbr -a gitaa git add .
abbr -a gitca git commit -a -m \"
abbr -a gits git status
abbr -a gitl git log --oneline --decorate --all --graph
abbr -a gitp git pull --rebase

zoxide init fish | source

source ~/.config/fish/work.fish

