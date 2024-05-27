# Disable welcome
set -U fish_greeting

set EDITOR emacs
set SUDO_EDITOR $EDITOR

# Enable vim-like 
fish_vi_key_bindings
set fish_cursor_default block
set fish_cursor_insert line
set fish_cursor_replace_one underscore
set fish_cursor_visual block
set fish_vi_force_cursor

# Add user installed binaries to path
fish_add_path ~/.local/bin/
fish_add_path ~/bin/
fish_add_path "$HOME/.rye/shims"

# neovim
if command -sq nvim
  alias vi=nvim
  alias vim=nvim
end

if command -sq gls
  alias ls="gls"
end

# lsd
if command -sq lsd
  alias ll="lsd --long --header --git --classify --group-directories-first"
  alias tree="ll --level 3 --ignore-glob=.git/ --color=always"
  abbr lla "ll --all"
  abbr llm "ll --sort=modified"
  abbr lls "ll --sort=size"
else
  alias ll="ls -lh"
  alias lla="ls -lha"
  alias llm="ls --sort=modified"
  alias lls="ls --sort=size"
end

alias play="ansible-playbook"

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

