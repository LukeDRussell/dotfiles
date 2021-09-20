# Disable welcome
set -U fish_greeting

# Enable vim-like 
fish_vi_key_bindings
set fish_cursor_default block
set fish_cursor_insert line
set fish_cursor_replace_one underscore
set fish_cursor_visual block
set fish_vi_force_cursor

# Add user installed binaries to path
fish_add_path ~/.local/bin/

# Pyenv
if command -sq pyenv
  status is-interactive; and pyenv init --path | source
  pyenv init - | source
end


# neovim
if command -sq nvim
  alias vi=nvim
  alias vim=nvim
end


# exa
if command -sq exa
  alias ls="exa"
  alias ll="ls --long --header --git --classify --tree --level 1"
  alias tree="ll --level 3 --ignore-glob=.git/ --color=always"
  abbr lla "ll --all"
  abbr llm "ll --sort=modified"
  abbr lls "ll --sort=size"
else
  alias ll="ls -l"
  alias lla="ls la"
  alias llm="ls --sort=modified"
  alias lls="ls --sort=size"
end

# Use "play" to pull credentials from ~
alias play="ansible-playbook -e @~/.secrets.yml"

# Git Abbreviations
abbr -a gitco git checkout
abbr -a gita git add
abbr -a gitaa git add .
abbr -a gitca git commit -a -m \"
abbr -a gits git status
abbr -a gitl git log --oneline --decorate --all --graph

