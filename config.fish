# Disable welcome
set -U fish_greeting

# Enable vim-like 
fish_vi_key_bindings
set fish_cursor_default block
set fish_cursor_insert line
set fish_cursor_replace_one underscore
set fish_cursor_visual block
set fish_vi_force_cursor

# Pyenv
if command -sq pyenv
  status is-interactive; and pyenv init --path | source
  pyenv init - | source
  status is-interactive; and pyenv-virtualenv-init - | source
  pyenv-virtualenv-init - | source
end

# pipx
if command -sq pipx
  fish_add_path ~/.local/bin/
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
end

# Ansible
if command -sq ansible
  alias play="ansible-playbook"
  alias play_askpass="ansible-playbook -k"
  alias play_vaultpass="ansible-playbook -e @~/.vault.yml --vault-password-file ~/.vault-pw"
end

# Git Abbreviations
abbr -a gco git checkout
abbr -a ga git add
abbr -a gaa git add .
abbr -a gca git commit-all


