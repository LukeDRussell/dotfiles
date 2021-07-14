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

# exa over ls if it exists
if command -sq exa
  alias ls="exa"
  alias ll="exa --long --header --git"
  alias la="exa --long --header --git --all"
  alias lt="exa --long --header --git --sort=accessed"
  alias tree="exa --long --header --git --tree --level 3 --ignore-glob=.git/* --color=always | less -R"
else
  alias ll="ls -l"
end

# Ansible
if command -sq ansible
  alias play="ansible-playbook"
  alias play_askpass="ansible-playbook -k"
  alias play_vaultpass="ansible-playbook -e @~/.vault.yml --vault-password-file ~/.vault-pw"
end

