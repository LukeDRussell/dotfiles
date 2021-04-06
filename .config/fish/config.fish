# fish_add_path requires fish 3.2.1

# PyEnv
fish_add_path ~/.pyenv/bin/
if command -sq pyenv
  pyenv init - | source
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

# exa exa over ls if it exists
if command -sq exa
  alias ls=exa
  alias ll="exa -l"
  alias tree="exa --tree"
end

# Disable welcome
set -U fish_greeting

# Use vim bindings
fish_vi_key_bindings

# Ansible
if command -sq ansible
  alias play_askpass="ansible-playbook -k"
  alias play_vaultpass="ansible-playbook -e @~/.vault.yml --vault-password-file ~/.vault-pw"
end