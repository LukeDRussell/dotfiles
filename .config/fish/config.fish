# Use pyenv shim if it exists
if command -v pyenv 1>/dev/null 2>&1
  pyenv init - | source
end

# Use neovim if it exists
if command -sq nvim 
  alias vi=nvim
  alias vim=nvim
end

# Use exa if it exists
if command -sq exa
  alias ls=exa
  alias ll="exa -l"
  alias tree="exa --tree"
end

# Disable welcome
set -U fish_greeting

# Use vim bindings
fish_vi_key_bindings

