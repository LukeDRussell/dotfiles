# Alias

alias vi=nvim
alias vim=nvim
alias ls=exa
alias ll="exa -l"
alias tree="exa --tree"

# Use pyenv shim if it exists
if command -v pyenv 1>/dev/null 2>&1
  pyenv init - | source
end

# Disable welcome
set -U fish_greeting

# Use vim bindings
fish_vi_key_bindings
