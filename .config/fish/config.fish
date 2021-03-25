# Alias

if command -sq nvim 
  alias vi=nvim
  alias vim=nvim
end

if command -sq exa
  alias ls=exa
  alias ll="exa -l"
  alias tree="exa --tree"
end


# Settings
fish_vi_key_bindings

# pyenv init
if command -v pyenv 1>/dev/null 2>&1
  pyenv init - | source
end
