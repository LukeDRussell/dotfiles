# Alias

if command -v nvim 1>/dev/null 2>&1
  alias vi=nvim
  alias vim=nvim
end

if command -v exa 1>/dev/null 2>&1
  alias ls=exa
  alias ll="exa -l"
  alias tree="exa --tree"
end


# Settings
fish_vi_key_bindings

# Add Pyenv folder to $PATH if doesn't exist
fish_add_path ~/.pyenv/bin/

# pyenv init
if command -v pyenv 1>/dev/null 2>&1
  pyenv init - | source
end

# Created by `pipx` on 2021-04-01 03:56:48
set PATH $PATH /home/4032956/.local/bin
