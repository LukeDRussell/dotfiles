# Alias

alias vi=nvim
alias vim=nvim
alias ls=exa
alias ll="exa -l"

# Settings
fish_vi_key_bindings

# Pyenv
set -Ux PYENV_ROOT $HOME/.pyenv
set -Ux fish_user_paths $PYENV_ROOT/bin $fish_user_paths

if command -v pyenv 1>/dev/null 2>&1
  pyenv init - | source
end

# Pipx
set PATH $PATH /home/luke/.local/bin
register-python-argcomplete --shell fish pipx | source
