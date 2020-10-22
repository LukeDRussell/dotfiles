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
status --is-interactive; and source (pyenv init -|psub)
