# Alias

alias vi=nvim
alias vim=nvim
alias ls=exa
alias ll="exa -l"

# Settings
fish_vi_key_bindings
status --is-interactive; and pyenv init - | source
status --is-interactive; and pyenv virtualenv-init - | source
