# Start Tmux
tmux attach-session || tmux new-session
register-python-argcomplete --shell fish pipx | .
# Register pipx completions


# Alias

alias vi=nvim
alias vim=nvim
alias ls=exa
alias ll="exa -l"

# Settings
fish_vi_key_bindings

# Created by `userpath` on 2020-08-20 21:12:17
set PATH $PATH /home/luke/.local/bin
