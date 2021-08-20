# Bootstrap Homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

brew install fish exa tmux jq git ansible fish tldr neovim

brew install --cask visual-studio-code iterm2 drawio

# Symlink config files

ln -sf gitconfig ~/.gitconfig
ln -sf tmux.conf ~/.tmux.conf
ln -sf config.fish ~/.config/fish/config.fish
ln -sf init.lua ~/.config/nvim/init.lua

# Disable silly keyboard shortcuts
defaults write -g ApplePressAndHoldEnabled -bool false

# Some MacOS built apps use the ancient built-in ncurses, which is missing tmux-256 terminal profile.
/usr/bin/tic -x tmux-256color

# Change the default shell to fish
# build the latest version of python using pyenv
# set latest version of python to global with pyenv

# ITerm2 profiles to use Tmux (if session exists attach, otherwise create and attach)
# /usr/local/bin/tmux new -A

