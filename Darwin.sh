# Homebrew (requires clicking confirmation for xcode CLI tools)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# CLI Apps
brew install pyenv pyenv-virtualenv exa tmux jq git ansible fish tldr neovim

# GUI Apps
brew install --cask visual-studio-code iterm2 drawio

# Link Fish config
mkdir ~/.config
ln -s ~/Repos/dotfiles/.config/fish ~/.config/fish
ln -s ~/Repos/dotfiles/.gitconfig ~/.gitconfig
ln -s ~/Repos/dotfiles/.tmux.conf ~/.tmux.conf

# Disable silly keyboard shortcuts
defaults write -g ApplePressAndHoldEnabled -bool false

# Change the default shell to fish
# build the latest version of python using pyenv
# set latest version of python to global with pyenv

# ITerm2 profiles to use Tmux (if session exists attach, otherwise create and attach)
# /usr/local/bin/tmux new -A

# Some MacOS built apps use the ancient built-in ncurses, which is missing tmux-256 terminal profile.
# /usr/bin/tic -x tmux-256color
