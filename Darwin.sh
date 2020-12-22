# Homebrew (requires clicking confirmation for xcode CLI tools)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# CLI Apps
brew install pyenv pyenv-virtualenv exa tmux jq git ansible fish tldr

# GUI Apps
brew install --cask visual-studio-code iterm2

# Link Fish config
mkdir ~/.config
ln -s ~/dotfiles/.config/fish ~/.config/fish
ln -s ~/dotfiles/.gitconfig ~/.gitconfig
ln -s ~/dotfiles/.tmux.conf ~/.tmux.conf

# Disable silly keyboard shortcuts
defaults write -g ApplePressAndHoldEnabled -bool false

