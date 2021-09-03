# Bootstrap Homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

brew install fish exa tmux jq git ansible tldr neovim python

brew install --cask visual-studio-code iterm2 drawio

# Symlink config files, run from this dir

ln -sf ~/Repos/dotfiles/gitconfig ~/.gitconfig
ln -sf ~/Repos/dotfiles/tmux.conf ~/.config/tmux/tmux.conf
ln -sf ~/Repos/dotfiles/config.fish ~/.config/fish/config.fish
ln -sf ~/Repos/dotfiles/init.lua ~/.config/nvim/init.lua

# Disable silly keyboard settings
defaults write -g ApplePressAndHoldEnabled -bool false

# disable automatic text changes as it’s annoying when typing code
defaults write NSGlobalDomain NSAutomaticCapitalizationEnabled -bool false
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false
defaults write NSGlobalDomain NSAutomaticPeriodSubstitutionEnabled -bool false
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

# Let me quit Finder		
defaults write com.apple.finder QuitMenuItem -bool true; killall Finder


# Change the default shell to fish
# build the latest version of python using pyenv
# set latest version of python to global with pyenv

# ITerm2 profiles to use Tmux (if session exists attach, otherwise create and attach)
# /usr/local/bin/tmux new -A

