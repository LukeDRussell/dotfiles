echo ""
echo "Copying settings files from ~ to dotfiles"
echo ""

echo "Copying bash settings..."
cp ~/.bashrc ~/repos/dotfiles/.bashrc
cp ~/.inputrc ~/repos/dotfiles/.inputrc

echo "Copying neovim settings..."
cp ~/.config/nvim/init.vim ~/repos/dotfiles/.config/nvim/init.vim

echo "Copying tmux settings..."
cp ~/.tmux.conf ~/repos/dotfiles/.tmux.conf

echo "Copying global git settings..."
cp ~/.gitconfig ~/repos/dotfiles/.gitconfig

echo "Copying powerline status bar settings..."
cp -r ~/.config/powerline ~/repos/dotfiles/.config/
