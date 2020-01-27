echo ""
echo "Copying settings files from ~ to dotfiles"
echo ""

echo "Copying bash settings..."
cp ~/.bashrc ~/repos/dotfiles/.bashrc
cp ~/.inputrc ~/repos/dotfiles/.inputrc
cp ~/.promptline.sh ~/repos/dotfiles/.promptline.sh

echo "Copying neovim settings..."
cp ~/.config/nvim/init.vim ~/repos/dotfiles/.config/nvim/init.vim

echo "Copying tmux settings..."
cp ~/.tmux.conf ~/repos/dotfiles/.tmux.conf
cp ~/.tmuxline.conf ~/repos/dotfiles/.tmuxline.conf

echo "Copying global git settings..."
cp ~/.gitconfig ~/repos/dotfiles/.gitconfig
