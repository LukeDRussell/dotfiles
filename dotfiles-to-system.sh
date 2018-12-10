# This is a general-purpose function to ask Yes/No questions in Bash, either
# with or without a default answer. It keeps repeating the question until it
# gets a valid answer.

ask() {
    # https://gist.github.com/davejamesmiller/1965569
    local prompt default reply

    if [ "${2:-}" = "Y" ]; then
        prompt="Y/n"
        default=Y
    elif [ "${2:-}" = "N" ]; then
        prompt="y/N"
        default=N
    else
        prompt="y/n"
        default=
    fi

    while true; do

        # Ask the question (not using "read -p" as it uses stderr not stdout)
        echo -n "$1 [$prompt] "

        # Read the answer (use /dev/tty in case stdin is redirected from somewhere else)
        read reply </dev/tty

        # Default?
        if [ -z "$reply" ]; then
            reply=$default
        fi

        # Check if the reply is valid
        case "$reply" in
            Y*|y*) return 0 ;;
            N*|n*) return 1 ;;
        esac

    done
}

if ask "This script will overwrite files. Are you sure?" Y; then
    echo "Yes"
else
    echo "No"
fi

echo ""
echo "Copying settings files from dotfiles to ~"
echo ""

echo "Copying bash settings..."
cp ~/repos/dotfiles/bashrc ~/.bashrc 

echo "Copying neovim settings..."
cp ~/repos/dotfiles/init.vim ~/.config/nvim/init.vim

echo "Copying tmux settings..."
cp ~/repos/dotfiles/tmux.conf ~/.tmux.conf

echo "Copying global git settings..."
cp ~/repos/dotfiles/gitconfig ~/.gitconfig

echo "Copying powerline status bar settings..."
cp -r ~/repos/dotfiles/powerline ~/.config/

echo "Copy markdown to pdf styles..."
cp -r ~/repos/dotfiles/mume ~/.mume/
