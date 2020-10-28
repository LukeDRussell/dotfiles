# dotfiles

```sh
## Enpass
cd /etc/yum.repos.d/
sudo wget https://yum.enpass.io/enpass-yum.repo
sudo yum install enpass -y

## Hack Fonts
sudo dnf copr enable zawertun/hack-fonts
sudo dnf install hack-fonts -y

## Fedora Better Fonts
sudo dnf copr enable samuelig/better_fonts
sudo dnf install fontconfig-enhanced-defaults fontconfig-font-replacements -y

## Vim-Plug (Neovim)
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

## Test terminal is working:
echo -e "\e[3m foo \e[23m"

## Remap CAPS to CTRL
gsettings set org.gnome.desktop.input-sources xkb-options "['caps:ctrl_modifier']"

## Terminal Always use TMUX command
#tmux new -A
```
