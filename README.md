# dotfiles

## First Time
```sh
pip install pipx
pipx install ansible
pipx install glances
pipx install poetry
```

## Enpass
```sh
cd /etc/yum.repos.d/
sudo wget https://yum.enpass.io/enpass-yum.repo
sudo yum install enpass
```

## Hack Fonts
```sh
sudo dnf copr enable zawertun/hack-fonts
sudo dnf install hack-fonts
```

## Fedora Better Fonts
```sh
dnf copr enable samuelig/better_fonts
dnf install fontconfig-enhanced-defaults fontconfig-font-replacements
```

## Vim-Plug (Neovim)
```sh
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
```

## Test terminal is working:
```sh
echo -e "\e[3m foo \e[23m"
```
