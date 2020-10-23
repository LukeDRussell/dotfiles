Sourced from https://gist.github.com/entropiae/326611addf6662d1d8fbf5792ab9a770

```
Install pyenv on Ubuntu 18.04 + fish shell

- Install the packages required to compile Python
$ sudo apt-get update; sudo apt-get install --no-install-recommends make build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev

- Download pyenv code from github
$ git clone https://github.com/pyenv/pyenv.git ~/.pyenv

- Define environment variable PYENV_ROOT to point to the path where pyenv repo is cloned
$ echo "set --export PYENV_ROOT $HOME/.pyenv" > ~/.config/fish/conf.d/pyenv.fish

- Add $PYENV_ROOT/bin to your $PATH for access to the pyenv command-line utility
$ set -U fish_user_paths $HOME/.pyenv/bin $fish_user_paths

- Add pyenv init to your shell to enable shims and autocompletion.
$ echo -e '\n\n# pyenv init\nif command -v pyenv 1>/dev/null 2>&1\n  pyenv init - | source\nend' >> ~/.config/fish/config.fish

- Install pyenv-virtualenv
$ git clone https://github.com/pyenv/pyenv-virtualenv.git (pyenv root)/plugins/pyenv-virtualenv

- Enable virtualenv autocomplete
$ echo -e "\n# Enable virtualenv autocomplete\nstatus --is-interactive; and pyenv init - | source\nstatus --is-interactive; and pyenv virtualenv-init - | source\n" >> ~/.config/fish/conf.d/pyenv.fish

######################################
- List available Python versions
$ pyenv install --list

- Install a version
$ pyenv install 2.7.16

- Create a virtualenv
$ pyenv virtualenv 3.7.4 my_venv

- Activate a virtualenv
pyenv activate my_venv

```
