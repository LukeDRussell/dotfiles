# dotfiles
My settings for emacs, pwsh, tmux, fish, etc.

- **Dotfiles** are managed by [chezmoi](https://chezmoi.io). Everything under
  `home/` maps onto `~` (`dot_config/fish/config.fish` → `~/.config/fish/config.fish`).
  Plain files are deployed as symlinks back into this repo, so editing either copy
  is live. `*.tmpl` files are rendered per OS.
  `home/.chezmoiignore` decides which files each OS gets.
- **Packages and OS settings** on Linux and macOS are managed by Ansible
  (`main.yml`). The playbook runs chezmoi too, so one command sets up everything.

## Installation

### Linux / macOS
1. Clone to `~/Repos/dotfiles`.
2. Run `install-ansible.sh` to make sure ansible is available.
3. On Linux, enable passwordless sudo:
  1. `sudo visudo`
  2. Comment the `%wheel       ALL=(ALL)     ALL` line.
  3. Uncomment the `%wheel      ALL=(ALL)      NOPASSWD: ALL` line.
  4. Reboot `systemctl reboot` or from Windows, `wsl --shutdown`
4. Run the playbook `ansible-playbook -K main.yml`

### Windows
1. Turn on Developer Mode (Settings → System → For developers) so symlinks work.
2. Clone to `~\Repos\dotfiles` and run `.\install.ps1` from PowerShell 7.

### Fonts
Install fonts by hand; neither Ansible nor chezmoi does it. The configs expect:

- **Hack Nerd Font Mono** (Windows Terminal, Emacs, prompt icons) from
  [nerdfonts.com](https://www.nerdfonts.com/font-downloads)
- **Atkinson Hyperlegible Next** (Emacs variable-pitch text)

For WSL, install them on Windows: the terminal draws the text, not Linux.

## Day to day

```sh
chezmoi status            # what differs between the repo and ~
chezmoi diff              # show the differences
chezmoi apply             # deploy (needed for new files and template changes)
chezmoi add ~/.config/foo # start tracking a new config
ansible-lint              # check the playbook (config in .ansible-lint)
```

Windows Terminal settings are in `Windows/TerminalLocalState` and are linked as a
whole directory. `Windows/*.reg` and `keyboard_manager.json` are applied by hand.

## Emacs

`home/dot_config/emacs/init.el` and `early-init.el` are vendored from
[minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) v1.5.2
(commit `7a15d5b`, GPL-3.0-or-later, licence in `LICENSE-minimal-emacs.d`), so
they only change when I change them. My own settings are in `pre-*.el` and
`post-*.el`.

To see what upstream has changed since, clone it somewhere and diff:

```sh
git clone https://github.com/jamescherti/minimal-emacs.d /tmp/minimal-emacs.d
git -C /tmp/minimal-emacs.d diff 7a15d5b -- init.el early-init.el
```

## Fish

`home/dot_config/fish/functions/_puffer_fish_*.fish` and
`conf.d/puffer_fish_key_bindings.fish` are vendored from
[puffer-fish](https://github.com/nickeb96/puffer-fish) v1.1.0 (commit `83174b0`,
MIT, licence in `LICENSE-puffer-fish`). It expands `...` to `../..`, `!!` to the
last command, and `!$` to its last argument.
