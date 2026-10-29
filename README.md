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

## Day to day

```sh
chezmoi status            # what differs between the repo and ~
chezmoi diff              # show the differences
chezmoi apply             # deploy (needed for new files and template changes)
chezmoi add ~/.config/foo # start tracking a new config
```

Windows Terminal settings are in `Windows/TerminalLocalState` and are linked as a
whole directory. `Windows/*.reg` and `keyboard_manager.json` are applied by hand.
