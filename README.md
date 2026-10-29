# dotfiles
My settings for emacs, bash, pwsh, tmux, etc.

- **Dotfiles** are managed by [chezmoi](https://chezmoi.io). Everything under
  `home/` maps onto `~` (`dot_config/tmux/tmux.conf` → `~/.config/tmux/tmux.conf`).
  Plain files are deployed as symlinks back into this repo, so editing either copy
  is live. `*.tmpl` files are rendered per OS.
  `home/.chezmoiignore` decides which files each OS gets.
- **Packages and OS settings** on Linux and macOS are managed by Ansible
  (`main.yml`). The playbook runs chezmoi too, so one command sets up everything.

## Installation

### Linux / macOS
1. Clone to `~/Repos/dotfiles`.
2. Run `./bootstrap.sh`. It installs Ansible if needed (dnf or Homebrew), then
   runs the playbook. It asks for your sudo password once, if sudo needs one, and
   uses it for both. Arguments are passed to `ansible-playbook`, e.g.
   `./bootstrap.sh --check`.

Later runs can use `./bootstrap.sh` again, or `ansible-playbook -K main.yml`.

### Windows
1. Turn on Developer Mode (Settings → System → For developers) so symlinks work.
2. Install git (`winget install --id Git.Git -e`), open a new terminal, and clone
   to `~\Repos\dotfiles`.
3. Run `powershell -ExecutionPolicy Bypass -File .\install.ps1`. It installs the
   apps in `Windows\winget-packages.json` (PowerShell 7 among them), then chezmoi,
   then applies the dotfiles. `-SkipApps` skips the apps.

To refresh the app list after installing something new, run
`winget export -o new.json --source winget` and copy the new entries across.
Runtimes and libraries (Visual C++, .NET, Windows App Runtime, VCLibs, UI.Xaml)
and apps that ship with Windows are left out on purpose.

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

## SSH

`~/.ssh/config` has shared defaults only; this repo is public. Each machine's
hosts go in `~/.ssh/config.d/hosts`, which isn't tracked:

```
Host example
    HostName 192.0.2.10
```

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

## ble.sh (trial)

[ble.sh](https://github.com/akinomyoga/ble.sh) gives bash fish-like
autosuggestions, syntax highlighting and a fuller vim mode. `.bashrc` loads it
only if `~/.local/share/blesh/ble.sh` exists; settings are in `.blerc`. Start a
shell with `BLE_DISABLE=1` to skip it. Not installed by Ansible yet; on Fedora 44
it was installed by hand from the nightly build (`0.4.0-nightly+aeabb1a`):

```sh
sudo dnf install xz
curl -fsSL https://github.com/akinomyoga/ble.sh/releases/download/nightly/ble-nightly.tar.xz | tar -xJ -C /tmp
bash /tmp/ble-nightly/ble.sh --install ~/.local/share
```

Run the same commands again to update it.
