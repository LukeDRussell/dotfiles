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

Windows Terminal's SSH profiles (the "SSH" folder in the new-tab menu) run
`ssh <name>`, so each name must resolve on the machine: through DNS, or a
`config.d/hosts` entry pointing at the full name (e.g. a Tailscale `.ts.net`
name, which works away from home).

### Key login to a Windows OpenSSH server

Windows OpenSSH Server reads keys for accounts in the Administrators group from
`C:\ProgramData\ssh\administrators_authorized_keys`, not `~\.ssh\authorized_keys`,
and ignores that file unless only Administrators and SYSTEM can access it. Run
this in an elevated PowerShell on the server (adds the key in both places, so it
works for admin and non-admin accounts; safe to re-run):

```powershell
$key = '<contents of ~/.ssh/pers_key.pub from the client>'

$admin = 'C:\ProgramData\ssh\administrators_authorized_keys'
if (-not ((Test-Path $admin) -and (Select-String -Path $admin -SimpleMatch $key -Quiet))) { Add-Content -Path $admin -Value $key }
icacls.exe $admin /inheritance:r /grant '*S-1-5-32-544:F' /grant '*S-1-5-18:F'

New-Item -ItemType Directory -Force "$HOME\.ssh" | Out-Null
$user = "$HOME\.ssh\authorized_keys"
if (-not ((Test-Path $user) -and (Select-String -Path $user -SimpleMatch $key -Quiet))) { Add-Content -Path $user -Value $key }

Restart-Service sshd
```

- `*S-1-5-32-544` and `*S-1-5-18` are Administrators and SYSTEM, in any Windows
  language. Use `Add-Content` (no byte-order mark); sshd can't read a key file
  with one.
- Check from the client with `ssh -o PasswordAuthentication=no <host> hostname`.
  If it fails, `ssh -v` shows whether the key was offered, and the server logs
  the reason under Event Viewer → Applications and Services Logs → OpenSSH →
  Operational.
- Reinstalling OpenSSH Server creates new host keys, so clients then warn that
  the host identification has changed. Check the new fingerprint on the server
  (`ssh-keygen -l -f C:\ProgramData\ssh\ssh_host_ed25519_key.pub`), then remove
  the old entries on each client with `ssh-keygen -R <name>` for every name you
  use for that host. The reinstall also resets `sshd_config` and leaves no key
  files, so repeat the steps above.

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
