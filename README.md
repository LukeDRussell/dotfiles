# dotfiles
My settings for fish, tmux, neovim, etc.

## Installation
1. Run `install-ansible.sh` to make sure ansible is available.
2. On Linux, enable passwordless sudo:
  1. `sudo visudo`
  2. Comment the `%wheel       ALL=(ALL)     ALL` line.
  3. Uncomment the `%wheel      ALL=(ALL)      NOPASSWD: ALL` line.
  4. Reboot `systemctl reboot` or from Windows, `wsl --shutdown`
3. Run the playbook `ansible-playbook -K main.yml`

## Windows Symlinks

```pwsh
New-Item -ItemType Junction `
    -Path "~\AppData\Local\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\" `
    -Target "C:\Users\Luke\Repos\dotfiles\wt_LocalState\"
    -Force
New-Item -ItemType SymbolicLink `
    -Path $PROFILE `
    -Target "$HOME\Repos\dotfiles\Microsoft.PowerShell_profile.ps1"
    -Force
New-Item -ItemType SymbolicLink `
    -Path "~\OneDrive\Documents\PowerShell\Microsoft.PowerShell_profile.ps1" `
    -Target "~\Repos\dotfiles\Microsoft.PowerShell_profile.ps1"
New-Item -ItemType SymbolicLink `
    -Path ~\.emacs.d\pre-early-init.el `
    -Target ~\Repos\dotfiles\pre-early-init.el
New-Item -ItemType SymbolicLink `
    -Path ~\.emacs.d\ppost-early-init.el `
    -Target ~\Repos\dotfiles\post-early-init.el
New-Item -ItemType SymbolicLink `
    -Path ~\.emacs.d\ppre-init.el `
    -Target ~\Repos\dotfiles\pre-init.el
New-Item -ItemType SymbolicLink `
    -Path ~\.emacs.d\ppost-init.el `
    -Target ~\Repos\dotfiles\post-init.el
```
