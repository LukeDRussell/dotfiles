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



