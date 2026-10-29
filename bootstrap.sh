#!/usr/bin/env bash
# Set up a Linux or macOS machine: install Ansible if needed, then run the
# playbook (main.yml). Asks for your sudo password once, if sudo needs one, and
# uses it for both steps. Extra arguments go to ansible-playbook, e.g.
#   ./bootstrap.sh --check
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"

# Read the password from the terminal, not stdin, so this also works when the
# script itself arrives on stdin (curl ... | bash)
pw=""
if ! sudo -n true 2>/dev/null; then
	read -rsp "sudo password for $USER: " pw </dev/tty
	echo
	if ! printf '%s\n' "$pw" | sudo -S -v -p '' 2>/dev/null; then
		echo "That password didn't work for sudo." >&2
		exit 1
	fi
fi

if ! command -v ansible-playbook >/dev/null 2>&1; then
	echo "Installing Ansible..."
	if command -v dnf >/dev/null 2>&1; then
		sudo dnf install -y ansible
	elif [[ $(uname -s) == Darwin ]]; then
		if ! command -v brew >/dev/null 2>&1; then
			echo "Installing Homebrew..."
			NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
			# Homebrew isn't on PATH until the shell is set up; do it for this script
			for brew in /opt/homebrew/bin/brew /usr/local/bin/brew; do
				if [[ -x $brew ]]; then
					eval "$("$brew" shellenv)"
					break
				fi
			done
		fi
		brew install ansible
	else
		echo "Don't know how to install Ansible on this system (only dnf and Homebrew)." >&2
		echo "Install it yourself, then run: ansible-playbook -K main.yml" >&2
		exit 1
	fi
fi

# Hand the password to Ansible through a pipe, so it never touches the disk
if [[ -n $pw ]]; then
	exec ansible-playbook --become-password-file <(printf '%s\n' "$pw") main.yml "$@"
else
	exec ansible-playbook main.yml "$@"
fi
