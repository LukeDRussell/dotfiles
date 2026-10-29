#!/usr/bin/env bash
# Make sure Ansible is installed, so the playbook (main.yml) can run.
set -euo pipefail

if command -v ansible-playbook >/dev/null 2>&1; then
	echo "Ansible is ready."
	exit 0
fi

echo "Installing Ansible..."

if [[ $(id -u) -eq 0 ]]; then
	sudo=()
else
	sudo=(sudo)
fi

if command -v dnf >/dev/null 2>&1; then
	"${sudo[@]}" dnf install -y ansible
elif [[ $(uname -s) == Darwin ]]; then
	if ! command -v brew >/dev/null 2>&1; then
		echo "Installing Homebrew..."
		/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
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

echo "Ansible is ready."
