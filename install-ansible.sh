#!/usr/bin/env bash

# This script uses ansible to install dotfiles, so make sure it's installed.

if command -v ansible &> /dev/null ; then
	echo "Ansible is ready."
	exit 0
fi

echo "Installing Ansible..."

# Set OS & Distro variables
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
	DISTRO="sed -n -e 's/^ID_LIKE=//p' /etc/os-release"
	if [[ "$DISTRO" == "fedora" ]]; then
		CMD_PKG_INSTALL="dnf"
	elif [[ "$DISTRO" == "centos"||"redhat" ]]; then
		CMD_PKG_INSTALL="yum"
	elif [[ "$DISTRO" == "debian"||"ubuntu" ]]; then
		CMD_PKG_INSTALL="apt"
	else
		echo "This script wasn't written for your distro: $DISTRO"
	fi

	if [[ "id -u" != 0 ]]; then
		SUDO="sudo"
	else
		SUDO=""
	fi

elif [[ "$OSTYPE" == "darwin".* ]]; then
	CMD_PKG_INSTALL="brew"
	# Makes sure Homebrew is ready to go
	if [[ ! "command -v brew" ]]; then
		echo "Installing homebrew..."
		/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
	fi
	SUDO = ""
else
	echo "Can't determine package installer. Check your OS is in the list"
	exit 1
fi

$SUDO $CMD_PKG_INSTALL install ansible -y
