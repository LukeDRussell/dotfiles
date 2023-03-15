#!/usr/bin/env bash

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
fi

# Makes sure Ansible is ready to go
if ! command -v ansible &> /dev/null ; then
	echo "Installing Ansible..."
	$SUDO $CMD_PKG_INSTALL install ansible -y
fi

