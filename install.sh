#!/usr/bin/env bash

# This script ensures we have ansible installed locally


if ! command -v ansible &> /dev/null ; then
	echo "Ansible needs to be installed..."


	if [[ "$OSTYPE" == "linux-gnu"* ]]; then
		DISTRO="sed -n -e 's/^ID_LIKE=//p' /etc/os-release"
		if [[ "$DISTRO" == "fedora"||"centos" ]]; then
			CMD_PKG_INSTALL="yum"
		elif [[ "$DISTRO" == "debian"||"ubuntu" ]]; then
			CMD_PKG_INSTALL="apt"
		else
			echo "Check '\$DISTRO' has a match condition: $DISTRO"
		fi
		if [[ "id -u" != 0 ]]; then
			SUDO="sudo"
		else
			SUDO=""
		fi

	elif [[ "$OSTYPE" == "darwin"* ]]; then
		CMD_PKG_INSTALL="brew"
		if [[ ! "command -v brew" ]]; then
			/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
		fi
		SUDO = ""

	fi

	$SUDO $CMD_PKG_INSTALL install ansible -y

fi

