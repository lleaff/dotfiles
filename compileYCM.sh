#!/bin/bash
if [[ -z $1 ]]; then
	if [[ -z $(hash apt-get 2>&1) ]]; then
		INSTALLCMD='apt-get install -y'
	elif [[ -z $(hash pacman 2>&1) ]]; then
		INSTALLCMD='pacman -Sy'
	elif [[ -z $(hash yum 2>&1) ]]; then
		INSTALLCMD='yum install'
	fi
else
	INSTALLCMD=$1;
fi

# "bundle" for Vundle, "plugged" for plug-vim, ...
if [[ -z $2 ]]; then
	PLUGINMANAGERDIR='plugged'
else
	PLUGINMANAGERDIR=$2
fi

# Dependencies
sudo $INSTALLCMD clang python2.7-dev mono-complete

# Update/Install Vim plugins
vim +PlugInstall +PlugUpdate +q

cd ~/.vim/$PLUGINMANAGERDIR/YouCompleteMe
./install.sh --clang-completer --omnisharp-completer
