#!/bin/bash
set -o nounset                              # Treat unset variables as an error


echo "Installing Dein...
________________________"
# Unix
DEIN_INSTALLATION_DIRECTORY=$HOME'/dotfiles/.vim/bundle/dein.vim'

DEIN_INSTALLER=$HOME'/dein_install.sh~'
curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh > $DEIN_INSTALLER
sh $DEIN_INSTALLER $DEIN_INSTALLATION_DIRECTORY

