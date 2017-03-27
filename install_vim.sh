#!/bin/bash
set -o nounset  # Treat unset variables as an error


#echo "Installing Dein...
#________________________"
## Unix
#DEIN_INSTALLATION_DIRECTORY=$HOME'/dotfiles/.vim/bundle/dein.vim'
#
#DEIN_INSTALLER=$HOME'/dein_install.sh~'
#curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh > $DEIN_INSTALLER
#sh $DEIN_INSTALLER $DEIN_INSTALLATION_DIRECTORY

echo "Installing vim-plug...
______________________"

set -e # Stop on error
# Unix
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
vim +'PlugInstall' +':qa'
echo "Done! Use :PlugInstall command to update plugins."
