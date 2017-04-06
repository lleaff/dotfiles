#!/bin/bash
set -o nounset  # Treat unset variables as an error

STARTING_DIR="$PWD"

#------------------------------------------------------------

echo "Installing vim-plug...
______________________"

VIM_DIR="$HOME/.vim"
PLUGINS_DIR="${VIM_DIR}/plugged"

set -e # Stop on error
# Unix
curl -fLo "${VIM_DIR}/autoload/plug.vim" --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
vim +'PlugInstall' +':qa'
echo "Done installing vim-plug. Use :PlugInstall command to update plugins."

#------------------------------------------------------------

echo "Building vimproc...
___________________"
cd "${PLUGINS_DIR}/vimproc.vim/"
make

#------------------------------------------------------------

echo "Installing tern_for_vim dependencies...
_______________________________________"
cd "${PLUGINS_DIR}/tern_for_vim/"
if hash yarn 1>/dev/null 2>&1; then
  yarn install
elif hash npm 1>/dev/null 2>&1; then
  npm install
else
  echo "Install npm and execute 'npm install' in '${PLUGINS_DIR}/tern_for_vim/' to install required packages for tern_for_vim."
fi

#------------------------------------------------------------

if ! hash pt 1>/dev/null 2>&1; then
  echo "_______________________________________"
  echo "Platinum Searcher executable (pt) not detected, install at:"
  echo "https://github.com/monochromegane/the_platinum_searcher/releases/latest"
  echo "for faster file search in Vim."
fi


echo "Done!"

cd "$STARTING_DIR"
