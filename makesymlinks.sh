#!/bin/bash
# This script creates symlinks from the home directory
#  to any desired files in the directory it is in.
############################

########## Variables

dir=$(dirname "$0")                    # dotfiles directory
olddir="$HOME/__old/dotfiles/"         # old dotfiles backup directory
SYMLINK_NAMES_FILE="$dir/to_symlink"
if [[ -n "$1" ]]; then
    TARGET="$1";
else
    TARGET="$HOME";
fi

# list of files/folders to symlink in homedir
files="$(cat "$SYMLINK_NAMES_FILE")"

##########

COLORRESET='\e[39m'
COLORGREEN='\e[32m'
COLORYELLOW='\e[33m'
COLORMAGENTA='\e[35m'

##########

if [[ "$TARGET" == "$HOME" ]]; then
    friendlyTargetName="home directory"
else
    friendlyTargetName="$TARGET"
fi

# create dotfiles_old in homedir
echo -e "Creating $olddir for backup of any existing dotfiles in $TARGET"
mkdir -p $olddir

if hash realpath 2>/dev/null; then
	realdir=$(realpath "$dir")
	realolddir=$(realpath "$olddir")
else
	realdir="$dir"
	realolddir="$olddir"
fi


cd $realdir
realdir=$(pwd)

existing=""

# move any existing dotfiles in homedir to dotfiles_old
#  directory, then create symlinks
for file in $files; do
    if $(mv "$TARGET/$file" "$realolddir" 2>/dev/null); then
        existing="${existing}${file}, "
    fi
    dirnm=$(dirname "$file")
    if [[ $dirnm != '.' ]]; then
        echo -e "${COLORMAGENTA}Creating directory ${COLORRESET}$dirnm ${COLORMAGENTA}for ${COLORRESET}$file${COLORRESET}"
        mkdir -p $dirnm
    fi
    echo -e "${COLORYELLOW}Creating symlink to $file in $friendlyTargetName${COLORRESET}"
    ln -s "$realdir/$file" "$TARGET/$file"
done

echo "____________________________________________________________"

if [[ -n "$existing" ]]; then
    echo -e "${COLORYELLOW}Moved existing file(s) ${COLORRESET}$existing${COLORYELLOW}to ${COLORRESET}$olddir${COLORRESET}"
fi
echo -e "${COLORGREEN}All done.${COLORRESET}"

# create vim/tmp folder
mkdir -p ~/.vim/tmp

mkdir -p "$HOME"/.config/nvim/
ln -s "$realdir/init.vim" "$HOME"/.config/nvim/init.vim

mkdir -p "$HOME"/.config/oni/
ln -s "$realdir"/oni/config.tsx "$HOME"/.config/oni/config.tsx
ln -s "$realdir"/oni/tsconfig.json "$HOME"/.config/oni/init.vim
ln -s "$realdir"/oni/plugins/theme-pond "$HOME"/.config/oni/plugins/theme-pond
ln -s "$realdir"/oni/plugins/lleaff-default-config "$HOME"/.config/oni/plugins/lleaff-default-config

mkdir -p "$HOME"/.config/Code/User/
ln -s "$realdir"/.config/Code/User/settings.json "$HOME"/.config/Code/User/settings.json
