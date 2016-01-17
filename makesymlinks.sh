#!/bin/bash
# This script creates symlinks from the home directory
#  to any desired dotfiles in ~/dotfiles
############################

########## Variables

dir=~/dotfiles                    # dotfiles directory
olddir=~/__old/dotfiles/             # old dotfiles backup directory
# list of files/folders to symlink in homedir
files=".bashrc .bash_profile .vimrc .vim .zshrc .oh-my-zsh .tmux.conf .tmux .gitconfig .locations .termaliases .spacemacs .emacs.d .ghci"

##########

# create dotfiles_old in homedir
echo "Creating $olddir for backup of any existing dotfiles in ~"
mkdir -p $olddir
echo "...done"

# change to the dotfiles directory
echo "Changing to the $dir directory"
cd $dir
echo "...done"

# move any existing dotfiles in homedir to dotfiles_old
#  directory, then create symlinks
for file in $files; do
    echo "Moving any existing dotfiles from ~ to $olddir"
    mv ~/$file $olddir
    echo "Creating symlink to $file in home directory."
    ln -s $dir/$file ~/$file
done

# create vim/tmp folder
mkdir -p ~/.vim/tmp
