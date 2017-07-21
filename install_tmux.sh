#!/bin/bash
set -o nounset
# Stop on first error
set -e

mkdir -p ~/dotfiles/.tmux/plugins/tpm
if [[ ! -d ~/.tmux ]]; then ln -s ~/dotfiles/.tmux ~/.tmux; fi
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
