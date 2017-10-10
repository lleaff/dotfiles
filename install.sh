#!/bin/bash
set -o nounset

TARGET="$HOME"

if ! hash git 2>/dev/null; then
    echo "git is not installed.";
    exit 1
fi

cd "$(dirname "$0")"

# SPACEMACS_TARGET="$TARGET"/.emacs.d
# if [[ ! -e "$SPACEMACS_TARGET" ]]; then
#     echo "Installing Spacemacs..."
#     git clone "https://github.com/syl20bnr/spacemacs" "$SPACEMACS_TARGET"
# fi
OHMYZSH_TARGET="$TARGET"/.oh-my-zsh
if [[ ! -e "$OHMYZSH_TARGET" ]]; then
    echo "Installing Oh My Zsh..."
    git clone "https://github.com/robbyrussell/oh-my-zsh" "$OHMYZSH_TARGET"
fi

./makesymlinks.sh "$TARGET"

RELATIVE_OHMYZSHTHEMEDIR=".oh-my-zsh/themes"
REAL_OHMYZSHTHEMEDIR=$(realpath "$RELATIVE_OHMYZSHTHEMEDIR")
ohmyzshthemes=""
for f in $(ls "$REAL_OHMYZSHTHEMEDIR"); do
    if [[ ! -e "$TARGET/$RELATIVE_OHMYZSHTHEMEDIR/$(basename $f)" ]]; then
        ohmyzshthemes="${ohmyzshthemes}$(basename $f), "
        ln -s "$f" "$TARGET/$RELATIVE_OHMYZSHTHEMEDIR"
    fi
done
if [[ -n "$ohmyzshthemes" ]]; then
    echo "Symlinked custom Oh My Zsh theme(s): ${ohmyzshthemes}from dotfiles to ${REAL_OHMYZSHTHEMEDIR}."
fi
