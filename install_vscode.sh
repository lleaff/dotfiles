#!/bin/bash

csd="$(realpath $(dirname $0))"

ln -s "${csd}/vscode" "$HOME/.config/Code/User"
