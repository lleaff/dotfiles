#!/bin/bash

# Stop on errors
set -e

NPM_PACKAGES="eslint eslint_d babel-eslint eslint-plugin-react"

if hash yarn 2>/dev/null; then
  echo "Installing with yarn..."
  yarn global add $NPM_PACKAGES
else
  echo "Installing with npm..."
  npm install -g $NPM_PACKAGES
fi

echo "Eslint installed."

if [ ! -f "$HOME/.eslintrc.js" ]; then
  echo "No ~/.eslintrc.js found, create one to configure eslint globally." 1>&2;
fi
