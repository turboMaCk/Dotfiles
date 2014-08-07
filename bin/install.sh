#!/bin/install.sh

# Command Line Tools for XCode
xcode-select --install

# install OH MY ZSH
curl -L http://install.ohmyz.sh | sh

# Home Brew
ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go/install)"
brew doctor

# RVM
\curl -sSL https://get.rvm.io | bash -s stable --ruby

# NVM
curl https://raw.githubusercontent.com/creationix/nvm/v0.11.1/install.sh | bash

# source profile
source ~/.profile
