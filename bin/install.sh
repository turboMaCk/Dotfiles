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

# Vundle
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

# create new folders
mkdir ~/.vim/tmp
mkdir ~/.vim/undo
mkdir ~/.vim/backup

# source profile
source ~/.profile

# Vagrant + parallels
vagrant plugin install vagrant-parallels

# ssh-copy-id
sudo curl “https://raw.github.com/garmoncheg/ssh-copy-id-for-OSX/master/ssh-copy-id.sh” -o /usr/bin/ssh-copy-id
chmod +x /usr/bin/ssh-copy-id

