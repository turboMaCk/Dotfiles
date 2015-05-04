#!/bin/bash

printf "Install script started... \n"

# Home Brew
printf "Installing Home Brew \n"
ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go/install)"
brew doctor

# Vundle
printf "Installing Vundle \n"
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim


# create new folders vor vim
printf "Setup vim directories"
mkdir ~/.vim/tmp
mkdir ~/.vim/undo
mkdir ~/.vim/backup

# source profile
source ~/.profile

# ssh-copy-id
printf "Installing ssh-copy-id script..."
sudo curl “https://raw.github.com/garmoncheg/ssh-copy-id-for-OSX/master/ssh-copy-id.sh” -o /usr/bin/ssh-copy-id
chmod +x /usr/bin/ssh-copy-id

