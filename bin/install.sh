#!/bin/install.sh

# Command Line Tools for XCode
xcode-select --install

zsh

# install prezto
git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"

setopt EXTENDED_GLOB
for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
  ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done

chsh -s /bin/zsh

# Home Brew
ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go/install)"
brew doctor

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

