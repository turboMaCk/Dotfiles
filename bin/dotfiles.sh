#!/bin/bash

# save locations
cwd="$(dirname "$0")"
pwd="$(pwd)"

# CD to dotfiles
cd $cwd
cd ..

# Files to backup and symlink
dotfiles=('agrc' 'gemrc' 'gitconfig' 'gitignore_global' 'tmux.conf' 'vimrc' 'vimrc.bundles' 'nvimrc' 'zpreztorc' 'zshrc' 'irssirc')

# Backup directory
backup_dir="$(pwd)/backup"

# Dotfiles directory
dotfiles_dir="$(pwd)/developer"

# backup first
source $cwd/backup.sh

# symlink new
source $cwd/symlink.sh

# symlink neovim
ln -fs ~/.vim ~/.nvim

# symlink irc config
ln -fs ~/.irssirc ~/.irssi/config

# CD back
cd $pwd

