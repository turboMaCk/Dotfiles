#!/bin/bash

# save locations
cwd="$(dirname "$0")"
pwd="$(pwd)"

# CD to dotfiles
cd $cwd
cd ..

# Files to backup and symlink
dotfiles=('ackrc' 'gemrc' 'gitconfig' 'gitignore_global' 'tmux.conf' 'vimrc' 'vimrc.bundles' 'zpreztorc' 'zshrc' 'tmux-powerlinerc')

# Backup directory
backup_dir="$(pwd)/backup"

# Dotfiles directory
dotfiles_dir="$(pwd)/developer"

# backup first
source $cwd/backup.sh

# symlink new
source $cwd/symlink.sh

# symlink neovim
ln -fs $dotfiles_dir/vimrc ~/.nvimrc
ln -fs ~/.vim ~/.nvim

# CD back
cd $pwd

