#!/bin/bash


# save locations
cwd="$(dirname "$0")"
pwd="$(pwd)"

# CD to dotfiles
cd $cwd
cd ..

# Files to backup and symlink
dotfiles=('bashrc' 'vimrc' 'gemrc')

# Backup directory
backup_dir="$(pwd)/backup"

# Dotfiles directory
dotfiles_dir="$(pwd)/server"

# backup first
source $cwd/backup.sh

# symlink new
source $cwd/symlink.sh

# CD back
cd $pwd
