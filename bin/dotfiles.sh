#!/bin/bash

# save locations
cwd="$(dirname "$0")"
pwd="$(pwd)"

# CD to dotfiles
cd $cwd
cd ..

# Files to backup and symlink
dotfiles=('agrc' 'gemrc' 'gitconfig' 'gitignore_global' 'tmux.conf' 'tmux.mac' 'vimrc' 'vimrc.bundles' 'nvimrc' 'zpreztorc' 'zshrc' 'irssirc' 'ghci')

# Backup directory
backup_dir="$(pwd)/backup"

# Dotfiles directory
dotfiles_dir="$(pwd)"

# backup first
printf "Backup old files using $backup_dir directory\n"

if [ -d "$backup_dir" ]; then
  printf "Removing old backup...\n"

  rm -rf $backup_dir/*
else
  printf "Creating backup directory...\n"

  mkdir $backup_dir
fi

printf "Backup current dotfiles...\n"

# loop all files
for i in "${dotfiles[@]}"; do

  # check if file exist
  if [ -f "~/.$i" ]; then
    mv ~/.$i $backup_dir/$i
  fi
done

# symlink new
printf "Creating symlinks to $pwd directory\n"

# loop all
for i in "${dotfiles[@]}"; do
  ln -fs $dotfiles_dir/$i ~/.$i
done

# symlink neovim
ln -fs ~/.vim ~/.nvim

# symlink irc config
#if ls ~/.irssi/config > /dev/null; then ln -fs ~/.irssirc ~/.irssi/config/.irssirc; fi

# CD back
cd $pwd
