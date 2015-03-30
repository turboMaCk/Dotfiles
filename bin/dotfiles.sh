#!/bin/dotfiles.sh

cwd="$(dirname "$0")"

dotfiles=('ackrc' 'gemrc' 'gitconfig' 'gitignore_global' 'tmux.conf' 'vimrc' 'nvimrc' 'vimrc.bundles' 'zshrc' 'zpreztorc')

# backup first
backup_dir="$cwd/../backup"

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
dotfiles_dir="$cwd/.."

printf "Creating symlinks to $pwd directory\n"

for i in "${dotfiles[@]}"; do
  ln -fs $dotfiles_dir/$i ~/.$i
done

# symlink neovim
ln -fs $dotfiles_dir/vimrc ~/.nvimrc
ln -fs ~/.vim ~/.nvim
