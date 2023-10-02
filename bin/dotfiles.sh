#! /usr/bin/env bash

# Dotfiles directory
dotfiles_dir="$(pwd)"
backup_dir="$(pwd)/backup"

# Files to backup and symlink
dotfiles=('agrc'
          'gemrc'
          'gitconfig'
          'gitignore_global'
          'tmux.conf'
          'tmux.mac'
          'vimrc'
          'zpreztorc'
          'profile'
          'zshrc'
          'aliases'
          'ghci'
          'Xresources'
          'Xmodmap'
          'xmonad')

# backup first
printf "Backup old files using $backup_dir directory\n"

if [ -d "$backup_dir" ]; then
  printf "Removing old backup...\n"

  rm -rf $backup_dir
else
  printf "Creating backup directory...\n"

  mkdir -p $backup_dir
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
  ln -fs $dotfiles_dir/$i $HOME/.$i
done

# create vim directories
mkdir -p $HOME/.vim/{backup,tmp,undo,view}

# polybar
mkdir -p $HOME/.config/polybar
ln -fs $dotfiles_dir/polybar $HOME/.config/polybar/config

# dunst
mkdir -p $HOME/.config/dunst
ln -fs $dotfiles_dir/dunstrc $HOME/.config/dunst/dunstrc

# weechat
mkdir -p $HOME/.weechat
ln -fs $dotfiles_dir/irc.conf $HOME/.weechat/irc.conf

# kitty
mkdir -p $HOME/.config/kitty
ln -fs $dotfiles_dir/kitty.conf $HOME/.config/kitty/kitty.conf

# symlink neovim
# ln -fs ~/.vim ~/.nvim

# symlink .emacs.d
if [ -d "$HOME/.emacs.d" ]; then
  echo "Backing up old .emacs.d"
  mv -f $HOME/.emacs.d $backup_dir
fi
ln -fs $dotfiles_dir/emacs.d $HOME/.emacs.d
# Setup secrets
cp $HOME/.emacs.d/secrets.example.el $HOME/.emacs.d/secrets.el


# CD back
cd $pwd
