#! /usr/bin/env bash

# Dotfiles directory
DOTFILES_DIR="$(pwd)"
BACKUP_DIR="$(pwd)/backup"

# Files to backup and symlink
DOTFILES=('agrc'
          'gitconfig'
          'gitignore_global'
          'tmux.conf'
          'tmux.mac'
          'vimrc'
          'zpreztorc'
          'zshrc'
          'bashrc'
          'aliases'
          'ghci'
          'Xresources'
          'xmonad')

# backup first
printf "Backup old files using $BACKUP_DIR directory\n"

if [ -d "$BACKUP_DIR" ]; then
  printf "Removing old backup...\n"

  rm -rf $BACKUP_DIR
else
  printf "Creating backup directory...\n"

  mkdir -p $BACKUP_DIR
fi

printf "Backup current dotfiles...\n"

# loop all files
for i in "${DOTFILES[@]}"; do
  # check if file exist
  file="$HOME/$i"
  if [ -f $file ]; then
    mv $HOME/.$i $BACKUP_DIR/$i
  fi
done

# symlink new
printf "Creating symlinks to $pwd directory\n"

# loop all
for i in "${DOTFILES[@]}"; do
  ln -fs $DOTFILES_DIR/$i $HOME/.$i
done

# create vim directories
mkdir -p $HOME/.vim/{backup,tmp,undo,view}

# starship
ln -fs $DOTFILES_DIR/starship.toml $HOME/.config

# polybar
mkdir -p $HOME/.config/polybar
ln -fs $DOTFILES_DIR/polybar $HOME/.config/polybar/config

# dunst
mkdir -p $HOME/.config/dunst
ln -fs $DOTFILES_DIR/dunstrc $HOME/.config/dunst/dunstrc

# weechat
mkdir -p $HOME/.weechat
ln -fs $DOTFILES_DIR/irc.conf $HOME/.weechat/irc.conf

# kitty
mkdir -p $HOME/.config/kitty
ln -fs $DOTFILES_DIR/kitty.conf $HOME/.config/kitty/kitty.conf

# check if file exist
if [ -f "$HOME/.emacs.d/init.el" ]; then
  mv $HOME/.emacs.d/init.el $BACKUP_DIR/init.el
fi

# Symlink emacs.d stuff
mkdir -p $HOME/.emacs.d/straight
ln -fs $DOTFILES_DIR/emacs.d/plugin $HOME/.emacs.d/plugin
ln -fs $DOTFILES_DIR/emacs.d/init.el $HOME/.emacs.d/init.el
ln -fs $DOTFILES_DIR/emacs.d/straight/versions $HOME/.emacs.d/straight/versions

# CD back
cd $pwd
