#!/bin/symlinks.sh

dotfiles_dir=$(pwd)

printf "Creating symlinks to $pwd directory\n"

for i in "${dotfiles[@]}"; do
  ln -fs $dotfiles_dir/$i ~/.$i
done

# symlink neovim
ln -fs $dotfiles_dir/vimrc ~/.nvimrc
ln -fs ~/.vim ~/.nvim

# link custom shel theme
ln -fs $dotfils_dir/prompt_marek_setup ~/.zprezto/modules/prompt/functions/prompt_marek_setup
