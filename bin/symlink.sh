#!/bin/symlink.sh

printf "Creating symlinks to $pwd directory\n"

# loop all
for i in "${dotfiles[@]}"; do
  ln -fs $dotfiles_dir/$i ~/.$i
done

