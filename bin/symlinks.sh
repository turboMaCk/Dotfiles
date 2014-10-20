#!/bin/symlinks.sh

dotfils_dir=$(pwd)

printf "Creating symlinks to $pwd directory\n"

for i in "${dotfiles[@]}"; do

  ln -fs $dotfils_dir/$i ~/.$i
done

