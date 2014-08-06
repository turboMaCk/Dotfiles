#!/bin/symlinks.sh

dotfils_dir=$(pwd)

printf "Creating symlinks to $pwd directory\n"

for i in "${dotfiles[@]}"; do

  ln -fs $dotfils_dir/$i ~/.$i
done

# sublime symlinks
ln -s $cwd/Preferences.sublime-settings ~/Library/Application\ Support/Sublime\ Text\ 3/Packages/User/Preferences.sublime-settings
ln -s $cwd/Package\ Control.sublime-settings Library/Application\ Support/Sublime\ Text\ 3/Packages/User/Package\ Control.sublime-settings

