# Store current working (repo) directory to variable
cwd=$(pwd)

# Change directory to home
cd ~/

mkdir .vim/tmp
mkdir .vim/undo
mkdir .vim/backup

# Generate symlinks to files in repo
ln -s $cwd/vimrc.bundles .vimrc.bundles
ln -s $cwd/vimrc .vimrc
ln -s $cwd/ackrc .ackrc
ln -s $cwd/zshrc .zshrc
ln -s $cwd/gitignore_global .gitignore_global
ln -s $cwd/gitconfig .gitconfig
ln -s $cwd/gemrc .gemrc

# sublime symlinks
ln -s $cwd/Preferences.sublime-settings ~/Library/Application\ Support/Sublime\ Text\ 3/Packages/User/Preferences.sublime-settings
ln -s $cwd/Package\ Control.sublime-settings Library/Application\ Support/Sublime\ Text\ 3/Packages/User/Package\ Control.sublime-settings

cd $cwd
