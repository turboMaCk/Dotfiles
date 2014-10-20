#!/bin/symlinks.sh

cwd="$(dirname "$0")"

dotfiles=('ackrc' 'gemrc' 'gitconfig' 'gitignore_global' 'tmux.conf' 'vimrc' 'vimrc.bundles' 'zshrc' 'zpreztorc')

# backup first
source $cwd/backup.sh

# symlink new
source $cwd/symlinks.sh
