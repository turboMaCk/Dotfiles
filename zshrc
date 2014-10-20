#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='mvim'
fi

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/"

# ALIASES
alias g="git"
alias v="vim"
alias s="subl"
alias love="/Applications/love.app/Contents/MacOS/love"
alias subl="/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl"

# folder aliases
alias developer="cd ~/Developer/"
alias sites="cd ~/Sites/"
alias documents="cd ~/Documents/"
alias downloads="cd ~/Downloads/"
alias dotfiles="cd ~/Dotfiles/"
alias desktop="cd ~/Desktop/"

# Shortcuts
alias fs="stat -f \"%z bytes\""

# Empty all thrashes
alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; rm -rfv ~/.Trash"

# Hide/show all desktop icons (useful when presenting)
alias hidedesktop="defaults write com.apple.finder CreateDesktop -bool false && killall Finder"
alias showdesktop="defaults write com.apple.finder CreateDesktop -bool true && killall Finder"

# Show/Hide hidden files
alias showdots="defaults write com.apple.finder AppleShowAllFiles TRUE & killall Finder"
alias hidedots="defaults write com.apple.finder AppleShowAllFiles FALSE & killall Finder"

# Http server
alias server="open http://localhost:8000 && http-server -p 8000"

# setup path
export PATH="/usr/local/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="$PATH:$(brew --prefix coreutils)/libexec/gnubin"

# set language
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# RBENV
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# NVM
source ~/.nvm/nvm.sh
