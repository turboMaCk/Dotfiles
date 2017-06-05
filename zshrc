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
export EDITOR='vim'

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/"

# ALIASES
alias g="git"
alias tm="tmux"
alias s="subl"
alias subl="/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl"

# vim
alias v="vim"

# emacs
alias e="emacs"

# folder aliases
alias developer="cd ~/Developer/"
alias sites="cd ~/Sites/"
alias documents="cd ~/Documents/"
alias downloads="cd ~/Downloads/"
alias dotfiles="cd ~/Dotfiles/"
alias desktop="cd ~/Desktop/"

# tmux aliases
alias tml="tmux list-sessions"
alias tma="tmux attach-session -t"
alias tmc="clear && tmux clear-history"
alias tmk="tmux kill-session"
alias tmn="tmux new -s"

# Rails aliases
alias rails="bundle exec rails"
alias rake="bundle exec rake"
alias rspec="bundle exec rspec"

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

# When using sudo, use alias expansion (otherwise sudo ignores your aliases)
alias sudo="sudo "

# Docker
function _dmenv() {
  eval $(docker-machine env $1)
};
alias dm="docker-machine"
alias dco="docker-compose"
alias dme="_dmenv"
alias dcu="docker-compose up"
alias dcs="docker-compose stop"

# Fix no new lines endings
function sanitizeFileEnds() {
  find -type f -exec sh -c "tail -1 {} | xxd -p | tail -1 | grep -v 0a$" ';' -exec sh -c "echo >> {}" ';'
}
alias line-ends="sanitizeFileEnds"

# Toys
function _wttr() {
  url="wttr.in/$1"
  curl $url
}
alias wttr="_wttr"

# setup path
export PATH="/usr/local/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
export PATH="~/.cabal/bin:$PATH"

export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"

# set language
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# RBENV
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# NVM
export NVM_DIR=~/.nvm
if which brew > /dev/null; then source "$(brew --prefix nvm)/nvm.sh"; fi
