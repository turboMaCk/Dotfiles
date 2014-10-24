# Install command-line tools using Homebrew
# Usage: `brew bundle Brewfile`

# Make sure we’re using the latest Homebrew
update

# Upgrade any already-installed formulae
upgrade

# Install GNU core utilities (those that come with OS X are outdated)
# Don’t forget to add `$(brew --prefix coreutils)/libexec/gnubin` to `$PATH`.
install coreutils

# Install wget with IRI support
install wget --enable-iri

# Install more recent versions of some OS X tools
install vim --override-system-vi
install homebrew/dupes/grep

# Install other useful binaries
install ack
install git
install git-flow
install tig
install ctags
install imagemagick --with-webp
install node # This installs `npm` too using the recommended installation method
install macvim
install mysql
install openssl
install openssh --with-brewed-openssl --with-keychain-support
install tmux
install osxfuse
install htop-osx
install memcached

# RBENV
install rbenv ruby-build

# PYENV + virtualenv
brew install pyenv-virtualenv

# NVM
brew install nvm

# Databses
install mysql
install postgresql
install sqlite
install redis
install mongo

# PHP
# brew tap homebrew/homebrew-php
# brew tap homebrew/dupes
# brew tap homebrew/versions
# brew install php55-intl
# brew install homebrew/php/composer
