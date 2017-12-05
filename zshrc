#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#   Marek Fajkus <marek.faj@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...

# When using sudo, use alias expansion (otherwise sudo ignores your aliases)

# Docker Machine ENV
function _dmenv() {
  eval $(docker-machine env $1)
};

# Fix no new lines endings
function sanitizeFileEnds() {
  find -type f -exec sh -c "tail -1 {} | xxd -p | tail -1 | grep -v 0a$" ';' -exec sh -c "echo >> {}" ';'
}

# Toys
function _wttr() {
  url="wttr.in/$1"
  curl $url
}

# RBENV
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# NVM
export NVM_DIR=~/.nvm
if which brew > /dev/null; then
  source "$(brew --prefix nvm)/nvm.sh";
else
  source /usr/share/nvm/nvm.sh
fi

# OPAM (OCaml) configuration
. /Users/marek/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# source aliases
if [ -f ~/.aliases ]; then
    . ~/.aliases
fi

screenfetch
