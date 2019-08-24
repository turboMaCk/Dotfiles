#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#   Marek Fajkus <marek.faj@gmail.com>
#

# prezto depends on coreutils
# This is fix for error in prompt on MacOS
export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Vim as editor
export EDITOR="vim"

# Emacs as visual editor
export VISUAL="emacsclient -n -c"

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

# NVM
if which brew > /dev/null; then
  export NVM_DIR=~/.nvm
  source "$(brew --prefix nvm)/nvm.sh";
fi

# OPAM (OCaml) configuration
# . /home/masdrek/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# source aliases
if [ -f ~/.aliases ]; then
    . ~/.aliases
fi

# GO FUCK YOURSELF
export GOPATH=$HOME/go
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin

export PATH=$PATH:$HOME/mutable_node_modules/bin

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/marek/google-cloud-sdk/path.zsh.inc' ]; then source '/Users/marek/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/marek/google-cloud-sdk/completion.zsh.inc' ]; then source '/Users/marek/google-cloud-sdk/completion.zsh.inc'; fi
