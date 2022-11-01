#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#   Marek Fajkus <marek.faj@gmail.com>
#

# prezto depends on coreutils
# This is fix for error in prompt on MacOS
# mac specific hack for GNU coreutils
export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Vim as editor
export EDITOR="emacsclient -t"

# Emacs as visual editor
export VISUAL="emacsclient -t"

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

# NVM on mac
if which brew > /dev/null; then
  export NVM_DIR=~/.nvm
  source "$(brew --prefix nvm)/nvm.sh";
fi

# OPAM (OCaml) configuration
# . /home/marek/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# source aliases
if [ -f ~/.aliases ]; then
    . ~/.aliases
fi


# GO FUCK YOURSELF
export GOPATH=$HOME/go
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin

export PATH=$PATH:$(npm config get prefix)/bin

# Add ~/.local/bin to PATH
export PATH=$PATH:$HOME/.local/bin

# Path to mutable node modules
# npm config set prefix $HOME/.npm
export PATH=$PATH:$HOME/.npm/bin

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/marek/google-cloud-sdk/path.zsh.inc' ]; then
  source '/Users/marek/google-cloud-sdk/path.zsh.inc';
fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/marek/google-cloud-sdk/completion.zsh.inc' ]; then
  source '/Users/marek/google-cloud-sdk/completion.zsh.inc';
fi

# Silence direnv
# see https://github.com/direnv/direnv/issues/68
_direnv_hook() {
  eval "$(direnv export zsh 2> >( egrep -v -e '^direnv: (loading|export|unloading)' ))"
};

eval "$(direnv hook zsh)"
