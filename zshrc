#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#   Marek Fajkus <marek.faj@gmail.com>
#

# Triger `vcs_info 'prompt'` before loading Prezto.
# We want the first call to this happen before prezto will
# `unsetopt CASE_GLOB` which will have negative effect on the performance.
# Calling before case sensitive globing make initial start much faster.
# see: https://github.com/nix-community/home-manager/issues/2255
autoload -Uz vcs_info
vcs_info 'prompt'

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# prezto depends on coreutils
# This is fix for error in prompt on MacOS
# mac specific hack for GNU coreutils
#export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"


# Vim as editor
export EDITOR="vim"

# Emacs as visual editor
export VISUAL="emacsclient -cn"

# Customize to your needs...

# When using sudo, use alias expansion (otherwise sudo ignores your aliases)

# Docker Machine ENV
function _dmenv() {
  eval $(docker-machine env $1)
};

# OPAM (OCaml) configuration
# . /home/marek/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# Source aliases
if [ -f ~/.aliases ]; then
    . ~/.aliases
fi


# GO PATH madness
export GOPATH=$HOME/go
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin

# NPM gloabl install
# npm config set prefix $HOME/.npm
export PATH=$PATH:$(npm config get prefix)/bin

# Add ~/.local/bin to PATH
# for hacks of all sorts
export PATH=$PATH:$HOME/.local/bin

# Nix tricks
# Avoid issues with `#` syntax for nix
alias nix="noglob nix"

# Silence direnv
# see https://github.com/direnv/direnv/issues/68
_direnv_hook() {
  eval "$(direnv export zsh 2> >( egrep -v -e '^direnv: (loading|export|unloading)' ))"
};

# eval dir env
eval "$(direnv hook zsh)"


# Set case-sensitivity for completion, history lookup, etc.
# This is sort of required to get reasonable performance
# see: https://github.com/sorin-ionescu/prezto/issues/2057
#zstyle ':prezto:*:*' case-sensitive 'yes'

