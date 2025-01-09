#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#   Marek Fajkus <marek.faj@gmail.com>
#

# Use starship instead of prezto prompt
# if there is no nix install via: curl -sS https://starship.rs/install.sh | sh
eval "$(starship init zsh)"

# Triger `vcs_info 'prompt'` before loading Prezto.
# We want the first call to this happen before prezto will
# `unsetopt CASE_GLOB` which will have negative effect on the performance.
# Calling this before case sensitive globing makes initial start much faster.
# see: https://github.com/nix-community/home-manager/issues/2255
#autoload -Uz vcs_info
#vcs_info 'prompt'

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# prezto depends on coreutils
# This is fix for error in prompt on MacOS
# mac specific hack for GNU coreutils
#export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

# Vim as editor
export EDITOR="emacsclient --tty"

# Emacs as visual editor
export VISUAL="emacsclient -cn"

# Customize to your needs...

# When using sudo, use alias expansion (otherwise sudo ignores your aliases)

# Docker Machine ENV
function _dmenv() {
  eval $(docker-machine env $1)
};

# Source aliases
if [ -f ~/.aliases ]; then
    . ~/.aliases
fi

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
