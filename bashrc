# source aliases
if [ -f ~/.aliases ]; then
    . ~/.aliases
fi

eval "$(starship init bash)"
