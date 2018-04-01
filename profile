# set language
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
export EDITOR='vim'

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/"

# setup path
export PATH="/usr/local/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
export PATH="$HOME/.cabal/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export LIBRARY_PATH="$LIBRARY_PATH:/usr/local/lib"

# Arch specific
export PATH="/sbin:$PATH"
export PATH="/usr/bin:$PATH"

# Mac with Home Brew specific
export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"

# NIX path
source ~/.nix-profile/etc/profile.d/nix.sh
nix-env -i nss-cacert
