My Dotfiles
===========
This repository contains my dotfiles and setup **macOS/NixOS** development machine.

**Purpose of this repository is to archive my configuration as it evolves and share
usefull parts of my config with other. That's being say you're free to use and modify
any part of this config to suit your own needs however, this repository evolves a lot
and from time to time can be contain outdated information or messy workarounds.**

## Shell
I'm ZSH and prezto user. I also use my [custom theme](https://github.com/turboMaCk/prezto-Prague-Shell).

## Install Dotfiles

I'm using only git for instalation. I always clone this repository to Home.

On Mac command line tools are required:

```shell
$ xcode-select --install # mac only
```

**Note that this clone is using HTTPS and not SSH. At the time clone I usually don't have ssh setuped yet!**

```shell
git clone --recursive https://github.com/turboMaCK/Dotfiles.git "$HOME/Dotfiles"
```

Switch to ZSH (don't forget to install it!)

```shell
$ zsh
```

This is how zsh can be set as default shell on OS other than NixOS (Mac for instance)

```shell
$ chsh -s /bin/zsh
```

This installation is broken:

>   Install Shell Theme
>
>   ```shell
>   $ sh ~/Dotfiles/prezto-Prague-Shell/bin/install.sh
>   ```

To install dotfiles themselves you can run the script that cerates symlink in home directlory

```shell
$ sh ~/Dotfiles/bin/dotfiles.sh
```
