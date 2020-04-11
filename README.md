# My Dotfiles

This repository contains my dotfiles and setup **MacOS/NixOS** development machine.

**Purpose of this repository is to archive my configuration as it evolves and share
usefull parts of my config with other. That's being say you're free to use and modify
any part of this config to suit your own needs however, this repository evolves a lot
and from time to time can be contain outdated information, messy workarounds
or even be broken.**

Usually when I'm setting up new box I spent quite some tome just in terminal setting up bouch of things.
To make this more pleasant experience I like to install my shell (ZSH) and configuration and vim config.
Once the shell is settuped I continue by configuration of other essetial tools which in my case is
Emacs and XMonad on X11 based system.

Shell and Vim instruction are part of this README.
For instaraction how to install different parts please fallow specific directory:

- Emacs installation [link](/emacs.d)
- XMonad installation in X11 systems [link](xmonad)


## Install Dotfiles

I'm using just git for instalation so it has to be installed and the begining of installation procees
depending on OS.

On Mac command line tools are required.

```shell
$ xcode-select --install # mac only
```

I always clone this repository to Home of a user.

**Note that this clone is using HTTPS and not SSH.
At the time clone I usually don't have ssh setuped yet!**

```shell
git clone --recursive https://github.com/turboMaCK/Dotfiles.git "$HOME/Dotfiles"
```

## Updating

Updating happens just via git. There is a shell script that automates update of git repository
with all submodules.

```shell
$ ./bin/update.sh
```

## Shell

I'm ZSH and Prezto user. I also use my [custom theme](https://github.com/turboMaCk/prezto-Prague-Shell).

Switch to ZSH (don't forget to install it first on the OS!)

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

To install Dotfiles themselves you can run the script that cerates symlink in home directlory.

```shell
$ sh ~/Dotfiles/bin/dotfiles.sh
```

## Git SSH

Don't forget to

```
$ ssh-add ~/.ssh/id_rsa # or better than rsa
```

so git is not asking for passphrase on every remote action.
