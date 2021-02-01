# My Dotfiles

This repository contains my dotfiles and setup **MacOS/NixOS** development machine.

**The purpose of this repository is to version my configuration as it evolves.
That's being said, you're free to use and modify any part of this config to suit your own needs.
However, this repository evolves a lot and from time to time can contain outdated information,
messy workarounds or even be broken.**

Usually I'm setting up my machine using just terminal and start by configuring shell and vim
which is the editor I prefer to use when working within terminal.
Once the shell is settuped I continue by configuration of other essetial tools which in my case is
Emacs and XMonad on X11 based system.

My primary OS is [NixOS](nixos.org/) and I use [Nix](https://nixos.org/manual/nix/stable/)
for many parts of my configuration. However my setup is not fully nix specific
since I like to use the same configuration on boxes without nix installed.
Primary I need to be able to setup zsh, vim emacs and xmonad environment on systems
without nix.

Shell and Vim configuration instructions are part of this README.
For instaraction about how to install different parts please fallow specific directory:

- Emacs installation [link](emacs.d)
- XMonad installation [link](xmonad)
- NixOS configuration [link](nixos)


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

Install [Shell Theme](https://github.com/turboMaCk/prezto-Prague-Shell)

```shell
$ cd ~/Dotfiles/prezto-Prague-Shell
$ ./bin/install.sh
```

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
