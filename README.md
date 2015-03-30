# turbo_MaCk's dotfiles
This repository contains my dotfiles and setup mostly optipalized for **OSX**.

Submodules
==========
* My [Prezto](https://github.com/sorin-ionescu/prezto) fork (zsh plugins + config) [turbo_MaCk/prezto](https://github.com/turboMaCk/prezto)
* Custom prezto theme [turboMaCk/prezto-Prague-Shell](https://github.com/turboMaCk/prezto-Prague-Shell)

Requirements
============
* OSX
* XCode + Command Line Tools

Install via bin/install.sh
==========================
* Homebrew (ruby + curl)
* Vundle - vim package managing (git install)
* ssh-copy-id script (curl install)

Developement enviroment managing
================================
* rbenv (homebrew install)
* nvm (homebrew install)
* pyenv (homebrew install)
* phantom.js (homebrew install)o
* dnsmasq (homebrew install)
* Vagrant (brew cask install)

Editors
=======
* vim (homebrew install)
* neoviem (homebrew install)
* macvim (homebrew install)
* sublime-text (**hand install**)

Binaries and utils (all via homebrew)
=====================================
* coreutils
* wget (iri enabled)
* vim
* grep
* the silver searcher
* git
* git flow
* tig
* ctags
* imagemagic (with webp)
* node (just system version)
* openssl
* open ssh
* tmux
* osxfuse
* htop-osx
* memcached
* apache ab

Databases (all via homebrew)
============================
* mysql
* postgresql
* sqlite
* redis
* mongo

Browsers (all via brewcask)
===========================
* Google Chrome
* Google Chrome Canary
* Firefox
* Opera

Open source applications (all via brewcask)
===========================================
* Transmission
* VLC
* Adium
* iTerm2
* Total Terminal
* Vagrant
* Tunelblick

Proprietary applications (all via brew cask)
============================================
* DropBox
* The Unarchiver
* Skype
* Alfred
* Transmit
* Tower Git
* Codekit

Pre-install setup
=================
Install command-line tools for XCode after install XCode via Appstore

Switch to ZSH
```shell
$ zsh
```

```shell
$ xcode-select --install
```

Install Dotfiles
================

download with submodules
```shell
$ git clone --recursive https://github.com/turboMaCk/Dotfiles "${ZDOTDIR:-$HOME}/Dotfiles"
```

Clone Zprezto
```shell
$ git clone --recursive git@github.com:turboMaCk/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
```

Create a new Zsh configuration by copying the Zsh configuration files from prezto
```shell
$ setopt EXTENDED_GLOB
for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
  ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done
```

Set Zsh as your default shell
```shell
$ chsh -s /bin/zsh
```

Install Shell Theme
```shell
$ sh ~/Dotfiles/prezto-Prague-Shell/bin/install.sh
```

Install Dontfiles itself
```shell
$ sh ~/Dotfiles/bin/dotfiles.sh
```

### Install Homebrew + Vundle
This script will install Homebrew, Vundle and ssh-copy-id script and setup directories for vim settings.

```shell
$ sh ~/Dotfiles/bin/install.sh
```

### setup OSX
This is experimantal script and might be broken on some machines.

```shell
$ sh bin/osx.sh
```
