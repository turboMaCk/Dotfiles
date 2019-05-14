# NixOS Config

Warning! This part of my dotfiles is still higly experimental, please don't rely on anything you see there!

My NixOS configuration per [machines](machines).

## Installation

Install NixOS default config as usual. Then edit `/etc/nixos/configuration.nix` to import machine settings from dotfiles repository.

Example:

```nix
# /etc/nixos/configuration.nix

# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      /home/marek/Dotfiles/nixos/machines/desktop.nix
    ];
}
```

> Note that `/etc/nixos/hardware-configuration.nix` is not part of this repository.

## Add a New Machine

Due to modularity of whole config it is pretty easy to compose new machine settings.
Just create new file in `/machines` and import profiles and user you want to include there.
