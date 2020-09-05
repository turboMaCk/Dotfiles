{ config, pkgs, ... }:

let
  my_vim =
    pkgs.vim_configurable.override {
      python = pkgs.python3;
      name = "my-vim";
    };

  nix-prune-roots = pkgs.writeScriptBin "nix-prune-roots" ''
    #!/usr/bin/env bash

    set -e
    set -o nounset
    set -o pipefail

    echo "Removing all the GC roots located within HOME."
    nix-store --gc --print-roots | grep ~ | cut -d" " -f1 | xargs -0 rm
    echo "Done! Run `nix-collect-garbage -d` to gain more disk space."
  '';
in
{
  environment.systemPackages = with pkgs; [
    tmux
    cask
    which
    nix-prefetch-scripts
    nix-prune-roots
    python
    python3
    my_vim
    lsof
    wget
    git
    gitAndTools.tig
    pass
    gnupg
    gcc
    clang
    gnumake
    htop
    cloc
    tree
    unzip
    killall
    libnotify
    direnv
    cachix
    # package seems to be broken
    # tldr-hs
    direnv

    #vpn
    networkmanager-l2tp
    networkmanager_strongswan
  ];

  # Allow unfree packages :'(
  nixpkgs.config.allowUnfree = true;

  services.xserver.layout = "us,cz";

  # Select internationalisation properties.
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  # ZSH
  programs.zsh.enable = true;

  # GnuPG
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  services.pcscd.enable = true;

  # lorri
  services.lorri.enable = false;

  location = {
    provider = "geoclue2";
  };

  environment.shellInit = ''
    gpg-connect-agent /bye
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
  '';

  nix.useSandbox = true;
}
