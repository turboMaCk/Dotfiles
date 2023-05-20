{ config, pkgs, ... }:

let
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
  # Enable flakes
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    settings.sandbox = true;
  };

  # enable ntfs support via NTFS-3G
  boot.supportedFilesystems = [ "ntfs" ];

  environment.systemPackages = with pkgs; [
    tmux
    cask
    which
    nix-prefetch-scripts
    nix-prune-roots
    python
    python3
    vim_configurable
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
    jq
    file
    # package seems to be broken
    # tldr-hs
    direnv
    openssl

    #vpn
    networkmanager-l2tp
    networkmanager_strongswan
  ];

  # !!! Insecure packages
  nixpkgs.config.permittedInsecurePackages = [
    "python2.7-Pillow-6.2.2"
    "python-2.7.18.6"
    "nodejs-16.20.0"
    "nodejs-14.21.3"
    "openssl-1.1.1t"
  ];

  # Allow unfree packages :'(
  nixpkgs.config.allowUnfree = true;

  services.xserver.layout = "us,cz";
  services.xserver.xkbVariant = ",qwerty";

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
}
