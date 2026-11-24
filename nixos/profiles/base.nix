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

  # !!! Insecure packages
  nixpkgs.config.permittedInsecurePackages = [
    #"openssl-1.1.1t"
    "keybase-gui-6.6.3"
    "clion-2026.2.4.1"
  ];

  # Enable flakes
  nix = {
    package = pkgs.nix;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    settings.sandbox = true;
  };

  # Allow unfree packages :'(
  nixpkgs.config = {
    allowUnfree = true;
    joypixels.acceptLicense = true;
  };

  # enable ntfs support via NTFS-3G
  boot.supportedFilesystems = [ "ntfs" "nfs" ];

  environment.systemPackages = with pkgs; [
    tmux
    which
    dig
    nix-prefetch-scripts
    nix-prune-roots
    python3
    vim-full
    lsof
    wget
    git
    tig
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
    ripgrep
    jq
    file
    zip
    direnv
    openssl

    # shell prompt
    starship

    # Github cli utlility
    gh

    # manpages
    linux-manual man-pages man-pages-posix man-pages-posix
  ];

  services.xserver.xkb = {
    layout = "us,cz";
    variant = ",qwerty";
  };

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

  # Enable QMK
  hardware.keyboard.qmk.enable = true;

  location = {
    provider = "geoclue2";
  };

  documentation = {
    enable = true;
    man = {
      enable = true;
      cache.enable = true;
      cache.generateAtRuntime = true;
    };
  };

  environment.shellInit = ''
    gpg-connect-agent /bye
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
  '';
}
