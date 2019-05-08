{ config, pkgs, ... }:

let
  my_vim =
    pkgs.vim_configurable.override { python3 = true; };
in
{
  environment.systemPackages = with pkgs; [
    nix-prefetch-scripts
    python
    python3
    my_vim
    lsof
    wget
    git
    pass
    gnupg
    gcc
    gnumake
    htop
    cloc
    tree
    unzip
  ];

  # Cachix
  nix = {
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://komposition.cachix.org"
    ];
    binaryCachePublicKeys = [
      "komposition.cachix.org-1:nzWESzP0bEENshGnqQYN8+mic6JOxw2APw/AJAXhF3Y="
    ];
    trustedUsers = [ "root" "marek" ];
  };

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
    #consoleUseXkbConfig = true;
  };

  # ZSH
  programs.zsh.enable = true;

  # GnuPG
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  services.pcscd.enable = true;

  environment.shellInit = ''
    gpg-connect-agent /bye
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
  '';

  nix.useSandbox = false;
}
