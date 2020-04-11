{ config, pkgs, ... }:

let
  my_vim =
    pkgs.vim_configurable.override { python3 = true; };
in
{
  environment.systemPackages = with pkgs; [
    which
    nix-prefetch-scripts
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
    lorri
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
