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
    #cachix

    #vpn
    networkmanager-l2tp
    networkmanager_strongswan
  ];

  # Allow unfree packages :'(
  nixpkgs.config.allowUnfree = true;

  services.xserver.layout = "us";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
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

  location = {
    provider = "geoclue2";
  };

  environment.shellInit = ''
    gpg-connect-agent /bye
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
  '';

  nix.useSandbox = true;
}
