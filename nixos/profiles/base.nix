{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    nix-prefetch-scripts
    nix-repl
    python
    python3
    (vim_configurable.override { python3 = true; })
    lsof
    wget
    git
    pass
    gnupg
    gcc
    gnumake
    htop
  ];

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
  environment.shellInit = ''
    gpg-connect-agent /bye
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
  '';

}
