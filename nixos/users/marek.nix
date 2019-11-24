{ config, pkgs, ... }:

{
  imports =
    [ ../profiles/devops.nix
      ../profiles/graphics.nix
      ../profiles/cpp.nix
    ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.marek = {
    description = "Marek Fajkus (@turbo_MaCk)";
    isNormalUser = true;
    uid = 1000;
    createHome = true;
    home = "/home/marek";

    extraGroups = [
      "wheel"
      "disk"
      "audio"
      "video"
      "networkmanager"
      "vboxusers"
      "docker"
      "systemd-jurnal"
    ];

    # Shell
    shell = pkgs.zsh;

    packages = with pkgs; [
        steam
        browserpass
        dropbox
        slack
        discord
        spotify
        wire-desktop
        lf
        ag
        vlc
        gource
        weechat
        aspell
        aspellDicts.en
        aspellDicts.en
        aspellDicts.en-computers
        aspellDicts.en-science
        aspellDicts.cs
        zeal
        emacs26
    ];
  };

  ## Systemd Services

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

  # urxvtd
  services.urxvtd = {
    enable = true;
    package = pkgs.rxvt_unicode-with-plugins;
  };

  # keybase
  services.kbfs = {
    enable = true;
  };
  services.keybase = {
    enable = true;
  };

  # Emacs
  services.emacs = {
    enable = true;
    package = pkgs.emacs26;
  };
}
