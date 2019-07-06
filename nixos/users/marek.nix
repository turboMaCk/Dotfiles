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
        rxvt_unicode
        steam
        browserpass
        dropbox
        obs-studio
        slack
        spotify
        emacs26
        wire-desktop
        ranger
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
    ];
  };

  ## Systemd Services

  systemd.user.services."urxvtd" = {
    enable = true;
    description = "rxvt unicode daemon";
    wantedBy = [ "default.target" ];
    path = [ pkgs.rxvt_unicode ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.rxvt_unicode}/bin/urxvtd -q -o";
  };
}
