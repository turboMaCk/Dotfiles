{ config, pkgs, ... }:
{
  imports =
    [ ../profiles/devops.nix
      ../profiles/graphics.nix
      ../profiles/cpp.nix
      # ../profiles/latex.nix
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
      "jackaudio"
      "video"
      "networkmanager"
      "vboxusers"
      "docker"
      "systemd-jurnal"
      "input"
    ];

    # Shell
    shell = pkgs.zsh;

    packages = with pkgs; [
        dropbox
        slack
        discord
        spotify
        lf
        silver-searcher
        vlc
        gource
        weechat
        aspell
        aspellDicts.en
        aspellDicts.en
        aspellDicts.en-computers
        # Currently broken - fixed by 101194 - uncomment later
        # aspellDicts.en-science
        aspellDicts.cs
        zeal
        keybase-gui
        zoom-us
        thunderbird
        signal-desktop
    ];
  };
}
