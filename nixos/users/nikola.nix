{ config, pkgs, ... }:

{
  imports =
    [];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.nikola = {
    description = "Zlobik";
    isNormalUser = true;
    uid = 2000;
    createHome = true;
    home = "/home/nikola";

    extraGroups = [
      "wheel"
      "disk"
      "audio"
      "video"
      "networkmanager"
      "systemd-jurnal"
      "input"
    ];

    # Shell
    shell = pkgs.zsh;

    packages = with pkgs; [
        slack
        discord
        spotify
        vlc
        gource
        aspell
        aspellDicts.en
        aspellDicts.cs
    ];
  };
}
