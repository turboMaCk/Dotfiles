{ config, pkgs, ... }:

let wire-desktop = pkgs.callPackage ../pkgs/wire-desktop.nix {};
in {
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.marek = {
    description = "Marek Fajkus (@turbo_MaCk)";
    isNormalUser = true;
    uid = 1000;

    extraGroups = [
      "wheel"
      "networkmanager"
      "vboxusers"
      "docker"
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
        emacs
        wire-desktop
        ranger

        # work
        nodejs-8_x
        yarn
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
