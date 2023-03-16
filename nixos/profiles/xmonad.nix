{ config, pkgs, ... }:
{
  imports = [
    ./desktop-base.nix
  ];

  environment.systemPackages = with pkgs; [
    # other xorg utils
    polybar
    feh

    # xmonad miscelaneous
    rofi
    rofi-pass
    dunst # notifications

    # gtk
    gnome.nautilus
    nautilus-open-any-terminal
    font-manager
  ];

  # X11 settings
  services.xserver = {
    # Xmonad
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
         haskellPackages.xmonad-contrib
         haskellPackages.xmonad-extras
         haskellPackages.xmonad
         haskellPackages.utf8-string
         haskellPackages.raw-strings-qq
      ];
    };
  };

  # autorandr
  services.autorandr.enable = true;

  # Picom compositor (compton alternative)
  services.picom = {
    enable          = true;
    fade            = true;
    shadow          = true;
    fadeDelta       = 4;
    shadowOpacity   = 0.5;

    # opacity
    activeOpacity   = 1.0;
    inactiveOpacity = 1.0;
    menuOpacity     = 1.0;

    # OpenGL
    backend         = "glx";
    vSync           = true;
  };

  # nm-applet service
  systemd.user.services.nm-applet = {
    description = "NetworkManagerApplet service";
    wantedBy = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "exec";
      ExecStart = "${pkgs.networkmanagerapplet}/bin/nm-applet";
      ExecStop = "${pkgs.procps}/bin/pkill nm-applet";
      Restart = "on-failure";
    };
  };
}
