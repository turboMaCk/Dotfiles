{ config, pkgs, ... }:
{
  imports = [
    ./desktop-base.nix
  ];

  environment.systemPackages = with pkgs; [
    # other xorg utils
    rxvt-unicode
    alacritty
    polybar
    feh

    xterm
    xdo
    xclip

    # xmonad miscelaneous
    rofi
    rofi-pass
    dunst # notifications

    kdePackages.dolphin
    kdePackages.qtsvg
    kdePackages.dolphin-plugins
    kdePackages.breeze-icons
    kdePackages.kio-fuse #to mount remote filesystems via FUSE
    kdePackages.kio-extras #extra protocols support (sftp, fish and more)
    kdePackages.konsole
    kdePackages.gwenview
    kdePackages.kservice
    kdePackages.plasma-workspace
  ];

  # Fix unpopulated MIME menus in dolphin
  environment.etc."/xdg/menus/applications.menu".text = builtins.readFile "${pkgs.kdePackages.plasma-workspace}/etc/xdg/menus/plasma-applications.menu";
  xdg.menus.enable = true;
  xdg.mime.enable = true;

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

  # Configure pinentry to QT
  # Otherwise I won't be able to sign commits
  # from Emacs
  programs.gnupg.agent.pinentryPackage = pkgs.pinentry-qt;

  services.displayManager = {
    sddm.enable = true;
  };

  # urxvtd
  services.urxvtd = {
    enable = true;
    package = pkgs.rxvt-unicode;
  };

  # autorandr
  services.autorandr.enable = true;

  # Redshift
  services.redshift = {
    enable = true;
    temperature.night = 2500;
  };

  # Dropbox service
  systemd.user.services.dropbox = {
    description = "Dropbox service";
    wantedBy = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "exec";
      ExecStart = "${pkgs.dropbox}/bin/dropbox";
      ExecStop = "${pkgs.procps}/bin/pkill dropbox";
      Restart = "on-failure";
    };
  };

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
