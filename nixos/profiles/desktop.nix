{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    xterm
    polybar
    feh
    firefox
    brave
    transmission-gtk
    openssl
    flameshot # screeshots
    dunst # notifications
    xclip
    xmonad-log
    rofi
    rofi-pass
    obs-studio
    peek # screen recording to gif

    # Themes
    xorg.xcursorthemes
    lxappearance
    pavucontrol

    # KDE utils
    gwenview
  ];

  # Packages
  nixpkgs.config = {
    firefox = {
      # jre = false;
      # enableGoogleTalkPlugin = true;
      enableAdobeFlash = false;
      enablePepperPDF = true;
    };
  };

  # X11 settings
  services.xserver = {
    enable = true;
    # 2d graphics acceleration
    useGlamor = true;

    # Display Manager
    displayManager = {
       sddm.enable = true;
       lightdm.enable = false;
       defaultSession = "none+xmonad";
    };

    desktopManager = {
      gnome3.enable = false;
      plasma5.enable = true;
   };

    # Xmonad
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
         haskellPackages.xmonad-contrib
         haskellPackages.xmonad-extras
         haskellPackages.xmonad
         haskellPackages.dbus
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
      ExecStop = "${pkgs.procps}/bin/pkill dropbnox";
      Restart = "on-failure";
    };
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

  # keybase-gui service
  systemd.user.services.keybase-gui = {
    description = "Keybase Gui";
    wantedBy = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "exec";
      ExecStart = "${pkgs.keybase-gui}/bin/keybase-gui";
      ExecStop = "${pkgs.procps}/bin/pkill keybase-gui";
      Restart = "on-failure";
    };
  };

  # matter-most service
  systemd.user.services.mattermost-desktop = {
    description = "Mattermost desktop";
    wantedBy = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "exec";
      ExecStart = "${pkgs.mattermost-desktop}/bin/mattermost-desktop --hidden";
      ExecStop = "${pkgs.procps}/bin/pkill mattermost-desktop";
      Restart = "on-failure";
    };
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
    package = pkgs.emacs27;
  };

  # Fonts
  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      anonymousPro
      corefonts
      dejavu_fonts
      noto-fonts
      freefont_ttf
      google-fonts
      inconsolata
      liberation_ttf
      powerline-fonts
      source-code-pro
      terminus_font
      ttf_bitstream_vera
      ubuntu_font_family
      emojione
    ];
  };
}
