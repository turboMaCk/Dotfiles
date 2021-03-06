{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # xorg
    xterm
    xdo
    xclip

    # browsers
    firefox
    brave

    # GTK utils
    peek # screen recording to gif

    # Others
    flameshot # screeshots
    obs-studio

    # Themes
    xorg.xcursorthemes
    lxappearance

    # KDE utils
    gwenview
    transmission-qt
  ];

  # Packages
  nixpkgs.config = {
    firefox = {
      enablePepperPDF = true;
    };
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

  #=====================
  #      SERVICES      #
  #=====================

  # X11 settings
  services.xserver = {
    enable = true;
    # 2d graphics acceleration
    useGlamor = true;

    # Display Manager
    displayManager = {
       sddm.enable = true;
       lightdm.enable = false;
    };
  };

  # urxvtd
  services.urxvtd = {
    enable = true;
    package = pkgs.rxvt_unicode-with-plugins;
  };

  # Emacs
  services.emacs = {
    enable = true;
    package = pkgs.emacs27;
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
      ExecStop = "${pkgs.procps}/bin/pkill dropbox";
      Restart = "on-failure";
    };
  };

  # keybase
  services.keybase = {
    enable = true;
  };

  services.kbfs = {
    enable = true;
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
}
