{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # xorg
    alacritty

    # browsers
    firefox
    google-chrome

    # Others
    flameshot # screeshots
    obs-studio
    gthumb # image viewer
    peek # screen recorder


    # Themes
    xorg.xcursorthemes
    lxappearance
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
    packages = with pkgs; [
      joypixels
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
      #emojione broken 
    ];
  };

  #=====================
  #      SERVICES      #
  #=====================

  # X11 settings
  services.xserver = {
    enable = true;
  };

  services.displayManager = {
    sddm.enable = true;
  };

  # urxvtd
  services.urxvtd = {
    enable = true;
    package = pkgs.rxvt-unicode;
  };

  # Emacs
  services.emacs = {
    enable = true;
    package = pkgs.emacs;
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
  # systemd.user.services.mattermost-desktop = {
  #   description = "Mattermost desktop";
  #   wantedBy = [ "graphical-session.target" ];
  #   serviceConfig = {
  #     Type = "exec";
  #     ExecStart = "${pkgs.mattermost-desktop}/bin/mattermost-desktop --hidden";
  #     ExecStop = "${pkgs.procps}/bin/pkill mattermost-desktop";
  #     Restart = "on-failure";
  #     TimeoutStartSec = "3600";
  #   };
  # };
}
