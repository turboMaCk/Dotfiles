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
    google-fonts
    xclip
    xmonad-log
    rofi
    rofi-pass
    obs-studio
    peek # screen recording to gif
    networkmanagerapplet

    # Themes
    xorg.xcursorthemes
    lxappearance
    pavucontrol

    # KDE utils
    gwenview
  ];

  # Packages
  nixpkgs.config = {
    chromium = {
      # enableGoogleTalkPlugin = true;
      # enableAdobeFlash = true;
      # enablePepperFlash = true;
      # enablePepperPDF = true;
      # enableWideVine = true;
    };

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

  # Compton
  services.compton = {
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
    wantedBy = [ "graphical-session.targe" ];
    serviceConfig = {
      Type = "exec";
      ExecStart = "${pkgs.dropbox}/bin/dropbox";
      ExecStop = "${pkgs.procps}/bin/pkill dropbnox";
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
    package = pkgs.emacs26;
  };

  # Fonts
  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      anonymousPro

      # corefonts TODO: fix hash missmatch
      # wanted: sha256:0baadsrgpqj15fgjmcn0aim0k0nk7mvivcxinw1zwg61kkcwhalx
      # got:    sha256:089d2m9bvaacj36qdq77pcazji0sbbr796shic3k52cpxkjnzbwh

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
