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
    adapta-gtk-theme
    gnome3.adwaita-icon-theme
    gnome3.evince
    xorg.xcursorthemes
    lxappearance
    pavucontrol
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
       sddm.enable = false;
       lightdm.enable = true;
       defaultSession = "none+xmonad";
    };

    desktopManager = {
      gnome3.enable = true;
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

  # Fonts
  fonts = {
    enableFontDir = true;
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
