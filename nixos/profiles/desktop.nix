{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    xterm
    polybar
    feh
    chromium
    firefox
    transmission-gtk
    openssl
    flameshot # screeshots
    dunst # notifications
    google-fonts
    xclip

    # Themes
    adapta-gtk-theme
    gnome3.adwaita-icon-theme
    empathy
    xorg.xcursorthemes
    lxappearance
    xmonad-log
    rofi
    rofi-pass
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
      jre = false;
      enableGoogleTalkPlugin = true;
      enableAdobeFlash = false;
      enablePepperPDF = true;
    };
  };

  # X11 settings
  services.xserver = {
    enable = true;

    # Display Manager
    displayManager = {
       sddm.enable = true;
       lightdm.enable = false;
    };

    desktopManager = {
      plasma5.enable = false;
      gnome3.enable = true;
      xfce.enable = false;
      default = "none";
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

  # Compton
  services.compton = {
    enable          = true;
    fade            = true;
    shadow          = true;
    fadeDelta       = 4;
    shadowOpacity   = "0.5";

    # opacity
    activeOpacity   = "1.0";
    inactiveOpacity = "1.0";
    menuOpacity     = "1.0";
    backend         = "glx";
  };

  # Redshift
  services.redshift = {
    enable = true;
    latitude = "49.1165";
    longitude = "16.594";
    temperature.night = 2500;
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
