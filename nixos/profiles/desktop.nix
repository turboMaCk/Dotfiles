{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    dmenu
    xterm
    haskellPackages.xmobar
    taffybar
    feh
    chromium
    firefox
    transmission-gtk
    openssl
    spectacle # screeshots
    dunst # notifications
    gnome3.adwaita-icon-theme # testing taffy bar
  ];

  # Packages
  nixpkgs.config = {
    allowUnfree = true; # :'(

    chromium = {
      jre = false;
      enableGoogleTalkPlugin = true;
      enableAdobeFlash = false;
      enablePepperFlash = true;
      enablePepperPDF = true;
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
    layout = "us";
    xkbOptions = "eurosign:e";
    dpi = 130;

    # Display Manager
    displayManager = {
       sddm.enable = true;
       lightdm.enable = false;
    };

    desktopManager = {
      plasma5.enable = true;
      gnome3.enable = true;
      xfce.enable = false;
      default = "plasma5";
   };

    # Xmonad
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
         haskellPackages.xmonad-contrib
         haskellPackages.xmonad-extras
         haskellPackages.xmonad
      ];
    };
 };

  # Compton
  services.compton = {
    enable          = true;
    fade            = true;
    inactiveOpacity = "1.0";
    shadow          = true;
    fadeDelta       = 4;
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
      font-droid
      freefont_ttf
      google-fonts
      inconsolata
      liberation_ttf
      powerline-fonts
      source-code-pro
      terminus_font
      ttf_bitstream_vera
      ubuntu_font_family
    ];
  };
}