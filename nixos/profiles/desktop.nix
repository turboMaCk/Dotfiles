{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    dmenu
    xterm
    haskellPackages.xmobar
    feh
    chromium
    firefox
    transmission-gtk
    vlc
  ];

  # packages

  nixpkgs.config = {
    allowUnfree = true; # :'(

    chromium = {
      jre = false;
      enableGoogleTalkPlugin = true;
      enableAdobeFlash = false;
      enablePepperFlash = false;
      enablePepperPDF = true;
      enableWideVine = false; # DRM shit
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
      gnome3.enable = false;
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
