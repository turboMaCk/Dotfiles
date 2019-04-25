{ config, pkgs, ... }:
let agilePoker = import /home/marek/Projects/agile-poker/default.nix;
in
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
    google-fonts

    # KDE
    kdeApplications.korganizer # calednar
    kdeApplications.akonadi-mime

    gnome3.adwaita-icon-theme # testing taffy bar

    # Themes
    adapta-gtk-theme
    gnome3.adwaita-icon-theme
    xorg.xcursorthemes
    lxappearance
  ];

  # Packages
  nixpkgs.config = {
    allowUnfree = true; # :'(

    chromium = {
      jre = false;
      enableGoogleTalkPlugin = true;
      enableAdobeFlash = true;
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
    shadow          = true;
    fadeDelta       = 4;
    shadowOpacity   = "0.5";

    # opacity
    activeOpacity   = "1";
    inactiveOpacity = "0.9";
    menuOpacity     = "0.9";
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
