{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
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
      ubuntu-classic
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

  # Emacs
  services.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  # keybase
  services.keybase = {
    enable = true;
  };

  services.kbfs = {
    enable = true;
  };
}
