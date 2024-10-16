{ config, pkgs, ... }:
{
  # Enable flakes
  nix = {
    package = pkgs.nix;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    settings.sandbox = true;
  };

  # !!! Insecure packages
  nixpkgs.config.permittedInsecurePackages = [
    "python2.7-Pillow-6.2.2"
    "python-2.7.18.6"
    "nodejs-16.20.0"
    "nodejs-14.21.3"
    "openssl-1.1.1t"
  ];

  # Allow unfree packages :'(
  nixpkgs.config = {
    allowUnfree = true;
    joypixels.acceptLicense = true;
  };

  # enable ntfs support via NTFS-3G
  boot.supportedFilesystems = [ "ntfs" ];

  # ZSH
  programs.zsh.enable = true;

  # Keyboard layouts
  services.xserver.xkb = {
    layout = "us,cz";
    variant = ",qwerty";
  };

  # Select internationalisation properties.
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  # Enable user access to QMK
  hardware.keyboard.qmk.enable = true;

  # Use 3rd party service to obtain location information
  location = {
    provider = "geoclue2";
  };
}
