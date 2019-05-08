# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      # ./hardware-configuration.nix
      ../profiles/base.nix
      ../profiles/desktop.nix
      ../users/marek.nix
      ../profiles/direnv.nix
      ../profiles/virtualization.nix
      ../profiles/nodejs.nix
      ../profiles/elm.nix
      ../profiles/haskell.nix
      ../profiles/heroku.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  networking = {
    hostName = "nixos-mainframe"; # Define your hostname.
    networkmanager.enable = true;
    wireless.enable = false;  # Enables wireless support via wpa_supplicant.
  };

  # Set your time zone.
  time.timeZone = "Europe/Prague";

  services.xserver.libinput.enable = false; # touchbar

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.

  # services.openssh.enable = true;
  # programs.ssh.startAgent = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;

  # HW support
  hardware = {
    # Sound config
    pulseaudio.enable = true;
    pulseaudio.support32Bit = true;

    # OpenGL
    opengl.driSupport32Bit = true;
  };

  services.xserver.layout = "us";
  services.xserver.dpi = 130;

  # HOSTS
  # networking.hosts = {
    # "35.233.47.118" = [ "pro-next.globalwebindex.net" ];
  # };

  # Set hosts
  # networking.hosts."128.199.58.247" = [ "planning-game.com" ];
  networking.hosts."35.244.244.204" = ["app.globalwebindex.com"];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
