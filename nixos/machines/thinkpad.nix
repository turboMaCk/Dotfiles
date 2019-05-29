# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ <nixos-hardware/lenovo/thinkpad/t460s>
      ../profiles/base.nix
      ../profiles/desktop.nix
      ../users/marek.nix
      ../profiles/virtualization.nix
      ../profiles/elm.nix
      ../profiles/nodejs.nix
      ../profiles/haskell.nix
      ../profiles/purescript.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    initrd.luks.devices = [
      {
        name = "root";
        device = "/dev/nvme0n1p3";
        preLVM = true;
      }
    ];
  };

  networking.hostName = "nixos"; # Define your hostname.
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Set your time zone.
  time.timeZone = "Europe/Prague";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    xlockmore
  ];

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;

  hardware.pulseaudio = {
    enable = true;
    # required for bluetooth package
    package = pkgs.pulseaudioFull;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
  };

  # X11 XKB key map
  # Mainly Caps -> Ctrl
  services.xserver.xkbOptions = "ctrl:nocaps,caps:none,shift:both_capslock,lv3:rwin_switch,grp:alt_space_toggle,altwin:swap_alt_win";

  # screen locking
  services.xserver.xautolock = {
    enable = true;
    locker = "${pkgs.xlockmore}/bin/xlock -mode ant";
    extraOptions = [ "-detectsleep" ];
    killer = "${pkgs.systemd}/bin/systemctl suspend";
    killtime = 10; # 10 is minimal value
    time = 5;
  };
  services.logind.lidSwitch = "suspend";

  # xss-lock subscribes to the systemd-events suspend, hibernate, lock-session,
  # and unlock-session with appropriate actions (run locker and wait for user to unlock or kill locker).
  # xss-lock also reacts to DPMS events and runs or kills the locker in response.
  programs.xss-lock.enable = true;
  programs.xss-lock.lockerCommand = "${pkgs.xlockmore}/bin/xlock -mode ant";

  # Enable touchpad support.
  services.xserver.libinput = {
    enable = true;
    scrollMethod = "twofinger";
    naturalScrolling = true;
    disableWhileTyping = true;
    clickMethod = "clickfinger";
  };

  # Trackpoint settings
  hardware.trackpoint = {
    enable = true;
    emulateWheel = true;
    sensitivity = 128; # default kernel value is 128
    speed = 110; # default kernel value is 97
  };

  # Bluetooth
  hardware.bluetooth = {
    enable = true;
  };

  # Brightness service
  systemd.services.brightness = {
    description = "Set brightness writable to everybody";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      User = "root";
      ExecStart = ''
        ${pkgs.bash}/bin/bash -c "chgrp -R -H video /sys/class/backlight/intel_backlight && chmod g+w /sys/class/backlight/intel_backlight/brightness";
      '';
    };
  };

  # Set hosts
  # networking.hosts."128.199.58.247" = [ "planning-game.com" ];
  networking.hosts."35.244.244.204" = ["app.globalwebindex.com"];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
