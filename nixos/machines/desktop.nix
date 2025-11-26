# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
{
  imports =
    [ <nixos-hardware/common/pc/ssd>
      ../nixpkgs.nix
      ../profiles/base.nix
      ../profiles/sound-pipewire.nix
      ../profiles/vulkan.nix
      ../profiles/kde.nix
      ../users/marek.nix
      ../profiles/virtualization.nix
      ../profiles/elm.nix
      ../profiles/nodejs.nix
      ../profiles/haskell.nix
      ../profiles/purescript.nix
      ../profiles/rust.nix
      # ../profiles/printing.nix
      ../profiles/holmusk.nix
      ../profiles/vpn.nix
      ../profiles/samba.nix
      # Extra
      ../profiles/stream.nix
      ../profiles/gaming.nix
      # ../profiles/rc.nix
     ../profiles/music.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    # Enable NTFS support
    supportedFilesystems = [ "ntfs" ];

    loader = {
      efi = {
        canTouchEfiVariables = true;
        # assuming /boot is the mount point of the  EFI partition in NixOS (as the installation section recommends).
        efiSysMountPoint = "/boot";
      };

      grub = {
        enable = true;
        efiSupport = true;

        # dualboot requires this
        useOSProber = true;
        device = "nodev";
      };
    };

    # Enable QEMU with aarch32
    binfmt.emulatedSystems = [ "armv7l-linux" "aarch64-linux" ];

    initrd.preLVMCommands = ''
      echo '  ______   __  __     ______     ______     ______     __    __     ______     ______     __  __  '
      echo '/\__  _\ /\ \/\ \   /\  == \   /\  == \   /\  __ \   /\ "-./  \   /\  __ \   /\  ___\   /\ \/ /   '
      echo '\/_/\ \/ \ \ \_\ \  \ \  __<   \ \  __<   \ \ \/\ \  \ \ \-./\ \  \ \  __ \  \ \ \____  \ \  _"-. '
      echo '   \ \_\  \ \_____\  \ \_\ \_\  \ \_____\  \ \_____\  \ \_\ \ \_\  \ \_\ \_\  \ \_____\  \ \_\ \_\'
      echo '    \/_/   \/_____/   \/_/ /_/   \/_____/   \/_____/   \/_/  \/_/   \/_/\/_/   \/_____/   \/_/\/_/'
    '';

    # Setup kernel
    # kernelPackages = pkgs.linuxPackages_5_10;

    kernelModules = [
      "iptable_nat"
      "iptable_filter"
      "xt_nat"
    ];
  };

  # Swapping to file because swap partitions are pointless
  swapDevices = [{
    device = "/var/lib/swapfile";
    size = 32*1024; # 32 GB
  }];

  # Expriemtnatl - ollama for LLMs
  services.ollama = {
    enable = true;
    acceleration = "rocm";
    #environmentVariables = {
      #HCC_AMDGPU_TARGET = "gfx1031"; # used to be necessary, but doesn't seem to anymore
    #};
    #rocmOverrideGfx = "10.3.1";
  };

  networking = {
    hostName = "nixos-mainframe"; # Define your hostname.
  };

  # Set your time zone.
  time.timeZone = "Europe/Prague";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
     libreoffice-qt
     hunspell
     hunspellDicts.en_US
   ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.

  # services.openssh.enable = true;
  # programs.ssh.startAgent = true;

  # networking.firewall.
  # Or disable the firewall altogether.

  # Force radv over amdvlk
  environment.variables.AMD_VULKAN_ICD = "RADV";

  hardware = {
    # graphic card requires this (AMDGPU)
    enableRedistributableFirmware = true;

    # Bluetooth
    bluetooth = {
      enable = true;
    };

    cpu.amd.updateMicrocode = true;

    # Enable logitech wheel controller
    new-lg4ff.enable = true;
  };

  # XORG
  services.xserver = {
    dpi = 130;
    videoDrivers = [ "amdgpu" ];

    serverFlagsSection = ''
      Option "BlankTime" "0"
      Option "StandbyTime" "0"
      Option "SuspendTime" "0"
      Option "OffTime" "0"
    '';
  };

  services.libinput.enable = true;

  # Enable ssh daemon
  services.openssh.enable = true;

  networking = {
    hosts."127.0.0.1" = [];
    # nameservers = ["1.1.1.1" "1.0.0.1"];
  };

  networking.nat = {
      enable = true;
      externalInterface = "enp5s0"; # see ifconfig for interface information
      # # forwardPorts = [{ sourcePort = 80; destination = "127.0.0.1:3000"; proto = "tcp"; }];
      forwardPorts = [{
        destination = "127.0.0.1:3000";
        proto = "tcp";
        sourcePort = 80;
      }];
  };

  networking.firewall = {
    enable = true;
    # allowedTCPPorts = [ 80 ];
    # allowedUDPPorts = [ ... ];
  };

  # Enable mitm proxy certs
  # security.pki.certificateFiles = [
  #   /home/marek/.mitmproxy/mitmproxy-ca.pem
  # ];

  # Configure postgres (ad hoc)
  services.postgresql = {
    enable = false;
    package = pkgs.postgresql_15;
    enableTCPIP = true;
    authentication = pkgs.lib.mkOverride 10 ''
      local all all trust
      host all all 127.0.0.1/32 trust
      host all all ::1/128 trust
    '';
  };

  services.avahi = {
    nssmdns4 = true;
    enable = true;
    ipv4 = true;
    ipv6 = true;

    publish = {
      enable = true;
      addresses = true;
      workstation = true;
    };
  };
}
