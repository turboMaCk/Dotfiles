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
      ../users/nikola.nix
      ../profiles/virtualization.nix
      ../profiles/elm.nix
      ../profiles/nodejs.nix
      ../profiles/haskell.nix
      ../profiles/purescript.nix
      ../profiles/rust.nix
      ../profiles/printing.nix
      ../profiles/holmusk.nix
      ../profiles/vpn.nix
      ../profiles/samba.nix
      # Extra
      ../profiles/stream.nix
      ../profiles/gaming.nix
      ../profiles/rc.nix
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

      # Using grub instead of systemd-boot
      # see https://github.com/NixOS/nixpkgs/issues/97426
      systemd-boot.enable = false;

      grub = {
        enable = true;
        efiSupport = true;

        # dualboot requires this
        useOSProber = true;
        device = "nodev";
      };
    };

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

  # Enable sound.
  sound.enable = true;

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

    libinput.enable = false; # touchbar

    imwheel = {
      enable = true;
      rules = {
        "chrom*|slack|discord|evolution|Firefox|brave-browser" = ''
        None,      Up,   Button4, 4
        None,      Down, Button5, 4
        Shift_L,   Up,   Shift_L|Button4, 4
        Shift_L,   Down, Shift_L|Button5, 4
        Control_L, Up,   Control_L|Button4
        Control_L, Down, Control_L|Button5
      '';
      };
      extraOptions = [
        "--buttons=45"
      ];
    };
  };

  # Enable ssh daemon
  services.openssh.enable = true;

  # Restart USB because mouse don't like to wake up
  powerManagement.powerDownCommands = ''
    ${pkgs.gnugrep}/bin/grep 'XHC.*enable' /proc/acpi/wakeup | ${pkgs.gawk}/bin/awk '{print $4}' | ${pkgs.gnused}/bin/sed -e 's/pci://g' > /sys/bus/pci/drivers/xhci_hcd/unbind
  '';

  # Wake up hack
  powerManagement.resumeCommands = ''
    ${pkgs.gnugrep}/bin/grep 'XHC.*enable' /proc/acpi/wakeup | ${pkgs.gawk}/bin/awk '{print $4}' | ${pkgs.gnused}/bin/sed -e 's/pci://g' > /sys/bus/pci/drivers/xhci_hcd/bind
  '';

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
}
