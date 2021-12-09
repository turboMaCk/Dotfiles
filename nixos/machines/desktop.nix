# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
{
  imports =
    [ <nixos-hardware/common/pc/ssd>
      ../nixpkgs.nix
      ../profiles/base.nix
      ../profiles/sound-traditional.nix
      ../profiles/vulkan.nix
      # ../profiles/kde.nix
      ../profiles/xmonad.nix
      ../users/marek.nix
      ../users/nikola.nix
      ../profiles/virtualization.nix
      ../profiles/elm.nix
      ../profiles/nodejs.nix
      ../profiles/haskell.nix
      ../profiles/purescript.nix
      ../profiles/rust.nix
      ../profiles/ocaml.nix
      ../profiles/printing.nix
      ../profiles/holmusk.nix
      ../profiles/vpn.nix
      ../profiles/samba.nix
      # Extra
      # ../profiles/data-science.nix
      # ../profiles/unison.nix
      ../profiles/stream.nix
      ../profiles/gaming.nix
      ../profiles/admin.nix # things for booring administrative tasks
      ../profiles/rc.nix
      ../profiles/music.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader = {

      # Using grub instead of systemd-boot
      # see https://github.com/NixOS/nixpkgs/issues/97426
      systemd-boot.enable = false;

      grub = {
        enable = true;
        efiSupport = true;
        efiInstallAsRemovable = true;
        device = "/dev/nvme0n1";
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
  };

  # virtualbox is not compatible with the 5.10 kernel
  virtualisation.virtualbox.host.enable = false;

  networking = {
    hostName = "nixos-mainframe"; # Define your hostname.
  };

  # Set your time zone.
  time.timeZone = "Europe/Prague";

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

  # Enable sound.
  sound.enable = true;

  hardware = {
    # graphic card requires this (AMDGPU)
    enableRedistributableFirmware = true;

    opengl = {
      enable = true;
      driSupport = true;

      # Vulkan
      extraPackages = with pkgs; [
        amdvlk
      ];

      extraPackages32 = with pkgs; [
        driversi686Linux.amdvlk
      ];
    };

    # Bluetooth
    bluetooth = {
      enable = true;
    };

    cpu.amd.updateMicrocode = true;
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

  # Fix XHCI suspend bug
  # see: https://github.com/NixOS/nixpkgs/issues/109048
  powerManagement.powerDownCommands = ''
    ${pkgs.gnugrep}/bin/grep 'XHC.*enable' /proc/acpi/wakeup | ${pkgs.gawk}/bin/awk '{print $4}' | ${pkgs.gnused}/bin/sed -e 's/pci://g' > /sys/bus/pci/drivers/xhci_hcd/unbind
  '';

  # Wake up hack
  powerManagement.resumeCommands = ''
    ${pkgs.gnugrep}/bin/grep 'XHC.*enable' /proc/acpi/wakeup | ${pkgs.gawk}/bin/awk '{print $4}' | ${pkgs.gnused}/bin/sed -e 's/pci://g' > /sys/bus/pci/drivers/xhci_hcd/bind
  '';

  networking.hosts."127.0.0.1" = [ "meadow.cdmp.local" "coach.meadow.cdmp.local" ];
  networking.nameservers = ["1.1.1.1" "1.0.0.1"];
}
