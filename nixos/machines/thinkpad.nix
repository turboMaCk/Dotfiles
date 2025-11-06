# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ <nixos-hardware/lenovo/thinkpad/t480s>
      ../nixpkgs.nix
      ../profiles/base.nix
      ../profiles/sound-pipewire.nix
      ../profiles/xmonad.nix
      ../users/marek.nix
      ../profiles/virtualization.nix
      ../profiles/elm.nix
      ../profiles/nodejs.nix
      ../profiles/haskell.nix
      ../profiles/purescript.nix
      ../profiles/rust.nix
      #../profiles/printing.nix
      ../profiles/holmusk.nix
      ../profiles/vpn.nix
      ../profiles/samba.nix
      ../profiles/gaming.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    # Enable QEMU with aarch64
    binfmt.emulatedSystems = [ "aarch64-linux" "armv7l-linux" ];

    # Kernel 5.4 has issues with i915 driver (intel gpu)
    #kernelPackages = pkgs.linuxPackages_4_19;

    initrd.preLVMCommands = ''
      echo '  ______   __  __     ______     ______     ______     __    __     ______     ______     __  __  '
      echo '/\__  _\ /\ \/\ \   /\  == \   /\  == \   /\  __ \   /\ "-./  \   /\  __ \   /\  ___\   /\ \/ /   '
      echo '\/_/\ \/ \ \ \_\ \  \ \  __<   \ \  __<   \ \ \/\ \  \ \ \-./\ \  \ \  __ \  \ \ \____  \ \  _"-. '
      echo '   \ \_\  \ \_____\  \ \_\ \_\  \ \_____\  \ \_____\  \ \_\ \ \_\  \ \_\ \_\  \ \_____\  \ \_\ \_\'
      echo '    \/_/   \/_____/   \/_/ /_/   \/_____/   \/_____/   \/_/  \/_/   \/_/\/_/   \/_____/   \/_/\/_/'
    '';

    initrd.luks.devices = {
      root = {
        device = "/dev/nvme0n1p3";
        preLVM = true;
      };
    };
  };

  networking = {
    hostName = "nixos"; # Define your hostname.
  };

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
 networking.firewall.allowedTCPPorts = [ 8000 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  #services.thinkfan.enable = true;
  # ACPID power saving
  services.acpid = {
    enable = true;
    handlers = {
        ac-power =
          {
            action =
              ''
                vals=($1)  # space separated string to array of multiple values
                case ''${vals[3]} in
                    00000000)
                        echo unplugged >> /tmp/acpi.log
                        ;;
                    00000001)
                        echo plugged in >> /tmp/acpi.log
                        ;;
                    *)
                        echo unknown ''${vals[3]} >> /tmp/acpi.log
                        ;;
                esac

              '';
            event = "ac_adapter/*";
          };
    };
  };

  hardware = {
    # Trackpoint settings
    trackpoint = {
      enable = true;
      emulateWheel = true;
      sensitivity = 128; # default kernel value is 128
      speed = 97; # default kernel value is 97
    };

    # Bluetooth
    bluetooth = {
      enable = true;

      # Modern headsets will generally try to connect
      # using the A2DP profile
      #extraConfig = "
        #[General]
        #Enable=Source,Sink,Media,Socket
      #";
    };

    # GPU support
    graphics.extraPackages = [ pkgs.intel-vaapi-driver ];
  };

  services.xserver = {
    # X11 XKB key map
    # Mainly Caps -> Ctrl
    # check https://www.x.org/releases/X11R7.6/doc/xorg-docs/input/XKB-Config.html
    xkb.options = "ctrl:nocaps,caps:none,lv3:rwin_switch,grp:alt_space_toggle,altwin:swap_alt_win";

    # Enable touchpad support.
  };

  services.libinput = {
      enable = true;

      touchpad = {
        scrollMethod = "twofinger";
        naturalScrolling = true;
        disableWhileTyping = true;
        clickMethod = "clickfinger";
      };
    };

  # Don't sleep
  services.xserver = {
    serverFlagsSection = ''
      Option "BlankTime" "0"
      Option "StandbyTime" "0"
      Option "SuspendTime" "0"
      Option "OffTime" "0"
    '';
  };

  services.logind.settings.Login.HandleLidSwitch = "suspend";

  # xss-lock subscribes to the systemd-events suspend, hibernate, lock-session,
  # and unlock-session with appropriate actions (run locker and wait for user to unlock or kill locker).
  # xss-lock also reacts to DPMS events and runs or kills the locker in response.
  programs.xss-lock = {
    enable = true;
    lockerCommand = "${pkgs.xlockmore}/bin/xlock -mode ant";
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
  # networking.hosts."35.244.244.204" = ["app.globalwebindex.com"];
  # networking.nameservers = ["1.1.1.1" "1.0.0.1"];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
