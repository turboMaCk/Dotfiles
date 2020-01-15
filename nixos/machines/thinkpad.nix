# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ <nixos-hardware/lenovo/thinkpad/t480s>
      ../profiles/base.nix
      ../profiles/desktop.nix
      ../users/marek.nix
      ../profiles/virtualization.nix
      ../profiles/elm.nix
      ../profiles/nodejs.nix
      ../profiles/haskell.nix
      ../profiles/purescript.nix
      ../profiles/rust.nix
      ../profiles/ocaml.nix
      ../profiles/direnv.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

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
    networkmanager = {
      enable = true;
      enableStrongSwan = true;
      extraConfig = ''
        [main]
        rc-manager=resolvconf
      '';
    };
  };

  services.strongswan = {
    enable = true;
    secrets = [
      # see https://github.com/NixOS/nixpkgs/issues/64965
      "ipsec.d/ipsec.nm-l2tp.secrets"
    ];
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
    xl2tpd
    strongswan
  ];

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

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

  # Enable sound.
  sound.enable = true;

  hardware = {
    pulseaudio = {
      enable = true;
      # required for bluetooth package
      package = pkgs.pulseaudioFull;
      extraModules = [ pkgs.pulseaudio-modules-bt ];
    };

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
    opengl.extraPackages = [ pkgs.vaapiIntel ];
  };

  services.xserver = {
    # X11 XKB key map
    # Mainly Caps -> Ctrl
    # check https://www.x.org/releases/X11R7.6/doc/xorg-docs/input/XKB-Config.html
    xkbOptions = "ctrl:nocaps,caps:none,lv3:rwin_switch,grp:alt_space_toggle,altwin:swap_alt_win";

    # screen locking
    xautolock = {
      enable = true;
      locker = "${pkgs.xlockmore}/bin/xlock -mode ant";
      extraOptions = [ "-detectsleep" ];
      killer = "${pkgs.systemd}/bin/systemctl suspend";
      killtime = 10; # 10 is minimal value
      time = 5;
    };

    # Enable touchpad support.
    libinput = {
      enable = true;
      scrollMethod = "twofinger";
      naturalScrolling = true;
      disableWhileTyping = true;
      clickMethod = "clickfinger";
    };
  };

  services.logind.lidSwitch = "suspend";

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
