{ config, pkgs, ... }:
let
  custom-home = "~/Holmusk-Home";

  dropbox = pkgs.writeScriptBin "dropbox-holmusk" ''
    #!/usr/bin/env bash
    set -e

    # create home directory if it doesn't exist
    mkdir -p ${custom-home}

    # start dropbox for Holmusk team
    HOME=${custom-home} ${pkgs.dropbox}/bin/dropbox
  '';

  sync-rwe-assets = pkgs.writeScriptBin "sync-rwe-assets" ''
    #!/usr/bin/env bash
    set -e

    if [[ ! -f package.json ]]; then
      echo "This command must be ran from frontend project directory!"
      exit 1
    fi

    PROJECT_NAME=$(${pkgs.jq}/bin/jq '.name' package.json | sed 's/"//g')

    if [[ $PROJECT_NAME != "pi-frontend" ]]; then
      echo "This is not RWE project!"
      exit 1
    fi

    cp -r public/assets/theme ${custom-home}/Dropbox\ \(Holmusk\)/RWE\ Design/Assets\ -\ Web
  '';
in {
  environment.systemPackages = [
    dropbox
    sync-rwe-assets
    pkgs.awscli
    pkgs.mattermost

    # yubikey
    pkgs.yubikey-manager
    pkgs.yubikey-manager-qt
    pkgs.yubioath-desktop
    # pkgs.yubico-pam
    # pkgs.yubico-piv-tool
  ];

  # Yubikey hw support
  hardware.u2f.enable = true;
}
