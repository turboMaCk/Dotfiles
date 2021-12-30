{ config, pkgs, ... }:

let
  /**
  figma-app = with pkgs; writeScriptBin "figma" ''
    #!/usr/bin/env bash

    ${brave}/bin/brave --app=https://figma.com
  '';
*/
in {
  environment.systemPackages = with pkgs; [
    inkscape
    gimp
    # figma-app
  ];
}
