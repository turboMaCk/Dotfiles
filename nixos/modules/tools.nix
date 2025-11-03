{ config, pkgs, ... }:

let
  nix-prune-roots = pkgs.writeScriptBin "nix-prune-roots" ''
    #!/usr/bin/env bash

    set -e
    set -o nounset
    set -o pipefail

    echo "Removing all the GC roots located within HOME."
    nix-store --gc --print-roots | grep ~ | cut -d" " -f1 | xargs -0 rm
    echo "Done! Run `nix-collect-garbage -d` to gain more disk space."
  '';
in
{
  environment.systemPackages = with pkgs; [
    # Shell base utils
    which lsof wget vim-full dig tree unzip killall libnotify jq file openssl

    # Shell fancier improvements
    tmux starship htop cloc direnv cachix pass

    # Git
    git tig

    # C compiling
    gcc gnumake

    # Nix utils
    nix-prefetch-scripts nix-prune-roots nixpkgs-review

    # Scripting
    python3
  ];
}
