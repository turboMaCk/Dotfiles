# In ideal world every project would have it's own nix defintion
# this is hard to enforce so let's provide sane globals
# So work can be done

{ pkgs, ... }:

let
  elmTools = import (pkgs.fetchFromGitHub {
    owner = "turboMaCk";
    repo = "nix-elm-tools";
    rev = "e2014925d60867c1e8f07e4bd5cbaeec3a484fff";
    sha256 = "10v3h3mmxx20dn93nwsm86grd3qqzllsyf46m6bj6d8grxfil3x8";
  }) { inherit pkgs; };
in {
    environment.systemPackages = with pkgs.elmPackages; [
        elm
        elm-format
        pkgs.elm2nix
        elmTools.elm-test
        elmTools.elm-verify-examples
        elmTools.elm-analyse
        elmTools.elm-doc-preview
    ];
}
