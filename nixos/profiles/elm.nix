# In ideal world every project would have it's own nix defintion
# this is hard to enforce so let's provide sane globals
# So work can be done

{ pkgs, ... }:

let
  elmTools = import (pkgs.fetchFromGitHub {
    owner = "turboMaCk";
    repo = "nix-elm-tools";
    rev = "49b79886a43f816f53f3325dba05c40f28b5233d";
    sha256 = "03j352q3s8d4x79570vgiwc4sjlyj5vi0nnvi15z4x0haga3410r";
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
