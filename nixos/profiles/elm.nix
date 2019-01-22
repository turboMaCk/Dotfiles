# In ideal world every project would have it's own nix defintion
# this is hard to enforce so let's provide sane globals
# So work can be done

{ config, pkgs, ... }:

let verify-examples = pkgs.callPackage ../pkgs/elm-verify-examples.nix {};
in {
    environment.systemPackages = with pkgs.elmPackages; [
        elm
        elm-format
        pkgs.nodePackages.node2nix
        pkgs.elm2nix
        # verify-examples
    ];
}
