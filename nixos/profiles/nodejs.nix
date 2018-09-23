# In ideal world every project would have it's own nix defintion
# this is hard to enforce so let's provide sane globals
# So work can be done

{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        nodejs-8_x
        yarn
    ];
}
