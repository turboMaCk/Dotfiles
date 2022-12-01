# In ideal world every project would have it's own nix defintion
# this is hard to enforce so let's provide sane globals
# So work can be done

{ pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        nodejs-16_x
        yarn
        nodePackages.node2nix
        # webpack-imagemin-plugin
        pkg-config autoconf automake libtool nasm autogen zlib libpng
    ];
    # Fix openssl issue in nodejs
    # see: https://stackoverflow.com/questions/69692842/error-message-error0308010cdigital-envelope-routinesunsupported
    # and: https://stackoverflow.com/questions/69962209/what-is-openssl-legacy-provider-in-node-js-v17
}
