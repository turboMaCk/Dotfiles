# In ideal world every project would have it's own nix defintion
# this is hard to enforce so let's provide sane globals
# So work can be done

{ pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        nodejs-12_x
        yarn
        nodePackages.node2nix
        # webpack-imagemin-plugin
        pkgconfig autoconf automake libtool nasm autogen zlib libpng
    ];
}
