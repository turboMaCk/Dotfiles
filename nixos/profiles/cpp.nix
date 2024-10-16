{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # build
    cmake
    # debugging
    gdb
    gf
    lldb
    valgrind
  ];
}
