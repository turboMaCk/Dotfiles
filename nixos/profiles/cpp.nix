{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # build
    cmake
    # debugging
    gdb
    gf
    valgrind
    clang-tools
    jetbrains.clion
  ];
}
