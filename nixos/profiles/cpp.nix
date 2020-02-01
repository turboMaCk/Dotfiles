{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # build
    cmake
    # debugging
    gdb
    lldb
    codeblocksFull
    valgrind
  ];
}
