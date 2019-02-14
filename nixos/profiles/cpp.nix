{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; with pkgs.xorg; [
    cmake

    # X11
    libX11.dev
    libXi.dev
    libXrandr.dev
    libXext.dev
    libXcursor.dev
    libXinerama.dev
    libXi.dev

    # OpenGL
    libGLU
    mesa_glu.dev
  ];
}
