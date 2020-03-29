{ config, pkgs, ... }:
{
  # TODO: add python related stuff
  # https://github.com/turboMaCk/jupyjaji
  environment.systemPackages = with pkgs; [
    # GNU R
    rEnv
    rStudioEnv
  ];
}
