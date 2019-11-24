{ config, pkgs, ... }:

let
  webcam-conf = pkgs.writeScriptBin "my-configure-webcam" ''
    # crappy creative webcam is over saturated
    v4l2-ctl -d /dev/video0 --set-ctrl=saturation=45
  '';
in
{
  environment.systemPackages = with pkgs; [
    obs-studio

    # webcam utilities
    v4l-utils
  ];

  # Configure WebCam
  systemd.user.services.webcam-conf = {
    description = "Overwrite webcam default settings";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${webcam-conf}/bin/my-configure-webcam";
      RemainAfterExit = true;
      StandardOutput="journal";
    };
  };
}
