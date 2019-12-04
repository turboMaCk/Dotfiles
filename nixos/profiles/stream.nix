{ config, pkgs, ... }:

let
  webcam-conf = pkgs.writeScriptBin "my-configure-webcam" ''
    # Main webcam Logitech C920 pro
    v4l2-ctl -d /dev/video0 --set-ctrl=exposure_auto=3
    v4l2-ctl -d /dev/video0 --set-ctrl=focus_auto=0
    v4l2-ctl -d /dev/video0 --set-ctrl=saturation=100

    # crappy creative live! webcam is over saturated
    v4l2-ctl -d /dev/video2 --set-ctrl=saturation=45
  '';
in
{
  environment.systemPackages = with pkgs; [
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
