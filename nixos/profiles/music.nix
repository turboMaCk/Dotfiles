{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    renoise
    qjackctl
    jack2
    ardour

    # DSP plugins
    carla
    calf
    tap-plugins
    caps
    zynaddsubfx
    wine
    helm
    magnetophonDSP.CharacterCompressor
    magnetophonDSP.CompBus
    magnetophonDSP.ConstantDetuneChorus
    # magnetophonDSP.MBdistortion
    magnetophonDSP.LazyLimiter
    magnetophonDSP.RhythmDelay
    magnetophonDSP.VoiceOfFaust
    magnetophonDSP.faustCompressors
    magnetophonDSP.shelfMultiBand
  ];

  services.pipewire.jack.enable = true;

  environment.variables = {
    DSSI_PATH   = "$HOME/.dssi:$HOME/.nix-profile/lib/dssi:/run/current-system/sw/lib/dssi";
    LADSPA_PATH = "$HOME/.ladspa:$HOME/.nix-profile/lib/ladspa:/run/current-system/sw/lib/ladspa";
    LV2_PATH    = "$HOME/.lv2:$HOME/.nix-profile/lib/lv2:/run/current-system/sw/lib/lv2";
    LXVST_PATH  = "$HOME/.lxvst:$HOME/.nix-profile/lib/lxvst:/run/current-system/sw/lib/lxvst";
    VST_PATH    = "$HOME/.vst:$HOME/.nix-profile/lib/vst:/run/current-system/sw/lib/vst";
    VST3_PATH   = "$HOME/.vst3:$HOME/.nix-profile/lib/vst3:/run/current-system/sw/lib/vst3";
  };
}
