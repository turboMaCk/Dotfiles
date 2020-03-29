self: super:
let
  packages = with self.rPackages; [
    pacman
    dplyr
    GGally
    ggplot2
    ggthemes
    ggvis
    httr
    lubridate
    plotly
    rio
    rmarkdown
    shiny
    stringr
    tidyr
  ];
in {
  rEnv = super.rWrapper.override {
    inherit packages;
  };
  rStudioEnv = super.rstudioWrapper.override {
    inherit packages;
  };
}
