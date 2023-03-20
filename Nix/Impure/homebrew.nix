{...}:
{
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "zap";
      upgrade = true;
    };
    taps = [
      "homebrew/cask-fonts"
    ];
    brews = [
      
    ];
    casks = [
      "acorn"
      "apparency"
      "discord"
      "nova"
      "raycast"
      "remarkable"
      "suspicious-package"
      "syntax-highlight"
      # fonts from `homebrew-cask-fonts`
      "font-cormorant"
      "font-eb-garamond"
      "font-et-book"
      "font-kaisei-decol"
      "font-kaisei-harunoumi"
      "font-kaisei-opti"
      "font-kaisei-tokumin"
      "font-murecho"
      "font-open-iconic"
      "font-playfair-display"
      "font-playfair-display-sc"
      "font-poly"
      "font-poppins"
      "font-sora"
      "font-sorts-mill-goudy"
      "font-spectral"
      "font-ysabeau"
    ];
  };
}
