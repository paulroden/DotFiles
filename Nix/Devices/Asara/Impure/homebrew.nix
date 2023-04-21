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
      "moom"
      "netnewswire"
      "nova"
      "raycast"
      "remarkable"
      "suspicious-package"
      "syntax-highlight"
      "MonitorControl"
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
    # Mac App Store Apps (via https://github.com/mas-cli/mas)
    masApps = {
      "Chambers Dictionary" = 500583211;
      "Chambers Thesaurus" = 498829958;
      "Dictionaries" = 1380563956;
      "PDF Viewer" = 1120099014;
      "Pastel" = 413897608;
    };
  };
}
