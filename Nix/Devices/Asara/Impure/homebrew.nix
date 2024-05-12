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
      "MonitorControl"
      "acorn"
      "apparency"
      "discord"
      "focusrite-control-2"
      "jquake"
      "meetingbar"
      "moom"
      "netnewswire"
      "nova"
      "orbstack"
      "racket"  # cask version includes Dr. Racket for Mac
      "raycast"
      "suspicious-package"
      "syntax-highlight"
      "zed"
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
      "Collections" = 1568395334;
      "Compressor" = 424390742;
      "Mainstage" = 634159523;
      "Final Cut Pro" = 424389933;
      "Ivory" = 6444602274;
      "Logic Pro" = 634148309;
      "Motion" = 434290957;
      "Pastel" = 413897608;
      "PDF Viewer" = 1120099014;
      "iA Writer" = 775737590;
      "reMarkable" = 1276493162;      
    };
  };
}
