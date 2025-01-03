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
      "chatgpt"
      "crystalfetch"
      "discord"
      "fantastical"
      "focusrite-control-2"
      "google-chrome"
      "iina"
      "jordanbaird-ice"
      "jquake"
      "kaleidoscope"
      "logseq"
      "meetingbar"
      "moom"
      "netnewswire"
      "nova"
      "obsidian"
      "orbstack"
      "qgis"
      "racket"  # cask version includes Dr. Racket for Mac
      "raycast"
      "soulver"
      "soulver-cli"
      "suspicious-package"
      "syntax-highlight"
      "zed"
      # fonts from `homebrew-cask-fonts`
      "font-cormorant"
      "font-eb-garamond"
      "font-et-book"
      "font-ia-writer-mono"
      "font-ia-writer-duo"
      "font-ia-writer-quattro"
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
      "Bumpr" = 1166066070;
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
      "Tailscale" = 1475387142;
    };
  };
}
