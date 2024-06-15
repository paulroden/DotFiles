{ config, pkgs, ... }:
{
  enable = true;
  apps = [
    "${pkgs.kitty}/Applications/kitty.app/"
    "/System/Applications/Calendar.app/"
    "/System/Applications/Mail.app/"
    "${config.home.homeDirectory}/Applications/HEY.app"
    "/System/Applications/Music.app/"
    "/System/Volumes/Preboot/Cryptexes/App/System/Applications/Safari.app/"  # this one is super-special!
    "/Applications/NetNewsWire.app/"
    "${pkgs.emacs'}/Applications/Emacs.app/"
    "/Applications/Zed.app/"
    "/Applications/iA Writer.app/"
    "/Applications/Soulver 3.app/"
    "/Applications/PDF Viewer.app/"
    "/Applications/ChatGPT.app/"
  ];
  others = [
    {
      path = "/Applications/";
      sort = "name";
      view = "list";
      display = "folder";
    }
    {
      path = "${config.home.homeDirectory}";
      sort = "name";
      view = "list";
      display = "folder";
    }
    {
      path = "${config.home.homeDirectory}/Library/Mobile Documents/com~apple~CloudDocs/Docs";
      sort = "name";
      view = "list";
      display = "folder";
    }
    {
      path = "${config.home.homeDirectory}/Downloads";
      sort = "dateadded";
      view = "grid";
      display = "stack";
    }
  ];
}
