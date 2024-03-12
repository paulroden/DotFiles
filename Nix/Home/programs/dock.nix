{ config, pkgs, }:
{
  enable = true;
  apps = [
    "${pkgs.kitty}/Applications/kitty.app/"
    "/System/Applications/Calendar.app/"
    "/System/Applications/Mail.app/"
    "/System/Applications/Music.app/"
    "/System/Applications/Maps.app/"
    "/System/Applications/Photos.app/"
    "/System/Volumes/Preboot/Cryptexes/App/System/Applications/Safari.app/"  # this one is super-special!
    "/Applications/NetNewsWire.app/"
    "/Applications/PDF Viewer.app/"
    "${pkgs.emacsGit}/Applications/Emacs.app/"
    "/Applications/iA Writer.app/"
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
