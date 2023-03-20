# MacOS Dock Configuration, Declaratively
# - original credit @dustinlyons (https://github.com/dustinlyons/nixos-config/tree/e4e1ff1773328102b0f1f906e522bcdca3a5d12a/darwin/dock)
# - uses dockutil <https://github.com/kcrawford/dockutil>
{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.local.dock;
  stdenv = pkgs.stdenv;
  dockutil = pkgs.dockutil;
in
{
  options = {
    local.dock.enable = lib.mkOption {
      description = "Enable Dock";
      default = stdenv.isDarwin;
      example = false;
    };

    local.dock.apps = lib.mkOption {
      description = "Applications included in the Dock";
      type = types.listOf types.str;
      default = [ ];
      example = [
        "/System/Applications/Mail.app/"
        "/System/Volumes/Preboot/Cryptexes/App/System/Applications/Safari.app/"
        "${pkgs.kitty}/Applications/kitty.app/"
      ];
    };
    local.dock.others = mkOption {
      description = "Other items in the Dock, after the separator (not Applications)";
      type = types.listOf (types.submodule {
        options = {
          path = lib.mkOption {
            type = types.str;
          };
          display = lib.mkOption {
            type = types.str;
            default = "folder";
            description = "Display as folder or stack.";
          };
          sort = lib.mkOption {
            type = types.str;
            default = "name";
            description = "Sort list by: name | dateadded | datemodified | datecreated | kind .";
          };
          view = lib.mkOption {
            type = types.str;
            default = "auto";
            description = "View contents as: grid | fan | list | auto .";
          };   
        };
      });
      readOnly = true;
    };
  };

  config =
    mkIf (cfg.enable) (
      let
        normalize = path: if hasSuffix "/" path then path else path + "/" ;
        
        normalisedURI = path: "file://" + (builtins.replaceStrings
          [" "   "!"   "\""  "#"   "$"   "%"   "&"   "'"   "("   ")"]
          ["%20" "%21" "%22" "%23" "%24" "%25" "%26" "%27" "%28" "%29"]
          (normalize path)
        );
        
        buildScriptAppItems = concatMapStrings
          (item: concatStringsSep " " [
            "${dockutil}/bin/dockutil --no-restart"
            "--add '${item}'"
            "--section apps"
            "&>/dev/null \n"    # TODO: respect VERBOSE flag: disable this if VERBOSE
          ])
          cfg.apps;
        
        buildScriptOtherItems = concatMapStrings
          (item: concatStringsSep " " [
            "${dockutil}/bin/dockutil --no-restart"
            "--add '${item.path}'"
            "--display '${item.display}'"
            "--sort '${item.sort}'"
            "--view '${item.view}'"
            "--section others"
            "&>/dev/null \n"    # TODO: respect VERBOSE flag: disable this if VERBOSE
          ])
          cfg.others;
        
        declaredURIs = concatStringsSep "" [
          (concatMapStrings (item: "${normalisedURI item}\n") cfg.apps)
          (concatMapStrings (item: "${normalisedURI item.path}\n") cfg.others)
        ];
        
        buildScriptAllItems = concatStringsSep "\n" [
          buildScriptAppItems
          buildScriptOtherItems
        ];
        
      in
        {
          # TODO: echo added/removed items in Dock (echo all changes if VERBOSE flag is set)
          home.activation = {
            buildDock = lib.hm.dag.entryAfter ["writeBoundary"] ''
              currentURIs="$(${dockutil}/bin/dockutil --list | ${pkgs.coreutils}/bin/cut -f2)"
              if ! diff -wu \
                <(echo -n "$currentURIs") \
                <(echo -n '${declaredURIs}') \
                &>/dev/null ; then
                ${dockutil}/bin/dockutil --no-restart --remove all
                ${buildScriptAllItems}            
                # only to make dockutil to restart the Dock (instead of killall)
                ${dockutil}/bin/dockutil --remove spacer-tiles --restart
              else
                echo "Dock items unchanged."
              fi
            '';
          };
        }
    );
}
