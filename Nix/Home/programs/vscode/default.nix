{ pkgs }:
{
  enable = true;
  package = pkgs.vscodium;
  userSettings = builtins.fromJSON
    (builtins.readFile ./settings.json);
  keybindings = [
    {
      key = "alt+down";
      command = "cursorColumnSelectDown";
      when = "textInputFocus";
    }
    {
      key = "shift+alt+cmd+down";
      command = "-cursorColumnSelectDown";
      when = "textInputFocus";
    }
    {
      key = "alt+up";
      command = "cursorColumnSelectUp";
      when = "textInputFocus";
    }
    {
      key = "shift+alt+cmd+up";
      command = "-cursorColumnSelectUp";
      when = "textInputFocus";
    }
    {
      key = "cmd+;";
      command = "editor.action.quickFix";
      when = "editorTextFocus";
    }
  ];
  mutableExtensionsDir = false;
  extensions = with pkgs.vscode-extensions; [
    arcticicestudio.nord-visual-studio-code
    arrterian.nix-env-selector
#    haskell.haskell
    jnoortheen.nix-ide
    #slbtty.lisp-syntax
    ms-pyright.pyright
    ms-python.python
    rust-lang.rust-analyzer
    streetsidesoftware.code-spell-checker
  ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    {
      name = "agda-mode";
      publisher = "banacorn";
      version = "0.3.11";
      sha256 = "113w2iis4zi4z3sqc3vd2apyrh52hbh2gvmxjr5yvjpmrsksclbd";
    }
    {
      name = "chalice-color-theme";
      publisher = "artlaman";
      version = "1.0.2";
      sha256 = "sha256-tTKgDMttb871XxgvmUg3jIB8HqH9mXAh1AhOsbXjPQ0";
    }
    {
      name = "chalice-icon-theme";
      publisher = "artlaman";
      version = "1.2.21";
      sha256 = "sha256-nCwTMAq83JPNSil5AaVueu5N2YxcoBl1GYxERO4u3Qo=";
    }
    {
      name = "haskell";
      publisher = "haskell";
      version = "2.5.0";
      sha256 = "sha256-WolhQ+D1rIcvoY1+1coEo0pnORv0+RA8Vti1P+vg2LY=";
    }
    {
      name = "theme-alabaster";
      publisher = "tonsky";
      version = "0.2.9";
      sha256 = "sha256-3LvXIJAyKUqgxAsC7fa48YRqX3/5UByMhYCQxnuKJm4";
    }
    {
      name = "tokyo-city";
      publisher = "huytd";
      version = "0.2.4";
      sha256 = "sha256-Jb2zIXdU/UE4OFsnOVdsgMgJZMLkxa43LKkQQNNjbAY";
    }
  ];
}
