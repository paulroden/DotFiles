{ lib }:
lib.mapAttrs (key: value: lib.mkDefault value)
{
  "Application Mode" = 2;  # 0 = dock icon; 1 = menu bar icon; 2 = faceless 

  "Allow For Drawers" = true;
  "Dismiss After Filling" = false;
  "Dismiss After Other" = false;

  "Grid Spacing: Gap" = 2;
  "Configuration Grid: Columns" = 8;
  "Configuration Grid: Rows" = 6;

  "Hide Keyboard Controls Logo" = true;

  "Key Control: Arrow: Control" = 52;
  "Key Control: Arrow: Option" = 53;
  "Key Control: Escape" = 0;
  "Key Control: Return" = 22;
  "Key Control: Tab" = 1;

  "Mouse Controls" = true;
  "Mouse Controls Delay" = 0.0;
  "Mouse Controls Grid" = true;
  "Mouse Controls Grid: Rows" = 2;
  "Mouse Controls Grid: Columns" = 3;
  "Mouse Controls Grid: Mode" = 3;
  "Mouse Controls Grid: Mode: Show Dialog" = false;
  "Mouse Controls Include Custom Controls: Show On Hover" = true;
  "Mouse Controls Include Custom Controls" = true;
  "Mouse Controls Trigger: Apple: Primary" = false;

  "Snap" = true;
  "Snap: 3" = 12;
  "Snap Back" = false;
  "Snap Back: Scope" = 1;
  "Snap Delay" = 0.05;
  "Snap: Ignore Bordering Edges And Corners" = false;

  "Show Cheat Sheet" = false;
  # TODO: capture this setting:
  # "Keyboard Controls" = {
    #   "Visual Representation" = "⌃⇧W";
    #   "Identifier" = "Keyboard Controls";
    #   "Key Code" = "13"; # W => 0x0D => 13
    #   "Modifier Flags" = "393475";
    # };

    # TODO: capture custom window sizes & arrangments (see output of `defaults read com.manytricks.Moom`)
}
