{
  enable = true;
  themes = {
    ayu-dark =
      builtins.readFile ./ayu-dark.tmTheme;
  };
  config = {
    theme = "ayu-dark";
  };
}
