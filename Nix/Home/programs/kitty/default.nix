{ config }:
{
  enable = true;
  font = {
    name = "JuliaMono";
    size = 14;
  };
  themeFile = "Alabaster_Dark";
  settings = {
    shell = "${config.home.profileDirectory}/bin/fish";
    scrollback_lines = 10000;
    enable_audio_bell = false;
    update_check_interval = 0;
    window_padding_width = 10;
    initial_window_width = "640";  # c => 'cells', rather than pixels
    initial_window_height = "360";
    cursor_shape = "beam";
    tab_bar_edge = "top";
    tab_title_max_length = 20;
    tab_activity_symbol = "⋯";
    tab_bar_margin_height = "0.0 0.0";
    tab_bar_style = "fade";
    tab_fade = "0.2 0.8 1 1";
    tab_bar_background = "#3b3b3b";
    tab_bar_margin_width = 12;
    active_tab_font_style = "bold";
    inactive_tab_font_style = "light";
    editor = "vim";
    paste_actions = "quote-urls-at-prompt";
    inactive_text_alpha = "0.67";
    confirm_os_window_close = -1;
    macos_titlebar_color = "dark";
    macos_colorspace = "displayp3";
    macos_option_as_alt = "yes";
  };
  keybindings = {
    "ctrl+t" = "new_tab_with_cwd";
    "cmd+t" = "new_tab_with_cwd";  #🍎
    "ctrl+shift+enter" = "new_window_with_cwd";
    "cmd+enter" = "new_window_with_cwd";  #🍎
    "cmd+f" = "launch --location=hsplit --allow-remote-control kitty +kitten search.py @active-kitty-window-id";
  };
  darwinLaunchOptions = [
    "--single-instance"
    "--directory=$HOME"
  ];
}
