{
  enable = true;
  settings = {
    shell = {
      fish_indicator = "􁖐︎ ";
      zsh_indicator = "􀀶 ";
      powershell_indicator = "_ ";
      unknown_indicator = "¿sh? ";
      style = "fg:bold yellow";
      disabled = false;
    };
    hostname = {
      ssh_symbol = "🤫 ";
    };
    character = {
      success_symbol = "[⊢](bold green)";
      error_symbol = "[⊥](bold red)";
    };
    add_newline = false;
    jobs = {
      symbol = "& ";
      format = "[$number$symbol]($style)";
      style = "bold yellow";
      symbol_threshold = 1;
      number_threshold = 1;
    };
    # starship should never make us *feel* like we're waiting for a prompt
    # https://www.nngroup.com/articles/response-times-3-important-limits/
    # 1/10th of a second is the perceptual response limit time, let's use that
    command_timeout = 100;
    right_format = "";
    package = {
      disabled = false;
    };
  };
}
