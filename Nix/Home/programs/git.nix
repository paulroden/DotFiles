{ config }:
{
  enable = true;
  userName = "Paul Roden Henderson";
  userEmail = "git@paulroden.net";
  aliases = {
    amend = ''commit --amend --no-edit'';
    lg1   = ''log --graph --decorate --all --oneline'';
    lg2   = ''log --graph
                  --abbrev-commit
                  --pretty=format:'%Cred%h%Creset :%C(yellow)%d%Creset %s %C(dim white)(%cr)%Creset %C(brightyellow)by %C(italic)%an%Creset' '';
  };
  extraConfig = {
    init.defaultBranch = "main";
  };
  delta = {
    enable = true;
    options = {
      interactive = {
        keep-plus-minus-markers = false;
      };
      decorations = {
        commit-decoration-style = "blue ol";
        commit-style = "raw";
        file-style = "omit";
        hunk-header-decoration-style = "blue box";
        hunk-header-file-style = "red";
        hunk-header-line-number-style = "#067a00";
        hunk-header-style = "file line-number syntax";
      };
    };
  };
}
