{ pkgs, config, ... }:
{
  enable = true;
  # envFile = env_nu;
  configFile.source = ./config.nu ;
  envFile.source = ./env.nu;
}
