{ lib, config, ... }:
{
  enable = true;
  # envFile = env_nu;
  configFile = ./config.nu ;
  # envFile.source = env_nu;
  # configFile.source = config_nu;
}
