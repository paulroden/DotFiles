# ∙files

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

Configurations of my system (or systems, depending on where one draws boundaries), managed almost entirely with [Nix](https://nixos.org).

This setup is evolving fast, due in part to [Darwin](https://github.com/LnL7/nix-darwin/). This page will be updated with finer details as the configuration settles.

## Useful commands
To be run from the root of this directory, unless otherwise stated.

##### Rebuild Home Configuration

``` shell
home-manager switch --flake .#paul
```
(optionally add `--show-trace` for crazy error messages, or --dry-run to have a preview of results)


##### Rebuild Machine Configuration
e.g. _Âsâra_, my favourite MacBook Pro (sans diacritics in Nix files...)

``` shell
nix build .#darwinConfigurations.Asara.system
```


##### Manual Installation of Nix Darwin
The [manual installation process](https://github.com/LnL7/nix-darwin/#manual-install) is worth knowing about to understand the changes that `nix-darwin` makes to a MacOS system does.

These are the steps followed here (on MacOS Ventura 13.2).

 + Note 1: the `configuration` parameter in `nix-darwin`'s `default.nix` should be swapped out for whichever configuration(`.nix`) file is being used for the device (e.g. `Asara.nix`).
 + Note 2: move any existing `/etc/nix/nix.conf` file and backup if need be.

``` shell
# add `darwin` to the nix channels & update
nix-channel --add https://github.com/LnL7/nix-darwin/archive/master.tar.gz darwin
nix-channel --update

# declare `run` synthetic object on root volume
echo -e "run\tprivate/var/run" | sudo tee -a /etc/synthetic.conf

# 'stitch and create' the synthetic object
/System/Library/Filesystems/apfs.fs/Contents/Resources/apfs.util -t

# bootstrap nix-darwin from it's `default.nix` (see note above)
nix-build -A system --no-out-link

# use the link returned to run the build, then switch
sudo <result-link>sw/bin/darwin-rebuild build -I darwin-config=../Devices/Asara.nix
sudo <result-link>sw/bin/darwin-rebuild switch -I darwin-config=../Devices/Asara.nix
```

or, the last two commands can be done using the flake in at the root of this directory, like so:

``` shell
darwin-rebuild build  --flake .#
darwin-rebuild switch --flake .#
```
...which is probably preferable, since this is aiming to be a flake-first setup. 

... we want to get the useful stuff out of the old `nix.conf`:

``` apacheconf
build-users-group = nixbld
trusted-users = root paul
# builders -- for details, see:
#   https://nixos.org/manual/nix/stable/advanced-topics/distributed-builds.html
builders = ssh://nemo aarch64-linux,x86_64-linux,wasm32-wasi ~/.ssh/id_ed25519 8 2 ;
```

