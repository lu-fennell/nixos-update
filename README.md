# nixos-update

Scripts to configure and update my nixos system and environment in a controlled
fashion.

## Features:

- uses channels from a git repo
- tracks installed version (of channel and .nixpkgs config)
- ...

## Requirements & Setup

- Required tools: git, mkdir, chmod, cp, rsync, nix-env-rebuild, nixos-rebuild
- NIX_PATH needs to be set to a git-checkout of the desired channel. Also a
  `nixpkgs=` entry should be in the path, set to the same channel. Example:

        NIX_PATH=/home/fennell/nixos-update/channels/nixos-16.09:nixpkgs=/home/fennell/nixos-update/channels/nixos-16.09:...
  The path can be set from the config with `environment.shellInit`
- Remove the nix-channels from `~/.nix-defexpr` (`sudo -i nix-channel --remove ...`)
- Directory layout (see also `parameters.rkt`):
    - `~/nixos-update` should be a clone of this repository
    - `~/nixos-update/channels` should contain checkouts of nixpkgs-channel branches (https://github.com/NixOS/nixpkgs-channels)
    - `~/.nixpkgs`. Has to be compatible with `nix-env-rebuild`. It should also
      contain a nixexprs `nix-env-installed-version.nix`:

````     
{ stdenv }: 
stdenv.mkDerivation {
  name = "nix-env-installed-version";
  version = "0.1";
  src = ./nix-env-installed-version;
  
  installPhase = ''
    mkdir -p $out/bin
    cp nix-env-installed-version $out/bin
  '';
}
````
     It has to be included in `packages.nix`:

````
with import <nixpkgs> {};
[ 
  nix-env-installed-version
  ....
````
    - `~/nixos-config`. Contains `.nix`-files with systems settings. They should
      be included in `configuration.nix`. It should include a `nixos-installed-version.nix` expression:

````
{ stdenv }: 
stdenv.mkDerivation {
  name = "nixos-installed-version";
  version = "0.1";
  src = ./.;
  
  installPhase = ''
    mkdir -p $out/bin
    cp nixos-installed-version $out/bin
  '';
}
````
     The expression should be included in `environment.systemPackages`.
