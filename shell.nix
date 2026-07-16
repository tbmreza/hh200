{ pkgs ? import <nixpkgs> {} }:

# ??: nix publish hh200
pkgs.mkShell {
  packages = [
    pkgs.nodejs
  ];
}
