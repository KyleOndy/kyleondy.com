{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "commands-nix";

  buildInputs = [
    pandoc # markdown -> html
  ];

  shellHook = ''
    export foo="bar"
  '';
}
