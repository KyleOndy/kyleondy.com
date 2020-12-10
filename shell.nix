{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "kyleondy_com";

  buildInputs = [
    html-tidy # html -> tidy html
    pandoc # md -> html
    sass # scss -> css
    yuicompressor # css compression
  ];

  shellHook = ''
  '';
}
