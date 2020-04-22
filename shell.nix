{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "kyleondy.com";

  buildInputs = [
    pandoc
  ];

  shellHook = ''
    export AWS_ACCESS_KEY_ID="AKIAW4WHQVVUXSIPY4YK"
    export AWS_SECRET_ACCESS_KEY="$(pass show aws.amazon.com/keys/AKIAW4WHQVVUXSIPY4YK)"
  '';
}
