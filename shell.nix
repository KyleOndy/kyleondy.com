{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "kyleondy.com";

  buildInputs = [
    ruby
    jre
  ];

  shellHook = ''
    export PATH="$HOME/.gem/ruby/2.6.0/bin:$PATH"
    export AWS_ACCESS_KEY_ID="AKIAW4WHQVVUXSIPY4YK"
    export AWS_SECRET_ACCESS_KEY="$(pass show aws.amazon.com/keys/AKIAW4WHQVVUXSIPY4YK)"
  '';
}
