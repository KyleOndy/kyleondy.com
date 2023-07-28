{
  description = "Source for kyleondy.com";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.hakyll-flakes.url = "github:Radvendii/hakyll-flakes";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, hakyll-flakes, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (
      system:
      hakyll-flakes.lib.mkAllOutputs {
        inherit system;
        name = "kyleondy-com";
        src = ./src;
        websiteBuildInputs = with nixpkgs.legacyPackages.${system}; [
        ];
      }
    );
}

