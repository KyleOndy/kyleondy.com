let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    clojure # the lang
    leiningen # build tooling
    clojure-lsp

    html-tidy # html -> tidy html
    pandoc # md -> html
    sass # scss -> css
    yq-go # like jq, but for json
    yuicompressor # css compression
  ];
}
