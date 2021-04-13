let
  pkgs = import ./pkgs.nix;
  hsPkgs = pkgs.hsPkgs;
in
hsPkgs.shellFor {
  withHoogle = true;
  tools = {
    cabal = "latest";
    ghcid = "latest";
    haskell-language-server = "latest";
    hlint = "latest";
  };
  # buildInputs = [ ormolu-wrapped ];
  exactDeps = true;
}
