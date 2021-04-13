let
  # Read fixed haskell.nix version
  sources = {
    haskellNix = builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/e1a7bab50735b19c8ba1db4e4b0db672a825d7cd.tar.gz";
  };
  # Supply sha256 of externally fetched packages
  haskellNix = import sources.haskellNix {
    sha256map =
    { "https://github.com/WorldSEnder/async-pool.git"."0.9.1"
      = "0zsq457l3zdvhbxfm06wdk3ckak9gpjsdsg9xkdzh9hig03hff36";
      "https://github.com/basvandijk/monad-control.git"."1.0.2.3"
      = "1andqbhxhmjhndpigda5kb73mqdqnfnzskz9y1gvcjqnxd4sgfyi";
      "https://github.com/def-/ghc-vis.git"."0.9.2"
      = "0a5y7wy6l2kl368c234m8pz28w004qlzr8cnx1sw1v43x2nfjn77";
    };
  };
  overlay = self: _: {
    hsPkgs = self.haskell-nix.project {
      # 'cleanGit' cleans a source directory based on the files known by git
      src = self.haskell-nix.haskellLib.cleanGit {
        name = "grubber";
        src = ./.;
      };
      stack-sha256 = "0fpj5ng1hmn05jxaq7136rq8983rzs8xr19avws2m4zslrm56vjz";
      materialized = ./grubber.materialized;
    };
  };

  pkgArgs = haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays ++ [ overlay ];
  };
  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
in import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    haskellNix.sources.nixpkgs-2009
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    pkgArgs
