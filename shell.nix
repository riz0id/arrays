{ ghc ? "ghc924" }:

let 
  pkgs = import ./default.nix { 
    inherit ghc; 
  };
in pkgs.arrays.env.overrideAttrs (self: {
  buildInputs = self.buildInputs ++ (with pkgs; [ 
    hlint
    stylish-haskell
  ]);
})