{ ghc }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".extend (self: _: {
        array-exceptions = self.callPackage ../pkgs/array-exceptions.nix { };
      });
    };
  };
}