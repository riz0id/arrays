{ ghc }:

import (import ./nixpkgs.nix) {
  config.packageOverrides = pkgs: with pkgs.lib;
    let 
      importExtension = filepath: import filepath { inherit ghc; };
      extensions = lists.forEach (filesystem.listFilesRecursive ./exts) importExtension;
    in composeManyExtensions extensions pkgs pkgs;
}