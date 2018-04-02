{ package ? "theseus", compiler ? "ghc822" }:
let fetchNixpkgs = import ./nix/fetchNixpkgs.nix;
    nixpkgs = fetchNixpkgs {
      rev = "7513208cd33e87eb1d66e4663232a3242e6818dd"; 
      sha256 = "0hh50spx97nnzcj4ph40prkalwmb6ayffg7gyvzrfhia1c9pb7hb"; 
      sha256unpacked = "0cbdy2rn6a9vvpcajfkix1czcg12b8xv0902sjhlzgvr73y49r1c"; 
    };
    pkgs = import nixpkgs { config = {}; overlays = []; };
    inherit (pkgs) haskell;
 
  filterPredicate = p: type:
    let path = baseNameOf p; in !(
       (type == "directory" && path == "dist")
    || (type == "symlink"   && path == "result")
    || (type == "directory" && path == ".git")
    || (type == "symlink"   && pkgs.lib.hasPrefix "result" path)
    || pkgs.lib.hasSuffix "~" path
    || pkgs.lib.hasSuffix ".o" path
    || pkgs.lib.hasSuffix ".so" path
    || pkgs.lib.hasSuffix ".nix" path);
    
  overrides = haskell.packages.${compiler}.override {
    overrides = self: super:
    with haskell.lib;
    with { cp = file: (self.callPackage (./nix/haskell + "/${file}.nix") {}); 
           build = name: path: self.callCabal2nix name (builtins.filterSource filterPredicate path) {}; 
         };
    {
      parsix  = dontCheck (cp "parsix");
      theseus = cp "theseus";
    };
  };
in rec {
  drv = overrides.${package};
  theseus = if pkgs.lib.inNixShell then drv.env else drv;
}
