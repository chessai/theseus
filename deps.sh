nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ parsec transformers ])"
