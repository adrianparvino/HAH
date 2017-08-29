compiler: {
  packageOverrides = pkgs: rec {
    haskellPackages = pkgs.haskell.packages.${compiler}.override {
      overrides = self: super: {
        lens = self.callPackage ./pkgs/lens.nix {};
        vector = self.callPackage ./pkgs/vector.nix {};
        primitive = self.callPackage ./pkgs/primitive.nix {};
        constraints = self.callPackage ./pkgs/constraints.nix {};
      };
    };
  };
}
