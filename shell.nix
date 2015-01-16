let pkgs = (import <nixpkgs> {});
  haskellPackages = pkgs.recurseIntoAttrs (pkgs.haskellPackages.override {
  extension = self : super :
  let callPackage = self.callPackage;
  in {
    # (1)
    prettyCompact = callPackage ./nix/pretty-compact.nix {};
      thisPackage = haskellPackages.callPackage (import ./default.nix) {};
  };});
in pkgs.lib.overrideDerivation haskellPackages.thisPackage (old: {
  buildInputs = old.buildInputs ++ [
    haskellPackages.cabalInstall
  # (2)
  ];})
