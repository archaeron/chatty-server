let pkgs = (import <nixpkgs> {});
  haskellPackages = pkgs.recurseIntoAttrs (pkgs.haskellPackages.override {
    extension = self : super :
    let callPackage = self.callPackage;
    in {
    # (1)
      groundhog = callPackage ./nix/groundhog.nix {};
      groundhogSqlite = callPackage ./nix/groundhog_sqlite.nix {};
      groundhogTh = callPackage ./nix/groundhog_th.nix {};
      thisPackage = haskellPackages.callPackage (import ./default.nix) {};
    };
  }
);
in pkgs.lib.overrideDerivation haskellPackages.thisPackage (old: {
  }
)
