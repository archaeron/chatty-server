{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

with haskellPackages; cabal.mkDerivation (self: {
  pname = "Chatty";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
  aeson groundhog groundhogSqlite groundhogTh monadControl
  monadLogger resourcePool servant servantServer text transformers
  wai warp
  ];
  meta = {
    homepage = "https://github.com/archaeron/Chatty";
    description = "A chat app";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
  buildTools = [ cabalInstall ];
})
