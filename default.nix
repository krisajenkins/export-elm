{ cabal, cabalInstall, text }:

cabal.mkDerivation (self: {
  pname = "elm-export";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  buildTools = [ cabalInstall ];
  buildDepends = [text];
  meta = {
    platforms = self.ghc.meta.platforms;
  };
})
