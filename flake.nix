{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };
  outputs = { self, nixpkgs }:
  let pkgs = nixpkgs.legacyPackages.x86_64-linux.haskellPackages.override {
    overrides = self: super: {
      h-raylib = nixpkgs.legacyPackages.x86_64-linux.haskell.lib.overrideCabal super.h-raylib {
        version = "4.5.0.5";
        sha256 = "sha256-PmDnnaN+RgVf6JetsVc/ko9HBY2xSU3KQQT/MZblU5Q=";
      };
    };
  };
  in rec {
    packages.x86_64-linux = {
      default = pkgs.callCabal2nix "notakto" ./. {};
    };
    apps.default = { type = "app"; program = packages.default; };
    devShell.x86_64-linux = pkgs.shellFor {
      packages = p: [ packages.x86_64-linux.default ];
      withHoogle = true;
      buildInputs = with pkgs; [
        haskell-language-server
        ghcid
        cabal-install
      ];
    };
  };
}
