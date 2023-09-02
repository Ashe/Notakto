{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };
  outputs = { self, nixpkgs }:
  let pkgs = nixpkgs.legacyPackages.x86_64-linux.haskellPackages;
  in rec {
    packages.x86_64-linux = {
      default = pkgs.callCabal2nix "notakto" ./. {};
    };
    devShell.x86_64-linux = pkgs.shellFor {
      packages = p: [ packages.x86_64-linux.default ];
      withHoogle = true;
      nativeBuildInputs = with pkgs; [
        haskell-language-server
        ghcid
        cabal-install
      ];
    };
  };
}
