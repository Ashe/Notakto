{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };
  outputs = { self, nixpkgs }: let pkgs = nixpkgs.legacyPackages.x86_64-linux; in rec {
    packages.x86_64-linux = {
      default = pkgs.haskellPackages.callCabal2nix "notakto" ./. {};
    };
    apps.default = { type = "app"; program = packages.default; };
    devShell.x86_64-linux = pkgs.haskellPackages.shellFor {
      packages = p: [ packages.x86_64-linux.default ];
      withHoogle = true;
      buildInputs = with pkgs.haskellPackages; [
        haskell-language-server
        ghcid
        cabal-install
      ];
    };
  };
}
