{
  description = "leaf-hs";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        conig.allowUnfree = true;
      };
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          cabal-install
          ghc
          haskellPackages.hls
          ormolu
        ];
      };

      DEV_SHELL_ACTIVE = "leaf-hs";
    };
}
