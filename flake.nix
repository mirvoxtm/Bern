{
  description = "Bern Interpreted Dynamically Typed Programming Language";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux"];
      perSystem = {pkgs, ...}: let
        hpkgs = pkgs.haskellPackages;
        opts = {root = ./.;};
        buildTools = with hpkgs; [
          cabal-install
          haskell-language-server
        ];
        pkg = op': hpkgs.developPackage (opts // op');
      in {
        packages.default = pkg {};

        devShells.default = pkg {
          returnShellEnv = true;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv buildTools;
        };

        formatter = pkgs.alejandra;
      };
    };
}
