{
  description = "WMonad flake";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.master.follows = "nixpkgs";
  inputs.large.follows = "nixpkgs";
  inputs.nix.follows = "nix";

  inputs.nixos.url = "github:bqv/nixrc/live";

  outputs = { self, nixos, ... }: {

    packages.x86_64-linux = rec {
      inherit (nixos.packages.x86_64-linux.velox) swc;
      wmonad = let
        pkg = { haskellPackages, libinput }: haskellPackages.callPackage ./. {
          inherit swc;
          input = libinput;
        };
      in 
        nixos.legacyPackages.x86_64-linux.callPackage pkg {};
    };

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.wmonad;

    devShell.x86_64-linux = nixos.legacyPackages.x86_64-linux.mkShell {
      buildInputs = [
        nixos.legacyPackages.x86_64-linux.wayland
        nixos.legacyPackages.x86_64-linux.libinput
        nixos.legacyPackages.x86_64-linux.libxkbcommon
        nixos.packages.x86_64-linux.velox.swc
      ];
      nativeBuildInputs = [
        nixos.legacyPackages.x86_64-linux.cabal2nix
      ];
    };

  };
}
