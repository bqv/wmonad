{
  description = "WMonad flake";

  inputs.nixpkgs.follows = "rc/master";
  inputs.nix.follows = "rc/nix";
  inputs.rc = {
    url = "github:bqv/nixrc";
    inputs = { # Follows is still a bit tempramental
      construct.inputs.nixpkgs.follows = "rc/large";
      dwarffs.inputs.nixpkgs.follows = "rc/master";
      guix.inputs.nixpkgs.follows = "rc/master";
      home.inputs.nixpkgs.follows = "rc/master";
      naersk.inputs.nixpkgs.follows = "rc/master";
      nix.inputs.nixpkgs.follows = "rc/master";
      wayland.inputs.nixpkgs.follows = "rc/small";
      xontribs.inputs.nixpkgs.follows = "rc/master";
    };
  };

  outputs = { self, rc, ... }: {

    packages.x86_64-linux = let
      pkgs = rc.legacyPackages.x86_64-linux
          // rc.packages.x86_64-linux;
    in rec {
      inherit (pkgs.velox) swc;
      wmonad = pkgs.haskellPackages.callCabal2nix "wmonad" ./. {
        inherit swc;
        input = pkgs.libinput;
      };
    };

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.wmonad;

    devShell.x86_64-linux = rc.legacyPackages.x86_64-linux.mkShell {
      buildInputs = [
        rc.legacyPackages.x86_64-linux.wayland
        rc.legacyPackages.x86_64-linux.libinput
        rc.legacyPackages.x86_64-linux.libxkbcommon
        rc.packages.x86_64-linux.velox.swc
      ];
      nativeBuildInputs = [
        rc.legacyPackages.x86_64-linux.cabal2nix
      ];
    };

  };
}
