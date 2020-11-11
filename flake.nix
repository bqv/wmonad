{
  description = "WMonad flake";

  inputs.xkbcommon = { url = "github:ongy/haskell-xkbcommon"; flake = false; };
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

  outputs = inputs@{ self, rc, ... }: let
    systems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
    forAllSystems = f: inputs.nixpkgs.lib.genAttrs systems (sys: f sys);
  in {

    nixosConfigurations = {
      demo = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [{
          environment.systemPackages = [
            self.packages.x86_64-linux.wmonad
            rc.legacyPackages.x86_64-linux.sway
          ];

          hardware.opengl.enable = true;

          users.users.root.password = "toor";
        }];
      };
    };

    packages = forAllSystems (system: let
      pkgs = rc.legacyPackages.${system} // rc.packages.${system};

      haskellPkgs = pkgs.haskellPackages.override {
        overrides = _: super: with pkgs.haskell.lib; {
          inherit (self.packages.${system}) wmonad;
          xkbcommon = markUnbroken
            (overrideSrc super.xkbcommon { src = inputs.xkbcommon; });
          cpphs = appendPatch super.cpphs ./cpphs.patch;
          co-log = markUnbroken super.co-log;
          typerep-map = markUnbroken super.typerep-map;
          polysemy-optics = markUnbroken super.polysemy-optics;
          polysemy-zoo = markUnbroken super.polysemy-zoo;
          compact = markUnbroken super.compact;
          co-log-polysemy-formatting = markUnbroken super.co-log-polysemy-formatting;
          formatting = super.formatting_7_1_1;
          protolude = super.protolude_0_3_0;
        };
      };

      fixWmonad = with pkgs.haskell.lib; p: dontStrip
        (disableHardening (addBuildTools (addPkgconfigDepend
          (appendConfigureFlags p [ "--ghc-options=-g" ])
        pkgs.libinput) [ haskellPkgs.c2hs ]) [ "bindnow" ])
      ;
    in rec {
      inherit (pkgs.velox) swc;
      inherit haskellPkgs;
      inherit (haskellPkgs) shellFor;
      wmonad = (fixWmonad (haskellPkgs.callCabal2nix "wmonad" ./. {
        inherit swc;
      }));
      ghc = haskellPkgs.ghc.withPackages (hs: let
        inherit (rc.legacyPackages.${system}) lib;
      in lib.flatten (builtins.attrValues wmonad.getBuildInputs) ++ [
        hs.c2hs
      ]);
      cabal = haskellPkgs.cabal-install;
    });

    defaultPackage = forAllSystems (system: self.packages.${system}.wmonad);

    apps = forAllSystems (system: {
      demo = with self.nixosConfigurations.demo.config; {
        type = "app";
        program = (rc.legacyPackages.${system}.writeShellScript "demo-vm" ''
          rm -f ${networking.hostName}.qcow2
          exec ${system.build.vm}/bin/run-${networking.hostName}-vm
        '').outPath;
      };
      local = {
        type = "app";
        program = (rc.legacyPackages.${system}.writeShellScript "wmonad-launch" ''
          exec /run/wrappers/bin/swc-launch -t /dev/tty10 \
            -- ${self.packages.${system}.wmonad}/bin/wmonad
        '').outPath;
      };
    });

    devShell = forAllSystems (system: rc.legacyPackages.${system}.mkShell rec {
      CABAL_CONFIG = with rc.legacyPackages.${system}; writeText "config" ''
        extra-include-dirs:
        extra-lib-dirs: ${lib.concatMapStringsSep " " (p: "${lib.getLib p}/lib") [
          wayland libinput libinput.dev libxkbcommon swc ]}
        program-locations
          c2hs-location: ${self.packages.${system}.haskellPkgs.c2hs}/bin/c2hs
        program-default-options
          ghc-options: -L${lib.getLib libinput}/lib -linput
      '';
      inherit (rc.legacyPackages.${system}) wayland;
      inherit (rc.legacyPackages.${system}) libinput;
      inherit (rc.legacyPackages.${system}) libxkbcommon;
      inherit (rc.packages.${system}.velox) swc;
      buildInputs = [
        wayland libinput libinput.dev libxkbcommon swc

        rc.legacyPackages.${system}.cabal2nix
        rc.legacyPackages.${system}.pkg-config
      ];
      propagatedBuildInputs = [
        self.packages.${system}.ghc
        self.packages.${system}.cabal
      ];
      nativeBuildInputs = [
        self.packages.${system}.haskellPkgs.c2hs
        self.packages.${system}.haskellPkgs.xkbcommon
        self.packages.${system}.haskellPkgs.hoogle
      ];
    });

  };
}
