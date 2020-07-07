{ mkDerivation, base, inline-c, input, libxkbcommon, stdenv, swc
, wayland
}:
mkDerivation {
  pname = "wmonad";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base inline-c ];
  executableSystemDepends = [ input libxkbcommon swc wayland ];
  license = stdenv.lib.licenses.mpl20;
}
