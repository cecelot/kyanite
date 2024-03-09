{
  pkgs,
  rust-bin,
}:
pkgs.rustPlatform.buildRustPackage {
  pname = "kyanite-rustfmt-check";
  version = "0.0.0";
  src = ../.;
  doCheck = false;
  nativeBuildInputs = [rust-bin.stable.latest.default];
  cargoLock = {
    lockFile = ../Cargo.lock;
  };
  buildPhase = ''
    cargo fmt --check
  '';
  installPhase = ''
    touch $out
  '';
}
