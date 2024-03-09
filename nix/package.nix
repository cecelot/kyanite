{
  pkgs,
  lib,
  rust-bin,
  llvmPackages_15,
  ncurses,
  libffi,
  libxml2,
  makeWrapper,
}: let
  rust = rust-bin.stable.latest.default.override {extensions = ["rust-src"];};
in
  pkgs.rustPlatform.buildRustPackage {
    pname = "kyanite";
    version = "0.4.0";
    src = ../.;
    nativeBuildInputs = [
      rust
      # LLVM
      llvmPackages_15.libllvm
      ncurses # -ltinfo
      libffi # -lffi
      libxml2 # -lxml2
      # Nix
      makeWrapper
    ];
    cargoLock = {
      lockFile = ../Cargo.lock;
    };
    buildPhase = ''
      cargo build --release --package runtime
      cargo build --release
    '';
    installPhase = ''
      mkdir -p $out/bin
      mkdir -p $out/lib
      cp target/release/libruntime.a $out/lib/libruntime.a
      cp target/release/main $out/bin/kyanite
      wrapProgram $out/bin/kyanite \
        --set KYANITE_RUNTIME_LIB $out/lib \
        --prefix PATH : ${pkgs.lib.makeBinPath [llvmPackages_15.libllvm]}
    '';
    RUSTFLAGS = "-C link-arg=-lc++abi"; # https://github.com/NixOS/nixpkgs/issues/166205
    LLVM_SYS_150_PREFIX = llvmPackages_15.libllvm.dev;
    meta = with lib; {
      description = "A toy compiled programming language to learn more about PLs";
      homepage = "https://github.com/alythical/kyanite";
      license = licenses.mit;
      platforms = ["aarch64-darwin"];
    };
  }
