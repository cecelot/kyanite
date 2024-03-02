{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    nixpkgs,
    rust-overlay,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem
    (system: let
      overlays = [(import rust-overlay)];
      pkgs = import nixpkgs {inherit overlays system;};
      rust-stable = pkgs.rust-bin.stable.latest.default.override {
        extensions = ["rust-src"];
      };
    in {
      packages.default = pkgs.rustPlatform.buildRustPackage {
        pname = "kyanite";
        version = "0.2.0";
        src = ./.;
        nativeBuildInputs = with pkgs; [
          rust-stable
          # LLVM
          llvmPackages_15.libllvm
          ncurses # -ltinfo
          libffi # -lffi
          libxml2 # -lxml2
          # Nix
          makeWrapper
        ];
        cargoLock = {
          lockFile = ./Cargo.lock;
        };
        buildPhase = ''
          cargo build --release --package runtime
          cargo build --release
        '';
        installPhase = ''
          mkdir -p $out/bin
          mkdir -p $out/lib
          cp target/release/libkyanite_runtime.a $out/lib/libkyanite_runtime.a
          cp target/release/main $out/bin/kyanite
          wrapProgram $out/bin/kyanite \
            --set KYANITE_RUNTIME_LIB $out/lib \
            --prefix PATH : ${pkgs.lib.makeBinPath (
            with pkgs; [
              llvmPackages_15.libllvm
            ]
          )}
        '';
        RUSTFLAGS = "-C link-arg=-lc++abi"; # https://github.com/NixOS/nixpkgs/issues/166205
        LLVM_SYS_150_PREFIX = pkgs.llvmPackages_15.libllvm.dev;
        meta = {
          description = "A toy compiled programming language to learn more about PLs";
          homepage = "https://github.com/alythical/kyanite";
          license = pkgs.lib.licenses.mit;
          platforms = ["aarch64-darwin"];
        };
      };
    });
}
