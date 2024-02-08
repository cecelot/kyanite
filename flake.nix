{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    personal.url = "github:alaidriel/nix-packages";
  };

  outputs = {
    nixpkgs,
    rust-overlay,
    flake-utils,
    personal,
    ...
  }:
    flake-utils.lib.eachDefaultSystem
    (system: let
      overlays = [(import rust-overlay) personal.overlays.default];
      pkgs = import nixpkgs {inherit overlays system;};
      rust-stable = pkgs.rust-bin.stable.latest.default.override {
        extensions = ["rust-src"];
        targets = ["x86_64-apple-darwin"];
      };
    in {
      packages.default = pkgs.rustPlatform.buildRustPackage {
        pname = "kyanite";
        version = "0.2.0";
        src = ./.;
        checkFlags = [
          # these want a writeable fs for some reason
          "--skip=kyir"
        ];
        nativeBuildInputs = with pkgs; [
          # rust/cargo
          rust-stable
          cargo-audit
          cargo-insta
          cargo-sort
          # zig
          zig-custom
          # dependencies
          llvmPackages_15.libllvm
          # (for llvm)
          ncurses # -ltinfo
          libffi # -lffi
          libxml2 # -lxml2
          # misc
          mdbook
          makeWrapper
        ];
        cargoLock = {
          lockFile = ./Cargo.lock;
        };
        buildPhase = ''
          cargo build --release --package runtime --target x86_64-apple-darwin
          cargo build --release --package runtime --target aarch64-apple-darwin
          cargo build --release
        '';
        installPhase = ''
          mkdir -p $out/bin
          mkdir -p $out/lib/llvm-support
          mkdir -p $out/lib/kyir-support
          cp target/x86_64-apple-darwin/release/libkyanite_runtime.dylib $out/lib/kyir-support/libkyanite_runtime.dylib
          cp target/aarch64-apple-darwin/release/libkyanite_runtime.dylib $out/lib/llvm-support/libkyanite_runtime.dylib
          cp target/release/main $out/bin/kyanite
          wrapProgram $out/bin/kyanite \
            --set KYANITE_BUILTINS_LIB $out/lib \
            --prefix PATH : ${pkgs.lib.makeBinPath (
            with pkgs; [
              llvmPackages_15.libllvm
              zig-custom
            ]
          )}
        '';
        LIBRARY_PATH = pkgs.lib.makeLibraryPath [pkgs.libiconv]; # -liconv *why* (https://github.com/nix-community/home-manager/issues/3482)
        CARGO_TARGET_AARCH64_APPLE_DARWIN_RUSTFLAGS = "-C link-arg=-lc++abi"; # https://github.com/NixOS/nixpkgs/issues/166205
        CARGO_TARGET_X86_64_APPLE_DARWIN_RUSTFLAGS = pkgs.lib.optional pkgs.stdenv.isx86_64 "-C link-arg=-lc++abi";
        LLVM_SYS_150_PREFIX = pkgs.llvmPackages_15.libllvm.dev;
        CARGO_TARGET_X86_64_APPLE_DARWIN_LINKER = "${pkgs.pkgsx86_64Darwin.llvmPackages.clangUseLLVM}/bin/cc";
        meta = {
          description = "A toy compiled programming language to learn more about PLs";
          homepage = "https://github.com/alaidriel/kyanite";
          license = pkgs.lib.licenses.mit;
          platforms = pkgs.lib.platforms.darwin;
        };
      };
    });
}
