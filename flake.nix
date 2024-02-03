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
      overlays = [
        (import rust-overlay)
        personal.overlays.default
      ];
      pkgs = import nixpkgs {
        inherit overlays;
        inherit system;
      };
      rust-stable = pkgs.rust-bin.stable.latest.default.override {
        extensions = ["rust-src"];
        targets = ["x86_64-apple-darwin"];
      };
      buildInputs = with pkgs; [
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
      ];
      CARGO_TARGET_AARCH64_APPLE_DARWIN_RUSTFLAGS = "-C link-arg=-lc++abi"; # https://github.com/NixOS/nixpkgs/issues/166205
      LLVM_SYS_150_PREFIX = pkgs.llvmPackages_15.libllvm.dev;
      CARGO_TARGET_X86_64_APPLE_DARWIN_LINKER = "${pkgs.pkgsx86_64Darwin.llvmPackages.clangUseLLVM}/bin/cc";
    in {
      devShells.default = pkgs.mkShell {
        inherit buildInputs CARGO_TARGET_AARCH64_APPLE_DARWIN_RUSTFLAGS LLVM_SYS_150_PREFIX CARGO_TARGET_X86_64_APPLE_DARWIN_LINKER;
      };
      packages.default = pkgs.rustPlatform.buildRustPackage {
        inherit CARGO_TARGET_AARCH64_APPLE_DARWIN_RUSTFLAGS LLVM_SYS_150_PREFIX CARGO_TARGET_X86_64_APPLE_DARWIN_LINKER;
        pname = "kyanite";
        version = "0.1.0";
        doCheck = false;
        src = ./.;
        nativeBuildInputs = buildInputs ++ (with pkgs; [makeWrapper]);
        cargoLock = {
          lockFile = ./Cargo.lock;
        };
        buildPhase = ''
          cargo build --release --package builtins --target x86_64-apple-darwin
          cargo build --release --package builtins --target aarch64-apple-darwin
          cargo build --release
        '';
        installPhase = ''
          mkdir -p $out/bin
          mkdir -p $out/lib/llvm-support
          mkdir -p $out/lib/kyir-support
          cp target/x86_64-apple-darwin/release/libkyanite_builtins.dylib $out/lib/kyir-support/libkyanite_builtins.dylib
          cp target/aarch64-apple-darwin/release/libkyanite_builtins.dylib $out/lib/llvm-support/libkyanite_builtins.dylib
          cp target/release/main $out/bin/kyanite
          wrapProgram $out/bin/kyanite \
            --set KYANITE_BUILTINS_LIB $out/lib \
            --prefix PATH : ${pkgs.lib.makeBinPath (with pkgs; [zig-custom])}
          '';
      };
    });
}
