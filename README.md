# Kyanite

Languages are cool! Compilers are cool! How do they work? That's why this project exists: Kyanite is a statically-typed, compiled programming language to learn more about how PLs are created. There are two backends: LLVM and a custom IR, `kyir`. The latter currently supports more language features, but is much less stable than the former.

## Explore

_(reqiures macOS on Apple Silicon)_

### Nix

Kyanite is available on [FlakeHub](https://flakehub.com/flake/alythical/kyanite):

```
nix build "https://flakehub.com/f/alythical/kyanite/[tag].tar.gz"
./result/bin/kyanite run path/to/program.kya
```

`[tag]` should be replaced by either:

1. a version string matching `0.1.[commit-count]` (where `[commit-count]` is the total number of commits in this repository)
2. any other [published release](https://flakehub.com/flake/alythical/kyanite/releases) tag

The test suite verifies the programs in the `examples` directory function correctly. Feel free to experiment with other programs, but expect panics to occur, particularly with the `kyir` backend.

> **Note**: By default, the `kyir` backend is used. Provide the `--llvm` flag to compile with LLVM instead.

### Cargo

The [Nix derivation](https://github.com/alythical/kyanite/blob/main/nix/package.nix) is an instructive resource for compiling manually. In particular, note `nativeBuildInputs` (dependencies), `buildPhase` and `installPhase`, and the `RUSTFLAGS` and `LLVM_SYS_150_PREFIX` environment variables.

---

Other operating systems are currently unsupported.

## Documentation

There's a reference and user guide [here](https://kyanite.alainacn.dev), and some working samples in the `examples/` directory for both backends to demonstrate basic features.
