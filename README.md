# Kyanite

Kyanite is a statically-typed, compiled programming language for ARM64, built to understand how programming languages actually work under the hood. It has two backends: LLVM and a custom IR (`kyir`). The custom backend supports more language features; the LLVM backend is more stable.

There's a reference and user guide at [kyanite.sydneyn.dev](https://kyanite.sydneyn.dev), and working samples in `examples/` for both backends.

> Requires macOS on Apple Silicon. Other operating systems are currently unsupported.

## Getting started

### Nix

```bash
nix build .#
./result/bin/kyanite run path/to/program.kya
```

The `kyir` backend is used by default. Pass `--llvm` to compile with LLVM instead.

### Cargo

See the [Nix derivation](https://github.com/sydrinea/kyanite/blob/main/nix/package.nix) for build dependencies and environment variables (`RUSTFLAGS`, `LLVM_SYS_150_PREFIX`).

---

[MIT license](LICENSE)
