# Kyanite

Languages are cool! Compilers are cool! How do they work? That's why this project exists: Kyanite is a statically-typed, compiled programming language to learn more about how PLs are created. The primary backend is LLVM, and a custom backend targeting x86_64 (`kyir`) is in early stages of development.

## Documentation

There are work-in-progress docs [here](https://alaidriel.github.io/kyanite/), and some working samples in the `examples/` folder.

## Explore

**Note:** Only macOS is tested; there are unresolved issues on Linux related to linking `libkyanite_builtins` (the shared library for hacking together builtin functions for debug purposes)

The project is a standard Rust workspace; the entrypoint is found within `kyanite-cli`. The recommended way to run `.kya` programs is with Nix:

```sh
nix develop -c cargo run -- run <filename>
```

Other methods may also work, but are untested. Note that LLVM and clang are additional required dependencies.