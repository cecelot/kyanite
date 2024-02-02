# Kyanite

Languages are cool! Compilers are cool! How do they work? That's why this project exists: Kyanite is a statically-typed, compiled programming language to learn more about how PLs are created. The primary backend is LLVM, and a custom backend (`kyir`) is a work-in-progress.

## Documentation

There are preliminary docs [here](https://alaidriel.github.io/kyanite/), and some working samples in the `examples/` directory for both backends to demonstrate basic features.

## Explore

As of now, the only programs confirmed to be working are those in the `examples/` directory. Experiment with other programs at your own peril, and expect panics to occur, particularly with the `kyir` backend. macOS and Linux _should_ both work using the method described below.

The project is a standard Rust workspace; the entrypoint is found within `crates/kyanite`. The recommended way to run `.kya` programs is with Nix:

```sh
nix develop -c cargo run -- run <filename>
```

Other methods may also work, but are untested. LLVM and clang are additional required dependencies.

**Note**: The CLI is extremely fragile. It's required to run the binary from inside the repository, since `libkyanite_builtins` is currently recompiled every time the binary is run. I'm working on figuring out a good way to handle builtins.
