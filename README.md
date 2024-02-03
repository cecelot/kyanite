# Kyanite

Languages are cool! Compilers are cool! How do they work? That's why this project exists: Kyanite is a statically-typed, compiled programming language to learn more about how PLs are created. The primary backend is LLVM, and a custom backend (`kyir`) is a work-in-progress.

## Documentation

There are preliminary docs [here](https://alaidriel.github.io/kyanite/), and some working samples in the `examples/` directory for both backends to demonstrate basic features.

## Explore

###### you probably don't actually want to do this

As of now, the only programs confirmed to be working are those in the `examples/` directory. Experiment with other programs at your own peril, and expect panics to occur, particularly with the `kyir` backend. Only macOS is supported using the method below because macOS-specified dependencies are required.

The project is a standard Rust workspace; the entrypoint is found within `crates/kyanite`. The recommended way to run `.kya` programs is with Nix:

```bash
nix build
# with kyir
./result/bin/kyanite --kyir run examples/kyir/simple.kya
# with llvm
./result/bin/kyanite run examples/llvm/hello.kya
```

Other methods may also work, but are untested. zig, LLVM and clang are additional required dependencies.
