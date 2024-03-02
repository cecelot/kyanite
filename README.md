# Kyanite

Languages are cool! Compilers are cool! How do they work? That's why this project exists: Kyanite is a statically-typed, compiled programming language to learn more about how PLs are created. There are two backends: LLVM and a custom IR, `kyir`. The latter currently supports more language features, but is much less stable than the former.

## Usage

###### you probably don't actually want to do this

As of now, the only programs confirmed to be working are those in the `examples/` directory. Experiment with other programs at your own peril, and expect panics to occur, particularly with the `kyir` backend. If you're using macOS on Apple Silicon, you can test out the compiler by running the following command, assuming Nix is installed:

```
nix run .# -- run examples/kyir/hello.kya
```

> **Note**: By default, the `kyir` backend is used. Provide the `--llvm` flag to compile with LLVM instead.

Other operating systems are currently unsupported.

## Documentation

There are preliminary docs [here](https://alythical.github.io/kyanite/), and some working samples in the `examples/` directory for both backends to demonstrate basic features.
