# Kyanite

Languages are cool! Compilers are cool! How do they work? That's why this project exists: Kyanite is a statically-typed, compiled programming language to learn more about how PLs are created. The primary backend is LLVM, and a custom backend (`kyir`) is a work-in-progress.

## Usage

###### you probably don't actually want to do this

As of now, the only programs confirmed to be working are those in the `examples/` directory. Experiment with other programs at your own peril, and expect panics to occur, particularly with the `kyir` backend. If you're using macOS, you can test out the compiler by running the following command, assuming Nix is installed:

> **Note:** Only macOS on Apple Silicon is actively tested, but macOS on Intel machines should work as well.

```
nix run .# -- run examples/llvm/hello.kya
```

or, using the `kyir` backend:

```
nix run .# -- run examples/kyir/simple.kya -k
```

Other operating systems are currently unsupported.

## Documentation

There are preliminary docs [here](https://alythical.github.io/kyanite/), and some working samples in the `examples/` directory for both backends to demonstrate basic features.
