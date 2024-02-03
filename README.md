# Kyanite

Languages are cool! Compilers are cool! How do they work? That's why this project exists: Kyanite is a statically-typed, compiled programming language to learn more about how PLs are created. The primary backend is LLVM, and a custom backend (`kyir`) is a work-in-progress.

## Usage

###### you probably don't actually want to do this

As of now, the only programs confirmed to be working are those in the `examples/` directory. Experiment with other programs at your own peril, and expect panics to occur, particularly with the `kyir` backend. If you're using macOS on Apple Silicon, you can test out the compiler by running the following command, assuming Nix is installed:

```
nix run .# -- run examples/llvm/hello.kya
```

or, using the `kyir` backend:

```
nix run .# -- run examples/kyir/simple.kya -k
```

Other platforms are currently unsupported.

## Documentation

There are preliminary docs [here](https://alaidriel.github.io/kyanite/), and some working samples in the `examples/` directory for both backends to demonstrate basic features.
