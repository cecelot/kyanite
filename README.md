# kyanite

(WIP) A statically typed, compiled programming language

## Structure
This is the structure of the project, for the sake of keeping my sanity and justifying it to myself.

**`kyanite-core/src`**: the main frontend for kyanite
- `ast`: An abstract syntax tree containing type information for a .kya program
- `bin`: The entrypoint for the compilation interface
- `cli`: Used by `clap` to parse command-line arguments into a Rust struct
- `codegen`: Converts a .kya AST to LLVM IR
    - `codegen/builtins`: Declares external kyanite builtins and produces appropriate LLVM IR `declare` blocks
- `pass`: Creates a symbol table and type checks a .kya program

---

- `parse.rs`: Parses a .kya program to an AST
- `token.rs`: Creates a stream of tokens from an input .kya file

**`kyanite-builtins/src`**: a collection of builtin functions implemented in Rust which are always available to .kya programs via dynamic links (`libkyanite_builtins.dylib`)