alias r := run
alias b := build
alias rl := run-llvm

_default:
    @just --list

build:
    cargo build --package builtins --target x86_64-apple-darwin
    cargo build

run file verbosity = "":
    cargo run -- --kyir run {{file}} {{verbosity}}

run-llvm file verbosity = "":
    cargo run -- run {{file}} {{verbosity}}
