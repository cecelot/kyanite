alias r := run
alias t := test
alias b := build
alias rl := run-llvm

_default:
    @just --list

build:
    cargo build --package runtime --target x86_64-apple-darwin
    cargo build

test: build
    cargo test

run file verbosity = "":
    cargo run -- --kyir run {{file}} {{verbosity}}

run-llvm file verbosity = "":
    cargo run -- run {{file}} {{verbosity}}
