alias r := run
alias b := build
alias rl := run-llvm

_default:
    @just --list

build:
    cargo build --package builtins --target x86_64-apple-darwin
    cargo build

run file:
    cargo run -- --kyir run {{file}}

run-llvm file:
    cargo run -- run {{file}}
