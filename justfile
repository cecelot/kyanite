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

run file verbosity = "": build
    cargo run -- --kyir run {{file}} {{verbosity}}

run-llvm file verbosity = "": build
    cargo run -- run {{file}} {{verbosity}}

zig file:
    zig cc kya-dist/{{file}}.kya.s -o kya-dist/{{file}}.kya -target x86_64-macos -L /Users/aly/Developer/alaidriel/kyanite/target/x86_64-apple-darwin/debug -lkyanite_runtime

run-dyld file:
    DYLD_LIBRARY_PATH=$(pwd)/target/x86_64-apple-darwin/debug ./kya-dist/{{file}}.kya