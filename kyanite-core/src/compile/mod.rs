mod kyir;
mod llvm;

pub use kyir::Kyir;
pub use llvm::LlvmIr;

#[must_use]
pub fn include_dir() -> String {
    let build = if cfg!(debug_assertions) {
        "target/debug"
    } else {
        "target/release"
    };
    let dir = env!("CARGO_MANIFEST_DIR");
    let dir = &dir[0..dir.len() - 12];
    let include = &format!("{dir}{build}");
    include.into()
}

#[must_use]
fn release_flag() -> &'static str {
    if cfg!(debug_assertions) {
        "--"
    } else {
        "--release"
    }
}
