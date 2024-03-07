mod alloc;
mod cmp;
mod print;

pub use alloc::METADATA_FIELDS;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");
