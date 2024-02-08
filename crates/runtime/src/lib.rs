mod cmp;
mod gc;
mod print;

use colored::Colorize;
use std::{ffi::CStr, sync::RwLock};

lazy_static::lazy_static! {
    pub static ref GC: RwLock<gc::GarbageCollector> = RwLock::new(gc::GarbageCollector::new());
}

const WORD_SIZE: usize = std::mem::size_of::<u64>();

#[no_mangle]
/// # Panics
/// This function will panic if the allocation fails or
/// if the string is not valid UTF-8.
pub extern "C" fn alloc(descriptor: *const u8) -> *const u64 {
    let descriptor = {
        let c_str = unsafe { CStr::from_ptr(descriptor.cast::<i8>()) };
        c_str.to_str().unwrap()
    };
    let allocated = unsafe { libc::malloc((descriptor.len() + 1) * WORD_SIZE) };
    if allocated.is_null() {
        panic!(
            "{}: failed to allocate memory",
            "runtime error".red().bold()
        );
    } else {
        let root = allocated.cast();
        unsafe {
            std::ptr::copy::<u64>(descriptor.as_ptr().cast(), root, descriptor.len());
        }
        root
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn allocate() {
        let descriptor = "iipi\0";
        let ptr = super::alloc(descriptor.as_ptr());
        let back = unsafe { std::ffi::CStr::from_ptr(ptr.cast()) };
        assert_eq!(back.to_str().unwrap(), "iipi");
    }
}
