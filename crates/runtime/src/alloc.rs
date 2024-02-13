use bumpalo::Bump;
use std::{alloc::Layout, ffi::CStr, sync::Mutex};

const LIMIT: usize = 4_000_000;

lazy_static::lazy_static! {
    pub static ref GLOBAL: Mutex<Allocator> = Mutex::new(Allocator::new());
}

#[derive(Default, Debug)]
pub struct Allocator {
    arena: Bump,
}

impl Allocator {
    pub fn new() -> Self {
        Self { arena: Bump::new() }
    }

    pub fn alloc(&mut self, descriptor: *const u8, count: usize) -> *const u64 {
        self.arena.set_allocation_limit(Some(LIMIT));
        let Ok(ptr) = self
            .arena
            .try_alloc_layout(Layout::array::<u64>(count).unwrap())
        else {
            todo!("call gc")
        };
        let dst = ptr.as_ptr().cast();
        unsafe {
            std::ptr::copy(descriptor, dst, count - 1);
        }
        dst.cast()
    }
}

#[no_mangle]
/// # Panics
/// This function will panic if the allocation fails or
/// if the string is not valid UTF-8.
pub extern "C" fn alloc(descriptor: *const u8) -> *const u64 {
    let count = unsafe { CStr::from_ptr(descriptor.cast()) }
        .to_bytes()
        .len()
        + 1;
    GLOBAL.lock().unwrap().alloc(descriptor, count)
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
