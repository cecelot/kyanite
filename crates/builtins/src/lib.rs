use colored::Colorize;
use std::ffi::CStr;

const WORD_SIZE: usize = std::mem::size_of::<u64>();

#[no_mangle]
pub extern "C" fn max_int(a: i64, b: i64) -> i64 {
    a.max(b)
}

#[no_mangle]
pub extern "C" fn min_int(a: i64, b: i64) -> i64 {
    a.min(b)
}

#[no_mangle]
pub extern "C" fn max_float(a: f64, b: f64) -> f64 {
    a.max(b)
}

#[no_mangle]
pub extern "C" fn min_float(a: f64, b: f64) -> f64 {
    a.min(b)
}

#[no_mangle]
pub extern "C" fn println_bool(b: bool) {
    println!("{b}");
}

#[no_mangle]
pub extern "C" fn println_int(i: i32) {
    println!("{i}");
}

#[no_mangle]
pub extern "C" fn println_float(f: f64) {
    println!("{f}");
}

#[no_mangle]
/// # Panics
/// This function will panic if the string is not valid UTF-8.
pub extern "C" fn println_str(s: *const u8) {
    let s = unsafe { CStr::from_ptr(s.cast::<i8>()) };
    println!("{}", s.to_str().unwrap());
}

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
        let root = allocated.cast::<u64>();
        #[allow(clippy::cast_ptr_alignment)]
        unsafe {
            std::ptr::copy(descriptor.as_ptr().cast::<u64>(), root, descriptor.len());
            root.offset(1)
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn allocate() {
        let descriptor = "iipi";
        let res = super::alloc(descriptor.as_ptr());
        let offset: isize = super::WORD_SIZE.try_into().unwrap();
        let back = unsafe { std::ffi::CStr::from_ptr(res.cast::<i8>().offset(-offset)) };
        assert_eq!(back.to_str().unwrap(), descriptor);
    }
}
