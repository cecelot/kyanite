use colored::Colorize;
use std::ffi::CStr;

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
/// This function will panic if the allocation fails.
pub extern "C" fn alloc(size: usize) -> *const u8 {
    let allocated = unsafe { libc::malloc(size) };
    if allocated.is_null() {
        panic!(
            "{}: failed to allocate memory",
            "runtime error".red().bold()
        );
    } else {
        allocated.cast()
    }
}
