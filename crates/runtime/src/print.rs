use std::ffi::CStr;

#[no_mangle]
pub extern "C" fn println_bool(b: bool) {
    println!("{b}");
}

#[no_mangle]
pub extern "C" fn println_int(i: i64) {
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
