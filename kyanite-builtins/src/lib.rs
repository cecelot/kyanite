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
    println!("{}", b);
}

#[no_mangle]
pub extern "C" fn println_int(i: i32) {
    println!("{}", i);
}

#[no_mangle]
pub extern "C" fn println_float(f: f64) {
    println!("{}", f);
}

#[no_mangle]
pub extern "C" fn println_str(s: *const u8) {
    let s = unsafe { CStr::from_ptr(s as *const i8) };
    println!("{}", s.to_str().unwrap());
}
