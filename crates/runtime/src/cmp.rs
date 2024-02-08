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
