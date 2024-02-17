use std::{alloc::Layout, collections::HashMap, ffi::CStr, sync::Mutex};

use bumpalo::Bump;

/// The maximum number of bytes that can be allocated before
/// running the garbage collector.
const LIMIT: usize = 4_000_000;
const ALIGN: usize = 16;

lazy_static::lazy_static! {
    static ref GLOBAL: Mutex<Allocator> = Mutex::new(Allocator::new());
}

#[derive(Default, Debug)]
pub struct Allocator {
    arena: Bump,
    frames: HashMap<String, u64>,
}

impl Allocator {
    pub fn new() -> Self {
        Self {
            arena: {
                let bump = Bump::new();
                bump.set_allocation_limit(Some(LIMIT));
                bump
            },
            frames: HashMap::new(),
        }
    }

    pub fn alloc(
        &mut self,
        descriptor: *const u8,
        count: usize,
        frame: &str,
        ptr: *const u8,
        tries: usize,
    ) -> *const u64 {
        if tries == 0 {
            let space = self
                .arena
                .try_alloc_layout(Layout::array::<u64>(count).unwrap());
            if let Ok(ptr) = space {
                let dst = ptr.as_ptr().cast();
                unsafe {
                    std::ptr::copy(descriptor, dst, count - 1);
                }
                dst.cast()
            } else {
                self.collect_garbage(frame, ptr);
                self.alloc(descriptor, count, frame, ptr, tries + 1)
            }
        } else {
            std::process::exit(1);
        }
    }

    pub fn collect_garbage(&mut self, frame: &str, ptr: *const u8) {
        let pm = PointerMap::new(ptr);
        // println!("frame: {frame}, pm: {pm:?}");
        unsafe {
            let fp = self.get_frame_ptr(frame);
            for chunk in pm.mapping.chunks_exact(2) {
                let offset = chunk[0];
                let rec = chunk[1] != 0;
                if rec {
                    let ptr = fp.sub(offset.try_into().unwrap()).cast();
                    let ptr = std::ptr::read::<u64>(ptr);
                    let (descriptor, _) = read_string(ptr as *const u8);
                    // println!("descriptor: {descriptor}");
                    let _fields: Vec<_> = descriptor
                        .chars()
                        .enumerate()
                        .filter(|(_, c)| *c == 'p')
                        .collect();
                    // TODO: implement copying algorithm
                    // println!("fields: {fields:?}");
                }
            }
        }
    }

    pub fn register_frame_ptr(&mut self, frame: *const u8, ptr: *const u8) {
        unsafe {
            let label = read_string(frame).0;
            self.frames.insert(label, ptr as u64);
        }
    }

    pub fn get_frame_ptr(&self, frame: &str) -> *const u8 {
        (*self.frames.get(frame).unwrap()) as *const u8
    }
}

#[derive(Debug)]
struct PointerMap {
    _label: String,
    mapping: Vec<u32>,
    _previous: Option<Box<PointerMap>>,
}

impl PointerMap {
    fn new(ptr: *const u8) -> Self {
        unsafe {
            let (label, ptr) = read_string(ptr);
            let (previous, ptr) = read_string(ptr);
            let n: usize = read_u32(ptr, 0).try_into().unwrap();
            let bound = (n + 1) * ALIGN;
            let mapping: Vec<_> = (0..bound)
                .step_by(ALIGN)
                .skip(1)
                .map(|i| read_u32(ptr, i))
                .collect();
            let ptr = ptr.add(bound);
            Self {
                _label: label,
                _previous: (previous != "nil").then(|| Box::new(Self::new(ptr))),
                mapping,
            }
        }
    }
}

unsafe fn read_string(ptr: *const u8) -> (String, *const u8) {
    let label = CStr::from_ptr(ptr.cast());
    let count = label.to_bytes().len() + 1;
    let ptr = ptr.add(count);
    (label.to_string_lossy().into_owned(), ptr)
}

unsafe fn read_u32(ptr: *const u8, offset: usize) -> u32 {
    let count: isize = offset.try_into().unwrap();
    let src = ptr.offset(count).cast();
    std::ptr::read(src)
}

#[no_mangle]
/// # Panics
/// This function will panic if the allocation fails or
/// if the string is not valid UTF-8.
pub extern "C" fn alloc(descriptor: *const u8, frame: *const u8, ptr: *const u8) -> *const u64 {
    let frame = unsafe { read_string(frame).0 };
    let count = unsafe { CStr::from_ptr(descriptor.cast()) }
        .to_bytes()
        .len()
        + 1;
    GLOBAL
        .lock()
        .unwrap()
        .alloc(descriptor, count, &frame, ptr, 0)
}

#[no_mangle]
pub extern "C" fn register_frame_ptr(frame: *const u8, ptr: *const u8) {
    GLOBAL.lock().unwrap().register_frame_ptr(frame, ptr);
}
