use bumpalo::Bump;
use std::{alloc::Layout, collections::HashMap, ffi::CStr, ptr::NonNull, sync::Mutex};

/// The maximum number of bytes that can be allocated before
/// running the garbage collector.
const LIMIT: usize = 4_000_000;
/// The metadata fields count.
pub const METADATA_FIELDS: usize = 2;

lazy_static::lazy_static! {
    static ref GLOBAL: Mutex<Allocator> = Mutex::new(Allocator::new());
}

#[derive(Debug)]
pub struct Allocator {
    /// from-space
    current: Bump,
    allocations: Mutex<Vec<*const u8>>,
    sp: Mutex<*const u8>,
}

// SAFETY: the raw pointers are behind a `Mutex`
unsafe impl Send for Allocator {}

fn init() -> Bump {
    let bump = Bump::new();
    bump.set_allocation_limit(Some(LIMIT));
    bump
}

impl Allocator {
    pub fn new() -> Self {
        Self {
            current: init(),
            allocations: Mutex::new(Vec::new()),
            sp: Mutex::new(std::ptr::null()),
        }
    }

    pub fn alloc(
        &mut self,
        descriptor: *const u8,
        frame: FrameInfo,
        count: usize,
        tries: usize,
    ) -> Result<*const u8, &'static str> {
        // `KYANITE_GC_ALWAYS` is set during tests. If we're running tests, we want to force a garbage collection
        // at every allocation to ensure it is functioning correctly.
        if std::env::var("KYANITE_GC_ALWAYS").is_ok() {
            self.gc(&frame);
        }
        // tries == 0: first attempt
        // tries == 1: we've garbage collected, try again
        // tries == 2: we've garbage collected again, give up
        if tries < 2 {
            let space = self
                .current
                .try_alloc_layout(Layout::array::<u64>(count).unwrap());
            if let Ok(ptr) = space {
                let dst = ptr.as_ptr().cast();
                unsafe {
                    // Copy the descriptor string to the allocated memory
                    std::ptr::copy(descriptor, dst, count - 1);
                }
                // keep track of this allocation so the garbage collector knows what values to scan for
                self.allocations.lock().unwrap().push(dst);
                Ok(dst.cast())
            } else {
                self.gc(&frame);
                self.alloc(descriptor, frame, count, tries + 1)
            }
        } else {
            Err("runtime: alloc: failed to allocate memory")
        }
    }

    fn reachable(&mut self, fp: *const u8, sp: *const u8) -> Vec<(*const u8, *const u8)> {
        log(&format!("runtime: gc: scanning range [{fp:?}, {sp:?}]"));
        (0..)
            .step_by(8)
            .skip(1)
            .map_while(|offset| {
                let src = unsafe { fp.add(offset) };
                (src <= sp).then_some(src)
            })
            .filter(|src| {
                let cls = unsafe { std::ptr::read(src.cast()) };
                log(&format!("runtime: gc: scanning {src:?} -> {}", cls as u64));
                let forward = {
                    let allocations = self.allocations.lock().unwrap();
                    allocations.contains(&cls)
                };
                forward
            })
            .map(|src| (src, unsafe { std::ptr::read(src.cast()) }))
            .collect()
    }

    fn forward_child_fields(
        reachable: &[(*const u8, *const u8)],
        children: &HashMap<u64, Vec<*mut u8>>,
        forwarded: &HashMap<*const u8, *const u8>,
    ) {
        for &cls in reachable.iter().map(|(_, cls)| cls) {
            if let Some(fields) = children.get(&(cls as u64)) {
                let &ars = forwarded.get(&cls).unwrap();
                for &new_value_ptr in fields {
                    log(&format!(
                        "runtime: gc: updating child pointer at {new_value_ptr:?} to {ars:?}"
                    ));
                    unsafe {
                        std::ptr::write::<*mut u8>(new_value_ptr.cast(), ars.cast_mut());
                    }
                }
            }
        }
    }

    fn copy_fields(
        count: usize,
        descriptor: &str,
        class: *const u8,
        new_region: NonNull<u8>,
        children: &mut HashMap<u64, Vec<*mut u8>>,
    ) {
        for (n, offset) in (0..count).map(|i| i * 8).enumerate() {
            unsafe {
                let new_value_ptr = new_region.as_ptr().add(offset);
                let current_value_ptr = class.add(offset);
                let current_value: u64 = std::ptr::read(current_value_ptr.cast());
                log(&format!("runtime: gc: [{offset}]: copying {current_value} from {current_value_ptr:?} to {new_value_ptr:?}"));
                std::ptr::copy::<u64>(current_value_ptr.cast(), new_value_ptr.cast(), 1);
                if n > 1 {
                    let pointer = descriptor.as_bytes()[n - METADATA_FIELDS] == b'p';
                    if pointer {
                        // we need to move *into* new_value_ptr the forwarded ptr for current_value
                        children
                            .entry(current_value)
                            .or_default()
                            .push(new_value_ptr);
                    }
                }
            }
        }
    }

    /// A garbage collector using breadth-first copying which traverses the currently reachable stack
    /// and forwards all valid classes it finds from `self.current` (from-space) to a new region of memory
    /// using the `Bump` allocator (to-space).
    pub fn gc(&mut self, frame: &FrameInfo) {
        let fp = unsafe { frame.ptr.sub(frame.size.abs().try_into().unwrap()) };
        let sp = *self.sp.lock().unwrap();
        let reachable = self.reachable(fp, sp);
        log(&format!("runtime: gc: forward: {reachable:#?}"));
        log(&format!(
            "runtime: gc: current: {:#?}",
            self.allocations.lock().unwrap()
        ));
        let mut scratch = init();
        let mut allocations: Vec<*const u8> = Vec::new();
        let mut forwarded: HashMap<*const u8, *const u8> = HashMap::new();
        let mut children: HashMap<_, Vec<*mut u8>> = HashMap::new();
        for &(loc, class) in &reachable {
            let forwarded = forwarded.get(&class).copied().unwrap_or_else(|| {
                let descriptor = unsafe { read_string(class).0 };
                log(&format!(
                    "runtime: gc: stack({loc:?}): (descriptor: {descriptor}), forwarding {class:?}"
                ));
                let count = descriptor.len() + METADATA_FIELDS;
                let new_region = scratch.alloc_layout(Layout::array::<u64>(count).unwrap());
                allocations.push(new_region.as_ptr().cast());
                Self::copy_fields(count, &descriptor, class, new_region, &mut children);
                forwarded.insert(class, new_region.as_ptr());
                new_region.as_ptr()
            });
            log(&format!(
                "runtime: gc: stack({loc:?}): forwarding {class:?} to {forwarded:?}"
            ));
            unsafe {
                std::ptr::write::<u64>(loc.cast_mut().cast(), forwarded as u64);
            }
        }
        // Forward all child fields after we finish forwarding everything else, otherwise we might
        // miss some fields that need to be forwarded.
        Self::forward_child_fields(&reachable, &children, &forwarded);
        log(&format!("runtime: gc: forwarding table: {forwarded:#?}"));
        std::mem::swap(&mut self.current, &mut scratch);
        self.allocations = Mutex::new(allocations);
        scratch.reset();
    }
}

unsafe fn read_string(ptr: *const u8) -> (String, *const u8) {
    let label = CStr::from_ptr(ptr.cast());
    let count = label.to_bytes().len() + 1;
    let ptr = ptr.add(count);
    (label.to_string_lossy().into_owned(), ptr)
}

pub struct FrameInfo {
    ptr: *const u8,
    size: i64,
}

impl FrameInfo {
    fn new(ptr: *const u8, size: i64) -> Self {
        Self { ptr, size }
    }
}

#[no_mangle]
/// # Panics
/// This function will panic if the allocation fails or
/// if the string is not valid UTF-8.
pub extern "C" fn alloc(field_descriptor: *const u8, fp: *const u8, size: i64) -> *const u64 {
    let frame = FrameInfo::new(fp, size);
    let count = unsafe { CStr::from_ptr(field_descriptor.cast()) }
        .to_bytes()
        .len()
        + METADATA_FIELDS;
    match GLOBAL
        .lock()
        .unwrap()
        .alloc(field_descriptor, frame, count, 0)
    {
        Ok(ptr) => ptr.cast(),
        Err(msg) => panic!("{msg}"),
    }
}

#[no_mangle]
pub extern "C" fn init_array(len: i64) -> *const u64 {
    // FIXME: multiple allocators don't like each other
    let slice = vec![0; usize::try_from(len).unwrap()];
    Box::into_raw(slice.into_boxed_slice()).cast()
}

#[no_mangle]
pub extern "C" fn set_stack_base(sp: *const u8) {
    *GLOBAL.lock().unwrap().sp.lock().unwrap() = sp;
}

fn log(msg: &str) {
    if std::env::var("KYANITE_LOG_GC").is_ok() {
        println!("{msg}");
    }
}
