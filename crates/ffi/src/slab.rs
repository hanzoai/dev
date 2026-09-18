//! A generational slab: a handle names a slot and the generation it was born
//! in, so a handle that outlives its session is refused instead of hitting
//! whatever took the slot next.

/// Why a handle did not resolve.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Fault {
    /// No such slot, or the slot has been reused since.
    Stale,
    /// Every slot index is spoken for.
    Full,
    /// A call on this session panicked; the session is gone.
    Poisoned,
    /// The session is inside another call on another thread.
    Busy,
}

struct Slot<T> {
    generation: u32,
    value: Option<T>,
    poisoned: bool,
}

pub struct Slab<T> {
    slots: Vec<Slot<T>>,
    free: Vec<u32>,
}

impl<T> Slab<T> {
    pub const fn new() -> Self {
        Self {
            slots: Vec::new(),
            free: Vec::new(),
        }
    }

    /// Store a value and return its handle. A handle is never zero.
    pub fn insert(&mut self, value: T) -> Result<u64, Fault> {
        match self.free.pop() {
            Some(index) => {
                let slot = match self.slots.get_mut(index as usize) {
                    Some(slot) => slot,
                    None => return Err(Fault::Stale),
                };
                slot.value = Some(value);
                slot.poisoned = false;
                Ok(handle(index, slot.generation))
            }
            None => {
                let Ok(index) = u32::try_from(self.slots.len()) else {
                    return Err(Fault::Full);
                };
                self.slots.push(Slot {
                    generation: 1,
                    value: Some(value),
                    poisoned: false,
                });
                Ok(handle(index, 1))
            }
        }
    }

    /// Lift the value out for the duration of a call, so no lock is held
    /// while the session runs.
    pub fn take(&mut self, handle: u64) -> Result<T, Fault> {
        let slot = self.slot(handle)?;
        if slot.poisoned {
            return Err(Fault::Poisoned);
        }
        slot.value.take().ok_or(Fault::Busy)
    }

    /// Put the value back after the call returned.
    pub fn put(&mut self, handle: u64, value: T) {
        if let Ok(slot) = self.slot(handle) {
            slot.value = Some(value);
        }
    }

    /// Mark the slot as poisoned: the value is gone and the handle answers
    /// [`Fault::Poisoned`] until it is dropped.
    pub fn poison(&mut self, handle: u64) {
        if let Ok(slot) = self.slot(handle) {
            slot.value = None;
            slot.poisoned = true;
        }
    }

    /// Free the slot and retire every handle to it.
    pub fn remove(&mut self, handle: u64) -> Result<(), Fault> {
        let (index, _) = parts(handle);
        let slot = self.slot(handle)?;
        slot.value = None;
        slot.poisoned = false;
        slot.generation = slot.generation.wrapping_add(1);
        if slot.generation == 0 {
            // Generation zero would make a handle collide with the reserved
            // zero handle; skip it.
            slot.generation = 1;
        }
        self.free.push(index);
        Ok(())
    }

    fn slot(&mut self, handle: u64) -> Result<&mut Slot<T>, Fault> {
        let (index, generation) = parts(handle);
        let slot = self.slots.get_mut(index as usize).ok_or(Fault::Stale)?;
        if slot.generation != generation {
            return Err(Fault::Stale);
        }
        Ok(slot)
    }
}

fn handle(index: u32, generation: u32) -> u64 {
    (u64::from(generation) << 32) | u64::from(index)
}

fn parts(handle: u64) -> (u32, u32) {
    let index = (handle & 0xffff_ffff) as u32;
    let generation = (handle >> 32) as u32;
    (index, generation)
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn a_dropped_handle_goes_stale_even_when_the_slot_comes_back() {
        let mut slab: Slab<u32> = Slab::new();
        let first = slab.insert(7).expect("insert");
        slab.remove(first).expect("remove");
        let second = slab.insert(9).expect("insert");
        assert_ne!(first, second);
        assert_eq!(slab.take(first), Err(Fault::Stale));
        assert_eq!(slab.take(second), Ok(9));
    }

    #[test]
    fn a_handle_is_never_zero_and_zero_is_stale() {
        let mut slab: Slab<u32> = Slab::new();
        assert_ne!(slab.insert(1).expect("insert"), 0);
        assert_eq!(slab.take(0), Err(Fault::Stale));
    }

    #[test]
    fn a_taken_slot_is_busy_and_a_poisoned_one_stays_poisoned() {
        let mut slab: Slab<u32> = Slab::new();
        let handle = slab.insert(1).expect("insert");
        assert_eq!(slab.take(handle), Ok(1));
        assert_eq!(slab.take(handle), Err(Fault::Busy));
        slab.poison(handle);
        assert_eq!(slab.take(handle), Err(Fault::Poisoned));
        slab.remove(handle).expect("remove");
        assert_eq!(slab.take(handle), Err(Fault::Stale));
    }
}
