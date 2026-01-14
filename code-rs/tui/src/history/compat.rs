//! Compatibility layer that mirrors upstream history exports while allowing
//! the fork to evolve `hanzo_core::history` without touching call sites.
#![cfg(feature = "hanzo-fork")]

// Re-export everything from the core history module so callers can switch
// from `crate::history::state` to this module without behavioural changes.
pub use hanzo_core::history::*;
