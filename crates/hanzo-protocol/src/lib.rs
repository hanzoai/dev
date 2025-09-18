//! Core protocol types for Hanzo AI SDK
//!
//! This crate provides pure data types with no business logic.
//! All types are serializable and can be used across network boundaries.

pub mod wire;
pub mod events;

pub use wire::{WireProtocol, Request, Response};
pub use events::{Event, EventStream, BoxedEventStream};