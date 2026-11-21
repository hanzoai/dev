//! Provider adapters. The recorded fixtures in `fixtures/` are the contract:
//! `<fixture>/<driver>.transcript.ndjson` is the provider wire, `.input.json` the
//! steps that drive it, `.events.json` the events a replay must produce.
//! The TS adapters and these read the same files.

pub mod fixture;
