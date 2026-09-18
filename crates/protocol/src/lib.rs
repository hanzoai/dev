//! Events in, actions out — the only language the loop and its host share.
//!
//! HIP-1330 splits a coding agent at the seam between reasoning and effect.
//! This crate is the alphabet of that seam: the host observes the world and
//! sends an [`Event`]; the core reasons and answers with [`Action`]s. Every
//! action carries an `id`, so the host's ledger can record dispatch and
//! completion under it and replay a result instead of repeating an effect.
//!
//! # Encoding
//!
//! [`encode`] and [`decode`] are the only two places that know the byte
//! format, so the format can change without moving a type or a C symbol.
//! Today it is `serde_json`.
//!
//! It becomes ZAP (HIP-0114) when the ZAP schema compiler is a build input
//! this repo has. The Rust side is published — `zap-proto` on crates.io
//! re-exports `zap-schema`, the same runtime that sits in `zap/rust/zap` —
//! but it is a runtime for generated code, and generating that code takes the
//! `zap` schema compiler, which is not installed here and is not a build
//! input of this repo or its CI. Pointing the generator at `capnp` instead
//! would be a shim across a fork boundary, and hand-writing the wire format
//! would be worse than either. So the two functions below stay serde until
//! the compiler is a dependency, and nothing here imitates ZAP framing in the
//! meantime.

use serde::Deserialize;
use serde::Serialize;
use serde::de::DeserializeOwned;

/// Version of the byte format and of the C ABI that carries it.
pub const ABI: u32 = 1;

/// A decode failure, with the reason the bytes were refused.
#[derive(Debug)]
pub struct Malformed(String);

impl core::fmt::Display for Malformed {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "malformed payload: {}", self.0)
    }
}

impl std::error::Error for Malformed {}

/// Encode a payload for the seam.
pub fn encode<T: Serialize>(value: &T) -> Vec<u8> {
    // serde_json only fails on a serializer that refuses a value (a map with
    // non-string keys, a float that is not finite). No protocol type has one.
    match serde_json::to_vec(value) {
        Ok(bytes) => bytes,
        Err(e) => panic!("protocol type is not encodable: {e}"),
    }
}

/// Decode a payload from the seam.
pub fn decode<T: DeserializeOwned>(bytes: &[u8]) -> Result<T, Malformed> {
    serde_json::from_slice(bytes).map_err(|e| Malformed(e.to_string()))
}

/// What the host tells the core.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Event {
    /// A person asks for work.
    Turn(Turn),
    /// A model answered the request dispatched under `id`.
    Model(Answer),
    /// A command dispatched under `id` exited.
    Exec(Output),
    /// A read, write or patch dispatched under `id` finished.
    File(Output),
    /// A git command dispatched under `id` exited.
    Git(Output),
    /// A browser request dispatched under `id` returned.
    Browse(Output),
    /// Time passed; a long turn should checkpoint.
    Timer,
    /// Stop this turn.
    Cancel,
}

/// What the core asks the host to do.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Action {
    pub id: u64,
    pub op: Op,
}

/// The ten things a core can ask for.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Op {
    Model(Ask),
    Read(Read),
    Write(Write),
    Patch(Patch),
    Exec(Exec),
    Git(Git),
    Browse(Browse),
    /// Show this text to whoever is watching.
    Emit(String),
    /// Persist the core's snapshot at this point in the sequence.
    Save,
    /// The turn is over.
    Done(Outcome),
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Turn {
    pub prompt: String,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Ask {
    pub model: Option<String>,
    pub messages: Vec<Message>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Read {
    pub path: String,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Write {
    pub path: String,
    pub text: String,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Patch {
    pub diff: String,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Exec {
    pub argv: Vec<String>,
    pub cwd: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Git {
    pub argv: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Browse {
    pub url: String,
    pub body: Option<String>,
}

/// Why a turn ended.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Outcome {
    Complete,
    Cancelled,
}

/// A model's answer to an [`Op::Model`].
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Answer {
    pub id: u64,
    pub reply: Reply,
}

/// Text, tool calls, or both.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Reply {
    pub text: Option<String>,
    pub calls: Vec<Call>,
}

/// A tool call, already typed. The host's model layer maps a provider's
/// function call onto one of these; the core never dispatches on a string.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Call {
    Read(Read),
    Write(Write),
    Patch(Patch),
    Exec(Exec),
    Git(Git),
    Browse(Browse),
}

/// The result of an effect, naming the action it answers.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Output {
    pub id: u64,
    pub text: String,
    pub failed: bool,
}

/// Who said something.
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum Role {
    User,
    Agent,
    Tool,
}

/// One entry of the conversation the core carries.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Message {
    pub role: Role,
    pub text: String,
}

/// What a session is born with.
#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct Config {
    /// Model the core names in every [`Op::Model`]; the host routes it.
    pub model: Option<String>,
    /// First message of the conversation, if the host wants one.
    pub prelude: Option<String>,
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn event_round_trips() {
        let event = Event::Turn(Turn {
            prompt: "fix the build".to_string(),
        });
        let back: Event = match decode(&encode(&event)) {
            Ok(e) => e,
            Err(e) => panic!("{e}"),
        };
        assert_eq!(event, back);
    }

    #[test]
    fn action_round_trips_with_its_id() {
        let action = Action {
            id: 7821,
            op: Op::Exec(Exec {
                argv: vec!["cargo".to_string(), "test".to_string()],
                cwd: None,
            }),
        };
        let back: Action = match decode(&encode(&action)) {
            Ok(a) => a,
            Err(e) => panic!("{e}"),
        };
        assert_eq!(back.id, 7821);
        assert_eq!(action, back);
    }

    #[test]
    fn garbage_is_refused_with_a_reason() {
        let err = decode::<Event>(b"not a payload").expect_err("garbage decoded");
        assert!(err.to_string().starts_with("malformed payload:"));
    }
}
