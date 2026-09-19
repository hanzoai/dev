//! Events in, actions out — the only language the loop and its host share.
//!
//! HIP-1330 splits a coding agent between reasoning and effect. This crate is
//! the alphabet of that split: the host observes the world and sends an
//! [`Event`]; the core reasons and answers with [`Action`]s. Every action
//! carries an `id`, so the host's ledger can record dispatch and completion
//! under it and replay a result instead of repeating an effect. A [`Turn`]
//! carries the host's own id for the same reason, in the other direction: the
//! core refuses a turn it has already accepted, and names that id in the
//! [`Done`] that ends it.
//!
//! # Encoding
//!
//! [`encode`] and [`decode`] are the only two places that know the byte
//! format, so the format can change without moving a type or a C symbol.
//! Today it is `serde_json`.
//!
//! It becomes ZAP (HIP-0114) when the ZAP schema compiler is a build input
//! this repo has. The Rust side exists — `zap` 1.0.0 in `zap/rust/zap` is the
//! runtime and `zapc` 1.0.0 is the codegen plugin, both published from the
//! `zap-proto` GitHub org — but the runtime is a runtime for generated code,
//! and generating that code takes the `zap` schema compiler, a C++ binary
//! `zapc` shells out to that is not installed here and is not a build input of
//! this repo or its CI. Pointing the generator at `capnp` instead would be a
//! shim across a fork boundary, and hand-writing the format would be worse
//! than either. So the two functions below stay serde until the compiler is a
//! dependency, and nothing here imitates ZAP framing in the meantime.
//!
//! # Paths
//!
//! A path in this protocol is workspace-relative, and [`inside`] is the one
//! statement of what that means. The host joins a path onto the tree it
//! mounted for the session, so `/etc/passwd` names no workspace and
//! `../../etc/passwd` climbs out of the one it was given. The core refuses
//! such a call rather than emitting it, so no host re-derives that rule —
//! resolving it against a real tree, symlinks and all, is still the host's.
//!
//! Which effects may happen at all — which argv, which URL — is policy, and
//! policy belongs to `/v1/dev` outside the sandbox (HIP-1330 §Security). This
//! crate takes no position on it.

use serde::Deserialize;
use serde::Serialize;
use serde::de::DeserializeOwned;

/// Version of the byte format and of the C ABI that carries it.
pub const ABI: u32 = 1;

/// A payload this protocol cannot carry, with the reason.
#[derive(Debug)]
pub struct Malformed(String);

impl Malformed {
    fn new(reason: impl core::fmt::Display) -> Self {
        Self(reason.to_string())
    }
}

impl core::fmt::Display for Malformed {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "malformed payload: {}", self.0)
    }
}

impl std::error::Error for Malformed {}

/// Encode a payload.
pub fn encode<T: Serialize>(value: &T) -> Result<Vec<u8>, Malformed> {
    serde_json::to_vec(value).map_err(Malformed::new)
}

/// Decode a payload.
pub fn decode<T: DeserializeOwned>(bytes: &[u8]) -> Result<T, Malformed> {
    serde_json::from_slice(bytes).map_err(Malformed::new)
}

/// Pack a payload that will be stored and handed back later: the ABI it was
/// written under, a checksum of the body, then the body.
///
/// The checksum is not a signature, and nothing here is a place to put one:
/// this crate holds no key and knows no identity. It catches a truncated,
/// corrupted, hand-edited or foreign snapshot, which is what a store gets
/// wrong. Whose snapshot this is, and whether the sequence it restores agrees
/// with the ledger, is the host's: IAM is the only identity (HIP-0026) and the
/// journal — not the snapshot — is the record of which ids were dispatched
/// (HIP-1330 §A session is three facts).
pub fn pack<T: Serialize>(value: &T) -> Result<Vec<u8>, Malformed> {
    let body = encode(value)?;
    let mut bytes = Vec::with_capacity(body.len() + STAMP);
    bytes.extend_from_slice(&ABI.to_le_bytes());
    bytes.extend_from_slice(&sum(&body).to_le_bytes());
    bytes.extend_from_slice(&body);
    Ok(bytes)
}

/// Open a packed payload, refusing one written under another ABI and one whose
/// bytes no longer match their checksum.
pub fn unpack<T: DeserializeOwned>(bytes: &[u8]) -> Result<T, Malformed> {
    let (stamp, body) = bytes
        .split_at_checked(STAMP)
        .ok_or_else(|| Malformed::new("shorter than its own stamp"))?;
    let (abi, checksum) = stamp.split_at(size_of::<u32>());
    let abi = u32::from_le_bytes(abi.try_into().map_err(Malformed::new)?);
    if abi != ABI {
        return Err(Malformed::new(format!(
            "written under ABI {abi}, not {ABI}"
        )));
    }
    let checksum = u64::from_le_bytes(checksum.try_into().map_err(Malformed::new)?);
    if checksum != sum(body) {
        return Err(Malformed::new("checksum does not match the body"));
    }
    decode(body)
}

/// Bytes of ABI and checksum in front of a packed body.
const STAMP: usize = size_of::<u32>() + size_of::<u64>();

/// FNV-1a, 64-bit: enough to catch a store that lost or changed bytes, and
/// deliberately not enough to pass for authentication.
fn sum(bytes: &[u8]) -> u64 {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    hash
}

/// Whether a path a tool call names stays inside the workspace.
///
/// Absolute is out, whatever spelling: a leading separator, a drive letter, a
/// UNC prefix. Climbing out is out: the parts are counted, and a `..` that
/// would pass the root refuses the path. An interior NUL is out, because the
/// path crosses a C ABI and would be cut there. What is left is a relative
/// path the host can join onto the workspace it mounted.
///
/// This is lexical, and it is the whole of what a crate with no filesystem can
/// say. A symlink inside the workspace that points out of it resolves outside
/// it, and only the host holds the tree to see that: confinement at resolution
/// is the host's half, and this is the half that keeps `/etc/passwd` from ever
/// being asked for.
pub fn inside(path: &str) -> bool {
    if path.is_empty() || path.contains('\0') {
        return false;
    }
    if path.starts_with('/') || path.starts_with('\\') {
        return false;
    }
    let bytes = path.as_bytes();
    if bytes.len() >= 2 && bytes[0].is_ascii_alphabetic() && bytes[1] == b':' {
        return false;
    }
    let mut depth: usize = 0;
    for part in path.split(['/', '\\']) {
        match part {
            "" | "." => {}
            ".." => match depth.checked_sub(1) {
                Some(up) => depth = up,
                None => return false,
            },
            _ => depth += 1,
        }
    }
    true
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
    Done(Done),
}

/// A prompt, under the id the host's journal gave the request.
///
/// Ids rise and are never zero. The core keeps the highest it has accepted and
/// refuses anything at or below it, so a redelivered turn produces no action
/// and runs the prompt once.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Turn {
    pub id: u64,
    pub prompt: String,
}

/// The end of the turn `turn` asked for.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Done {
    pub turn: u64,
    pub outcome: Outcome,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Ask {
    pub model: Option<String>,
    /// The session's standing instructions, ahead of the conversation.
    pub prelude: Option<String>,
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

/// A diff, and the one file it edits.
///
/// The paths inside a diff are visible only to the parser that applies it, so
/// the action names the file instead: the core confines `path`, and the host
/// applies `diff` to that file and nothing else.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Patch {
    pub path: String,
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
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
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

impl Call {
    /// The path this call would reach outside the workspace, if it names one.
    ///
    /// A `Git` argv and a `Browse` url are not paths and are not judged here:
    /// what may be run and what may be fetched is policy, and policy is
    /// `/v1/dev`'s.
    pub fn escape(&self) -> Option<&str> {
        let path = match self {
            Self::Read(read) => Some(read.path.as_str()),
            Self::Write(write) => Some(write.path.as_str()),
            Self::Patch(patch) => Some(patch.path.as_str()),
            Self::Exec(exec) => exec.cwd.as_deref(),
            Self::Git(_) | Self::Browse(_) => None,
        };
        path.filter(|path| !inside(path))
    }
}

/// A call the core sent out, under the id its result will name.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Dispatch {
    pub id: u64,
    pub call: Call,
}

/// The result of an effect, naming the action it answers.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Output {
    pub id: u64,
    pub text: String,
    pub failed: bool,
}

/// One entry of the conversation the core carries.
///
/// An agent entry carries the calls it asked for, each under the id it was
/// dispatched with, and a tool entry names the id it answers. So a provider
/// that requires the assistant's tool call ahead of the tool result can be
/// driven straight from this: the pair is in the transcript, not inferred.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Message {
    /// A person asked for work.
    User(String),
    /// The agent spoke, asked for tools, or both.
    Agent {
        text: Option<String>,
        calls: Vec<Dispatch>,
    },
    /// A tool answered the call dispatched under `id`.
    Tool { id: u64, text: String, failed: bool },
}

/// What a session is born with.
#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct Config {
    /// Model the core names in every [`Op::Model`]; the host routes it.
    pub model: Option<String>,
    /// Standing instructions every ask carries, if the host wants some.
    pub prelude: Option<String>,
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn event_round_trips() {
        let event = Event::Turn(Turn {
            id: 1,
            prompt: "fix the build".to_string(),
        });
        let back: Event = decode(&encode(&event).expect("encode")).expect("decode");
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
        let back: Action = decode(&encode(&action).expect("encode")).expect("decode");
        assert_eq!(back.id, 7821);
        assert_eq!(action, back);
    }

    #[test]
    fn garbage_is_refused_with_a_reason() {
        let err = decode::<Event>(b"not a payload").expect_err("garbage decoded");
        assert!(err.to_string().starts_with("malformed payload:"));
    }

    #[test]
    fn a_packed_payload_survives_the_trip_and_an_edited_one_does_not() {
        let turn = Turn {
            id: 9,
            prompt: "ship it".to_string(),
        };
        let packed = pack(&turn).expect("pack");
        assert_eq!(unpack::<Turn>(&packed).expect("unpack"), turn);

        for cut in 0..packed.len() {
            assert!(unpack::<Turn>(&packed[..cut]).is_err(), "{cut} bytes");
        }

        let mut edited = packed.clone();
        let last = edited.len() - 1;
        edited[last] ^= 0x20;
        let err = unpack::<Turn>(&edited).expect_err("an edited body was opened");
        assert!(err.to_string().contains("checksum"), "{err}");

        let mut older = packed;
        older[0] = older[0].wrapping_sub(1);
        let err = unpack::<Turn>(&older).expect_err("a foreign ABI was opened");
        assert!(err.to_string().contains("ABI"), "{err}");
    }

    #[test]
    fn a_path_is_inside_only_when_it_is_relative_and_stays_down() {
        for path in ["src/lib.rs", "a/../b", "./x", "deep/./er/../er/x"] {
            assert!(inside(path), "{path}");
        }
        for path in [
            "",
            "/etc/passwd",
            "..",
            "../x",
            "a/../../x",
            "C:\\Windows",
            "\\\\host\\share",
            "\\etc",
            "a\0b",
        ] {
            assert!(!inside(path), "{path}");
        }
    }

    #[test]
    fn a_call_names_the_path_it_would_escape_with() {
        let read = Call::Read(Read {
            path: "../../etc/passwd".to_string(),
        });
        assert_eq!(read.escape(), Some("../../etc/passwd"));
        let write = Call::Write(Write {
            path: "src/lib.rs".to_string(),
            text: String::new(),
        });
        assert_eq!(write.escape(), None);
        let patch = Call::Patch(Patch {
            path: "/etc/hosts".to_string(),
            diff: String::new(),
        });
        assert_eq!(patch.escape(), Some("/etc/hosts"));
        let exec = Call::Exec(Exec {
            argv: vec!["ls".to_string()],
            cwd: Some("/".to_string()),
        });
        assert_eq!(exec.escape(), Some("/"));
        let anywhere = Call::Exec(Exec {
            argv: vec!["ls".to_string()],
            cwd: None,
        });
        assert_eq!(anywhere.escape(), None);
    }
}
