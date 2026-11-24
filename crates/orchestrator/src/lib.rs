//! Pure decide functions of the Dev orchestrator, ported one slice at a time from
//! `apps/server/src/orchestration-v2`. Each slice is gated on byte-identical replay of the
//! corpus under `corpus/`, recorded from the TS functions on real event streams.

pub mod command_policy;
pub mod dispatch_mode_limit;
pub mod guest_share;
pub mod model_switch;
pub mod queued_run;
pub mod thread_triage;

use serde_json::value::RawValue;

/// Applies the decide function `name` to a JSON input and returns its JSON output, spelled as
/// the TS function's `JSON.stringify` spells it (`null` for "nothing"). `None` for an unknown name.
pub fn apply(name: &str, input: &RawValue) -> Option<Result<String, serde_json::Error>> {
    if name == "modelSwitch" {
        return Some(model_switch::apply(input));
    }
    guest_share::apply(name, input)
        .or_else(|| command_policy::apply(name, input))
        .or_else(|| dispatch_mode_limit::apply(name, input))
        .or_else(|| queued_run::apply(name, input))
        .or_else(|| thread_triage::apply(name, input))
}
