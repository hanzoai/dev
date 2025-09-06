use chrono::Local;
use chrono::Timelike;

/// Build a time-aware placeholder like
/// "What can I code for you this morning?".
pub(crate) fn greeting_placeholder() -> String {
    let hour = Local::now().hour();
    let when = if (5..=11).contains(&hour) {
        "this morning"
    } else if (12..=16).contains(&hour) {
        "this afternoon"
    } else if (17..=20).contains(&hour) {
        "this evening"
    } else {
        // Late night and very early hours
        "tonight"
    };
    format!("What can I code for you {when}?")
}

