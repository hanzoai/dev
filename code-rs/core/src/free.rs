//! Working without an account.
//!
//! The Hanzo Cloud answers `dev` with no login, on the free pool. A key built
//! into this binary asks for the `free` model; the gateway holds that key in an
//! allowlist, caps what a single request may spend, and bills nothing for it.
//! That is what makes a credential safe to ship inside a binary — it cannot
//! spend an account, because it has none.
//!
//! Free costs nothing and in exchange it is data-shared, which [`NOTICE`] says
//! at the top of every run that uses it. Signing in swaps both halves at once:
//! the user's own key, and the coding model it pays for.

/// The credential the gateway answers to without an account. Public by design,
/// so it is declared here rather than fetched — a value every copy of this
/// binary already carries gains nothing from a lookup. `HANZO_FREE_KEY`
/// overrides it against a private deployment.
const FREE_KEY: &str = "hz_7GrX4ZKTMlExtziPF4EZtPA9QfdsxFBkr1AXEB3COo8XVQ9R";

/// The gateway's free pool: one id that always answers free, whichever route is
/// carrying it. Never a vendor `:free` id — those come and go underneath this
/// name, and this name outlives them.
pub const MODEL: &str = "free";

/// The bearer a signed-in user brings. `hanzo code` sets it on the child, and
/// `hanzo auth login` is how a human fills it in.
pub const USER_KEY: &str = "HANZO_USER_KEY";

fn env(name: &str) -> Option<String> {
    std::env::var(name)
        .ok()
        .filter(|v| !v.trim().is_empty())
        .map(|v| v.trim().to_string())
}

/// Whether an account is behind this run.
pub fn signed_in() -> bool {
    env(USER_KEY).is_some()
}

/// The free credential, when there is one to use.
pub fn key() -> Option<String> {
    env("HANZO_FREE_KEY").or_else(|| (!FREE_KEY.is_empty()).then(|| FREE_KEY.to_string()))
}

/// The account always wins. Free answers only when there is no account to bill
/// and a credential to ask with — no credential means the run asks for nothing,
/// which is how a build with no free key behaves exactly as it did before.
fn free_answers(user: Option<&str>, free: Option<&str>) -> bool {
    user.is_none() && free.is_some()
}

/// Whether this run will be answered free.
pub fn anonymous() -> bool {
    free_answers(env(USER_KEY).as_deref(), key().as_deref())
}

/// Said once at the top of a free run — what it costs, what it shares, and how
/// to leave it. The same bargain hanzo.ai and hanzo.chat state in their own
/// words.
pub const NOTICE: &str = "Running free, without an account. Free is data-shared: what you send and what comes back may be used to improve models — ours, and the provider that serves them. Paid stays private. Sign in with `hanzo auth login` for the coding models and private requests. Terms: https://hanzo.ai/terms";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_account_outranks_the_free_credential() {
        assert!(!free_answers(Some("hk-real"), Some("hz_free")));
        assert!(!free_answers(Some("hk-real"), None));
    }

    #[test]
    fn free_answers_only_when_it_can() {
        assert!(free_answers(None, Some("hz_free")));
        assert!(!free_answers(None, None));
    }

    /// The pool's own id outlives the routes under it. A vendor `:free` id names
    /// one route, and naming it here would send this binary to a model that can
    /// be retired without warning.
    #[test]
    fn the_free_id_is_the_pool_not_a_route() {
        assert_eq!(MODEL, "free");
        assert!(!MODEL.contains(':') && !MODEL.contains('/'));
    }

    /// A free run is data-shared, so the notice has to say so and say where the
    /// terms are.
    #[test]
    fn the_notice_states_the_bargain() {
        assert!(NOTICE.contains("data-shared"));
        assert!(NOTICE.contains("https://hanzo.ai/terms"));
        assert!(NOTICE.contains("hanzo auth login"));
    }
}
