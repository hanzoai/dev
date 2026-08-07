//! A picture of the UI the agent just changed, published beside the diff that
//! changed it.
//!
//! WHY. A diff says what moved; it does not say what it looks like. For a change
//! to a rendered surface the page itself is the reviewable artifact, and a human
//! watching a session live should not have to check out the branch and start a
//! server to see one. The session log already streams to that console, so the
//! picture rides the channel that exists — one ordinary event naming one object
//! in storage — rather than standing up a second artifact plane beside it. The
//! bytes go to object storage because a log is for reading, and a 200 KiB image
//! inlined into a transcript makes it unreadable for everything else.
//!
//! WHY IT MAY FAIL QUIETLY. Evidence is not the work. No dev server listening, a
//! route that 404s, storage unreachable, no session to publish to — none of those
//! are reasons to fail a coding turn, and none of them are worth more than a
//! debug line. Every step below returns rather than propagates, and the whole
//! thing runs off the turn's critical path so a slow browser costs the agent
//! nothing.
//!
//! WHY IT NEVER STARTS A SERVER. It attaches to a dev server already listening
//! and will not launch one. Launching one means running the `dev` script out of
//! the repository's own package.json — arbitrary code from the tree the agent is
//! being asked to edit — outside the approval and sandbox path every other
//! command in this program goes through. A repository shipping
//! `"dev": "curl … | sh"` would otherwise get execution for free by baiting the
//! agent into touching a stylesheet. Taking a picture must not be a way to run
//! something, so this reads package.json for a port and dials it.

mod ui;

use std::path::PathBuf;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use anyhow::Context as _;
use anyhow::Result;
use anyhow::anyhow;
use code_browser::BrowserConfig;
use code_browser::BrowserManager;
use code_browser::ScreenshotMode;
use code_browser::ViewportConfig;
use serde::Deserialize;
use serde_json::Value;
use serde_json::json;
use tokio::net::TcpStream;
use tokio::sync::Semaphore;
use tokio::time::Duration;
use tokio::time::timeout;
use tracing::debug;

use crate::codex::Session;
use crate::default_client::create_client;
use crate::model_provider_info::create_hanzo_provider;

/// The org-scoped bucket session artifacts live in. The org never appears in the
/// key — cloud derives the physical bucket from the caller's own principal, so a
/// key is meaningless outside the tenant that wrote it.
const BUCKET: &str = "sessions";

/// The session to publish to, set by whatever created it. The agent cannot
/// invent one: without a session there is nowhere for a picture to go, so the
/// whole gesture is abandoned before a browser or a socket is touched. This is
/// also why a laptop with no cloud session behaves exactly as it did before.
const SESSION: &str = "HANZO_SESSION";

/// One fixed viewport, so two pictures of the same page are comparable. Not
/// configurable — a reviewer flipping between runs taken at different widths is
/// reading noise, not a change.
const VIEW: (u32, u32) = (1280, 800);

/// The budget for the whole gesture: launch, navigate, capture, two round trips.
/// Past this the picture describes a page that has already moved on.
const BUDGET: Duration = Duration::from_secs(45);

/// How long to wait on the dev server's socket. It is on loopback; either it is
/// listening or it is not.
const DIAL: Duration = Duration::from_millis(300);

/// At most one capture in flight. A turn that patches thirty files wants one
/// picture of where it ends up, not thirty of it arriving there — and an
/// unbounded fan-out of browsers driven by model output is a way to run a
/// machine out of memory.
static INFLIGHT: Semaphore = Semaphore::const_new(1);

/// Numbers the objects within a session so the console can order them.
static COUNT: AtomicU64 = AtomicU64::new(0);

/// Publish a picture of the page this diff changed, if it changed one.
///
/// Returns immediately. The decision is cheap and made here; everything with a
/// cost happens on a detached task, because the turn must not wait on evidence.
pub(crate) fn capture(sess: &Session, diff: &str) {
    let Some(plan) = Plan::new(sess, diff) else {
        return;
    };
    tokio::spawn(async move {
        let Ok(_held) = INFLIGHT.try_acquire() else {
            debug!("shot: a capture is already running");
            return;
        };
        match timeout(BUDGET, plan.run()).await {
            Ok(Ok(key)) => debug!("shot: published {key}"),
            Ok(Err(why)) => debug!("shot: skipped — {why:#}"),
            Err(_) => debug!("shot: skipped — over {BUDGET:?}"),
        }
    });
}

/// Everything the capture needs, owned, so it can outlive the turn that asked
/// for it.
struct Plan {
    cwd: PathBuf,
    session: String,
    base: String,
    token: String,
    route: String,
    /// Workspace-relative, capped. Relative because an absolute path carries the
    /// operator's home directory — and so their account name — into a log the
    /// whole org can read.
    files: Vec<String>,
    slug: String,
    n: u64,
}

/// How many changed files the payload names. Enough to see what the picture is
/// of; the diff beside it is the complete list.
const NAMED: usize = 10;

impl Plan {
    fn new(sess: &Session, diff: &str) -> Option<Self> {
        // Cheapest question first: is anyone listening for this?
        let session = std::env::var(SESSION).ok()?;
        let session = handle(&session)?;

        let changed: Vec<PathBuf> = ui::touched(diff)
            .into_iter()
            .filter(|p| ui::rendered(p))
            .collect();
        let first = changed.first()?;

        // The same base URL and bearer the agent already talks to Hanzo with.
        // A second notion of where the cloud is would be a second thing to get
        // wrong.
        let provider = create_hanzo_provider();
        let base = provider.base_url.clone()?;
        let token = provider.api_key().ok().flatten()?;

        let cwd = sess.get_cwd().to_path_buf();
        let route = changed
            .iter()
            .find_map(|p| ui::route(p))
            .unwrap_or_else(|| "/".to_string());
        let slug = ui::slug(&route, first);
        let files = changed
            .iter()
            .take(NAMED)
            .map(|p| {
                p.strip_prefix(&cwd)
                    .unwrap_or(p)
                    .to_string_lossy()
                    .into_owned()
            })
            .collect();

        Some(Self {
            cwd,
            session: session.to_string(),
            base: base.trim_end_matches('/').to_string(),
            token,
            route,
            files,
            slug,
            n: COUNT.fetch_add(1, Ordering::Relaxed) + 1,
        })
    }

    async fn run(&self) -> Result<String> {
        let origin = self
            .origin()
            .await
            .ok_or_else(|| anyhow!("no dev server listening"))?;
        let png = shoot(&format!("{origin}{}", self.route)).await?;
        let http = create_client("shot");
        let key = format!("{}/{}/{}-{}.png", BUCKET, self.session, self.n, self.slug);
        let signed = self.presign(&http, &key).await?;
        put(&http, &signed, png).await?;
        self.append(&http, &key, &origin).await?;
        Ok(key)
    }

    /// The dev server, if one is already listening.
    ///
    /// Exactly one port is dialed: the one this repository declares. Sweeping the
    /// usual suspects would eventually connect to some unrelated program on the
    /// operator's machine and present a picture of it as their diff.
    async fn origin(&self) -> Option<String> {
        let manifest = std::fs::read_to_string(self.cwd.join("package.json")).ok()?;
        let port = ui::port(&manifest)?;
        let addr = format!("127.0.0.1:{port}");
        timeout(DIAL, TcpStream::connect(&addr)).await.ok()?.ok()?;
        Some(format!("http://{addr}"))
    }

    /// Ask cloud for a URL that may write this one object.
    ///
    /// The image never passes through the API: cloud signs, we upload straight to
    /// storage. The signature is time-boxed and scoped to this exact bucket and
    /// key, which is why it is a credential and why it stops here.
    async fn presign(&self, http: &reqwest::Client, key: &str) -> Result<String> {
        #[derive(Deserialize)]
        struct Signed {
            url: String,
        }
        let res = http
            .post(format!("{}/s3/buckets/{BUCKET}/objects", self.base))
            .bearer_auth(&self.token)
            .json(&json!({ "key": key }))
            .send()
            .await
            .context("presign")?;
        let status = res.status();
        if !status.is_success() {
            return Err(anyhow!("presign: {status}"));
        }
        Ok(res.json::<Signed>().await.context("presign body")?.url)
    }

    /// One event, naming the object. The bytes are in storage; the log carries
    /// the pointer, the route, and the files that earned the picture.
    async fn append(&self, http: &reqwest::Client, key: &str, origin: &str) -> Result<()> {
        let res = http
            .post(format!("{}/agents/sessions/{}/events", self.base, self.session))
            .bearer_auth(&self.token)
            .json(&json!({ "kind": "tool-call", "payload": self.payload(key, origin) }))
            .send()
            .await
            .context("append")?;
        let status = res.status();
        if !status.is_success() {
            return Err(anyhow!("append: {status}"));
        }
        Ok(())
    }

    /// What the console reads.
    ///
    /// Bucket and key rather than a URL: the only URL for a private object is a
    /// signed, expiring one, and writing a credential into a transcript would be
    /// a leak that outlives the session. A reader resolves the key through the
    /// same door, under their own identity, whenever they open the event.
    fn payload(&self, key: &str, origin: &str) -> Value {
        json!({
            "tool": "shot",
            "route": self.route,
            "bucket": BUCKET,
            "key": key,
            "origin": origin,
            "viewport": { "width": VIEW.0, "height": VIEW.1 },
            "files": self.files,
        })
    }
}

/// A session id fit to be spliced into a URL path and an object key. An
/// environment variable is caller-controlled input, and this one addresses
/// another tenant's session if it is allowed to contain a slash or a dot-dot.
fn handle(raw: &str) -> Option<&str> {
    let id = raw.trim();
    let ok = !id.is_empty()
        && id.len() <= 128
        && id
            .bytes()
            .all(|b| b.is_ascii_alphanumeric() || b == b'-' || b == b'_');
    ok.then_some(id)
}

/// Photograph a URL in a browser of this function's own.
///
/// Its own, not the agent's: the `browser` tool's page belongs to the
/// conversation, and navigating it away to take this picture would move the
/// ground under whatever the agent was looking at. Headless with a temporary
/// profile, so it carries none of the operator's cookies to a page it was told
/// about by a model.
async fn shoot(url: &str) -> Result<Vec<u8>> {
    let browser = BrowserManager::new(BrowserConfig {
        enabled: true,
        headless: true,
        persist_profile: false,
        viewport: ViewportConfig {
            width: VIEW.0,
            height: VIEW.1,
            device_scale_factor: 1.0,
            mobile: false,
        },
        ..BrowserConfig::default()
    });
    let png = frame(&browser, url).await;
    let _ = browser.stop().await;
    png
}

async fn frame(browser: &BrowserManager, url: &str) -> Result<Vec<u8>> {
    browser.start().await.context("launch")?;
    browser.goto(url).await.with_context(|| format!("open {url}"))?;
    let page = browser.get_or_create_page().await.context("page")?;
    let shots = page
        .screenshot(ScreenshotMode::Viewport)
        .await
        .context("capture")?;
    shots
        .into_iter()
        .next()
        .map(|s| s.data)
        .ok_or_else(|| anyhow!("empty capture"))
}

/// Upload the bytes with the signature and nothing else. Our bearer is for the
/// API; sending it to whatever host cloud signed against would hand an org
/// credential to a third party for no reason.
async fn put(http: &reqwest::Client, signed: &str, png: Vec<u8>) -> Result<()> {
    let res = http
        .put(signed)
        .header(reqwest::header::CONTENT_TYPE, "image/png")
        .body(png)
        .send()
        .await
        .context("upload")?;
    let status = res.status();
    if !status.is_success() {
        return Err(anyhow!("upload: {status}"));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn plan() -> Plan {
        Plan {
            cwd: PathBuf::from("/home/someone/work/web"),
            session: "ses_abc123".to_string(),
            base: "https://api.hanzo.ai/v1".to_string(),
            token: "hk-secret-value".to_string(),
            route: "/settings".to_string(),
            files: vec!["src/app/settings/page.tsx".to_string()],
            slug: "settings".to_string(),
            n: 7,
        }
    }

    #[test]
    fn the_payload_names_the_object_and_what_earned_it() {
        let p = plan();
        let key = format!("{}/{}/{}-{}.png", BUCKET, p.session, p.n, p.slug);
        let body = p.payload(&key, "http://127.0.0.1:3000");
        assert_eq!(body["tool"], "shot");
        assert_eq!(body["route"], "/settings");
        assert_eq!(body["bucket"], "sessions");
        assert_eq!(body["key"], "sessions/ses_abc123/7-settings.png");
        assert_eq!(body["origin"], "http://127.0.0.1:3000");
        assert_eq!(body["viewport"]["width"], 1280);
        assert_eq!(body["viewport"]["height"], 800);
        assert_eq!(body["files"][0], "src/app/settings/page.tsx");
    }

    #[test]
    fn the_payload_carries_no_credential() {
        let p = plan();
        let body = p.payload("sessions/ses_abc123/7-settings.png", "http://127.0.0.1:3000").to_string();
        assert!(!body.contains(&p.token));
        assert!(!body.contains("hk-"));
        // A presigned URL is a bearer with an expiry. It must never be written down.
        assert!(!body.contains("X-Amz-Signature"));
    }

    #[test]
    fn the_payload_names_no_absolute_path() {
        let body = plan()
            .payload("sessions/ses_abc123/7-settings.png", "http://127.0.0.1:3000")
            .to_string();
        assert!(!body.contains("/home/someone"));
    }

    #[test]
    fn the_payload_fits_the_log() {
        // Cloud refuses a payload over 64 KiB. The cap on named files is what
        // keeps this true for a turn that rewrites a design system.
        let mut p = plan();
        p.files = (0..NAMED)
            .map(|i| format!("src/app/{}/page.tsx", "n".repeat(200 + i)))
            .collect();
        let body = p.payload("sessions/ses_abc123/7-settings.png", "http://127.0.0.1:3000");
        assert!(body.to_string().len() < 64 * 1024);
    }

    #[test]
    fn a_session_id_cannot_retarget_the_request() {
        for bad in [
            "",
            "   ",
            "../../admin",
            "ses_abc/../../other",
            "ses abc",
            "ses_abc?x=1",
            "ses_abc#frag",
            "ses_abc/events",
            "https://evil.example/",
        ] {
            assert_eq!(handle(bad), None, "{bad} must be refused");
        }
        assert_eq!(handle(" ses_abc123 "), Some("ses_abc123"));
        assert_eq!(handle("ses-ABC_123"), Some("ses-ABC_123"));
        assert_eq!(handle(&"a".repeat(128)), Some("a".repeat(128).as_str()));
        assert_eq!(handle(&"a".repeat(129)), None);
    }
}
