//! The persona a session speaks as.
//!
//! A persona is a name — `feynman`, `hopper` — and the text behind the name
//! lives in cloud: the catalogue compiled into api.hanzo.ai, plus whatever the
//! account has saved of its own. The composed system turn is asked for by name
//! and never assembled here. Building it out of a persona's fields would put a
//! second copy of the composition in every client, free to drift, and the
//! fields a given client happened to know about would quietly become the whole
//! persona.
//!
//! WHERE IT LANDS. In `user_instructions`, which `project_doc` already treats
//! as the text that precedes AGENTS.md, so the persona speaks first and the
//! project's own instructions follow it. Not in `base_instructions`: that slot
//! replaces the model family's coding harness wholesale, and a persona is a
//! voice, not a replacement for the agent's tool instructions.
//!
//! WHY IT IS FATAL. A named persona is an explicit instruction. An agent that
//! silently isn't Feynman looks like a model ignoring its prompt, and there is
//! nothing on screen to debug. So an unresolvable persona stops the program
//! before the first frame, with cloud's own sentence about why.

use std::time::Duration;

use anyhow::Context as _;
use anyhow::Result;
use anyhow::anyhow;
use code_app_server_protocol::AuthMode;
use reqwest::Method;
use reqwest::Url;
use serde::Deserialize;

use crate::CodexAuth;
use crate::auth::AuthManager;
use crate::config::Config;
use crate::default_client::create_client;
use crate::model_provider_info::ModelProviderInfo;
use crate::model_provider_info::create_hanzo_provider;

/// Long enough for a cold start, short enough that an unreachable cloud is an
/// error at launch rather than a terminal that never paints.
const TIMEOUT: Duration = Duration::from_secs(10);

/// Speak as `config.persona`, if one was named. A configuration without a
/// persona is left exactly as it was.
pub async fn adopt(config: &mut Config) -> Result<()> {
    let Some(name) = config.persona.clone() else {
        return Ok(());
    };
    let name = name.trim();
    if name.is_empty() {
        return Ok(());
    }

    // Always the Hanzo provider, never the session's: a run pointed at a local
    // Ollama still reads its persona from the account that holds it.
    let provider = create_hanzo_provider();
    let auth = AuthManager::shared_with_mode_and_originator(
        config.code_home.clone(),
        AuthMode::ApiKey,
        config.responses_originator_header.clone(),
    )
    .auth();
    let client = create_client(&config.responses_originator_header);

    let system = system(&client, &provider, &auth, name)
        .await
        .with_context(|| format!("persona `{name}`"))?;

    config.user_instructions = Some(ahead_of(&system, config.user_instructions.take()));
    Ok(())
}

/// Ask cloud for the composed system turn.
async fn system(
    client: &reqwest::Client,
    provider: &ModelProviderInfo,
    auth: &Option<CodexAuth>,
    name: &str,
) -> Result<String> {
    let base = provider
        .base_url
        .as_deref()
        .ok_or_else(|| anyhow!("the Hanzo provider has no base URL"))?;

    let response = provider
        .create_request_builder_for_url(client, auth, Method::GET, url(base, name)?)
        .await
        .context("sign in with `hanzo auth login`")?
        .timeout(TIMEOUT)
        .send()
        .await
        .context("could not reach the persona service")?;

    let status = response.status();
    let body = response.text().await.unwrap_or_default();
    if !status.is_success() && body.trim().is_empty() {
        return Err(anyhow!("the persona service answered {status}"));
    }
    read(&body)
}

/// `{base}/persona/system?name=…`. The Hanzo base URL already ends in `/v1`.
fn url(base: &str, name: &str) -> Result<Url> {
    let mut url = Url::parse(base).with_context(|| format!("invalid base URL {base}"))?;
    let path = url.path().trim_end_matches('/').to_string();
    url.set_path(&format!("{path}/persona/system"));
    url.query_pairs_mut().append_pair("name", name);
    Ok(url)
}

/// The `/v1` envelope. HTTP 200 only says the request arrived; the envelope's
/// own `status` says whether it worked. A client that reads `data` without
/// reading `status` splices "The object does not exist" into the agent's
/// instructions and calls it a persona.
#[derive(Deserialize)]
struct Envelope {
    status: String,
    #[serde(default)]
    msg: String,
    #[serde(default)]
    data: serde_json::Value,
}

fn read(body: &str) -> Result<String> {
    let envelope: Envelope =
        serde_json::from_str(body).context("the persona service answered something unreadable")?;
    if envelope.status != "ok" {
        let msg = envelope.msg.trim();
        return Err(anyhow!(
            "{}",
            if msg.is_empty() { "refused" } else { msg }
        ));
    }
    let system = envelope.data.as_str().unwrap_or_default().trim();
    if system.is_empty() {
        return Err(anyhow!("no system turn"));
    }
    Ok(system.to_string())
}

/// The persona speaks first, and whatever instructions the user already had
/// follow it.
fn ahead_of(system: &str, instructions: Option<String>) -> String {
    match instructions {
        Some(existing) if !existing.trim().is_empty() => format!("{system}\n\n{existing}"),
        _ => system.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use wiremock::Mock;
    use wiremock::MockServer;
    use wiremock::Request;
    use wiremock::ResponseTemplate;
    use wiremock::matchers::method;
    use wiremock::matchers::path;
    use wiremock::matchers::query_param;

    #[test]
    fn the_url_does_not_repeat_the_api_version() {
        let url = url("https://api.hanzo.ai/v1", "feynman").unwrap();
        assert_eq!(
            url.as_str(),
            "https://api.hanzo.ai/v1/persona/system?name=feynman"
        );
    }

    #[test]
    fn a_refusal_never_becomes_a_prompt() {
        // The failure that matters: HTTP 200 carrying a denial.
        let err = read(&json!({ "status": "error", "msg": "Please sign in first" }).to_string())
            .unwrap_err()
            .to_string();
        assert_eq!(err, "Please sign in first");

        assert!(read(&json!({ "status": "ok", "data": "" }).to_string()).is_err());
        assert!(read(&json!({ "status": "ok", "data": null }).to_string()).is_err());
        assert!(read("<html>502</html>").is_err());
    }

    #[test]
    fn the_system_turn_is_taken_verbatim() {
        let body = json!({ "status": "ok", "msg": "", "data": "You are Richard Feynman." });
        assert_eq!(read(&body.to_string()).unwrap(), "You are Richard Feynman.");
    }

    #[test]
    fn the_persona_speaks_before_the_users_own_instructions() {
        assert_eq!(
            ahead_of("You are Grace Hopper.", Some("Be terse.".to_string())),
            "You are Grace Hopper.\n\nBe terse."
        );
        assert_eq!(ahead_of("You are Grace Hopper.", None), "You are Grace Hopper.");
        assert_eq!(
            ahead_of("You are Grace Hopper.", Some("   ".to_string())),
            "You are Grace Hopper."
        );
    }

    /// One authenticated GET, by name, and the answer used as written.
    #[tokio::test]
    async fn the_system_turn_comes_from_cloud() {
        let cloud = MockServer::start().await;

        Mock::given(method("GET"))
            .and(path("/v1/persona/system"))
            .and(query_param("name", "feynman"))
            .and(|req: &Request| req.headers.contains_key("authorization"))
            .respond_with(ResponseTemplate::new(200).set_body_json(json!({
                "status": "ok", "msg": "", "data": "You are Richard Feynman."
            })))
            .expect(1)
            .mount(&cloud)
            .await;

        let mut provider = create_hanzo_provider();
        provider.base_url = Some(format!("{}/v1", cloud.uri()));
        let auth = Some(CodexAuth::from_api_key("hk-secret-value"));

        let system = system(&create_client("test"), &provider, &auth, "feynman")
            .await
            .unwrap();
        assert_eq!(system, "You are Richard Feynman.");
    }

    #[tokio::test]
    async fn an_unknown_persona_is_an_error_not_a_prompt() {
        let cloud = MockServer::start().await;

        Mock::given(method("GET"))
            .and(path("/v1/persona/system"))
            .respond_with(ResponseTemplate::new(200).set_body_json(json!({
                "status": "error", "msg": "The object does not exist", "data": null
            })))
            .mount(&cloud)
            .await;

        let mut provider = create_hanzo_provider();
        provider.base_url = Some(format!("{}/v1", cloud.uri()));
        let auth = Some(CodexAuth::from_api_key("hk-secret-value"));

        let err = system(&create_client("test"), &provider, &auth, "nobody")
            .await
            .unwrap_err()
            .to_string();
        assert_eq!(err, "The object does not exist");
    }
}
