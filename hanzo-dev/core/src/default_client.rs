use reqwest::header::HeaderValue;
use std::sync::LazyLock;
use std::sync::Mutex;

pub const DEFAULT_ORIGINATOR: &str = "code_cli_rs";

/// Optional suffix for the Codex User-Agent string.
///
/// This is primarily used by the MCP server implementation to include
/// client-provided identity in the UA. Because there is a single MCP server
/// per process, a global is acceptable here. Other callers should prefer
/// passing an explicit originator.
pub static USER_AGENT_SUFFIX: LazyLock<Mutex<Option<String>>> = LazyLock::new(|| Mutex::new(None));

pub fn get_code_user_agent(originator: Option<&str>) -> String {
    let build_version = hanzo_version::version();
    let os_info = os_info::get();
    let prefix = format!(
        "{}/{build_version} ({} {}; {}) {}",
        originator.unwrap_or(DEFAULT_ORIGINATOR),
        os_info.os_type(),
        os_info.version(),
        os_info.architecture().unwrap_or("unknown"),
        crate::terminal::user_agent()
    );
    let suffix = USER_AGENT_SUFFIX
        .lock()
        .ok()
        .and_then(|guard| guard.clone())
        .as_deref()
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .map_or_else(String::new, |v| format!(" ({v})"));

    let candidate = format!("{prefix}{suffix}");
    sanitize_user_agent(candidate, &prefix)
}

/// Convenience wrapper using the default originator.
pub fn get_code_user_agent_default() -> String {
    get_code_user_agent(None)
}

/// Replace invalid header characters with '_' and ensure the UA is syntactically valid.
fn sanitize_user_agent(candidate: String, fallback: &str) -> String {
    if HeaderValue::from_str(candidate.as_str()).is_ok() {
        return candidate;
    }
    let sanitized: String = candidate
        .chars()
        .map(|ch| if matches!(ch, ' '..='~') { ch } else { '_' })
        .collect();
    if !sanitized.is_empty() && HeaderValue::from_str(sanitized.as_str()).is_ok() {
        tracing::warn!(
            "Sanitized Codex user agent because provided suffix contained invalid header characters"
        );
        sanitized
    } else if HeaderValue::from_str(fallback).is_ok() {
        tracing::warn!(
            "Falling back to base Codex user agent because provided suffix could not be sanitized"
        );
        fallback.to_string()
    } else {
        tracing::warn!(
            "Falling back to default Codex originator because base user agent string is invalid"
        );
        DEFAULT_ORIGINATOR.to_string()
    }
}

/// Create a reqwest client with default `originator` and `User-Agent` headers set.
pub fn create_client(originator: &str) -> reqwest::Client {
    use reqwest::header::HeaderMap;
    use reqwest::header::HeaderValue;
    use std::panic::AssertUnwindSafe;

    let mut headers = HeaderMap::new();
    let originator_value = HeaderValue::from_str(originator)
        .unwrap_or_else(|_| HeaderValue::from_static(DEFAULT_ORIGINATOR));
    headers.insert("originator", originator_value);
    let ua = get_code_user_agent(Some(originator));

    let primary = std::panic::catch_unwind(AssertUnwindSafe(|| {
        build_client_with_headers(&ua, headers.clone(), false)
    }));
    match primary {
        Ok(Ok(client)) => client,
        Ok(Err(err)) => {
            tracing::warn!(
                "failed to create reqwest client with system proxy settings ({err}); retrying without proxy autodetection"
            );
            build_client_with_headers(&ua, headers, true).unwrap_or_else(|fallback_err| {
                tracing::warn!(
                    "failed to create reqwest client without system proxy settings ({fallback_err}); falling back to default reqwest client"
                );
                build_fallback_client()
            })
        }
        Err(_) => {
            tracing::warn!(
                "reqwest client creation panicked while resolving system proxy settings; retrying without proxy autodetection"
            );
            build_client_with_headers(&ua, headers, true).unwrap_or_else(|fallback_err| {
                tracing::warn!(
                    "failed to create reqwest client without system proxy settings ({fallback_err}); falling back to default reqwest client"
                );
                build_fallback_client()
            })
        }
    }
}

fn build_client_with_headers(
    user_agent: &str,
    headers: reqwest::header::HeaderMap,
    disable_system_proxy: bool,
) -> Result<reqwest::Client, reqwest::Error> {
    let mut builder = reqwest::Client::builder()
        // Set UA via dedicated helper to avoid header validation pitfalls.
        .user_agent(user_agent.to_string())
        .default_headers(headers);

    #[cfg(target_os = "macos")]
    {
        // Avoid reqwest's system proxy autodetection on macOS. In headless or
        // restricted runtime contexts this can panic inside system-configuration.
        builder = builder.no_proxy();
    }

    if disable_system_proxy {
        builder = builder.no_proxy();
    }

    builder.build()
}

fn build_fallback_client() -> reqwest::Client {
    let mut builder = reqwest::Client::builder();

    #[cfg(target_os = "macos")]
    {
        builder = builder.no_proxy();
    }

    builder.build().unwrap_or_else(|_| reqwest::Client::new())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_code_user_agent() {
        let user_agent = get_code_user_agent(None);
        assert!(user_agent.starts_with("code_cli_rs/"));
    }

    #[tokio::test]
    async fn test_create_client_sets_default_headers() {
        use wiremock::Mock;
        use wiremock::MockServer;
        use wiremock::ResponseTemplate;
        use wiremock::matchers::method;
        use wiremock::matchers::path;

        let originator = "test_originator";
        let client = create_client(originator);

        // Spin up a local mock server and capture a request.
        let server = MockServer::start().await;
        Mock::given(method("GET"))
            .and(path("/"))
            .respond_with(ResponseTemplate::new(200))
            .mount(&server)
            .await;

        let resp = client
            .get(server.uri())
            .send()
            .await
            .expect("failed to send request");
        assert!(resp.status().is_success());

        let requests = server
            .received_requests()
            .await
            .expect("failed to fetch received requests");
        assert!(!requests.is_empty());
        let headers = &requests[0].headers;

        // originator header is set to the provided value
        let originator_header = headers
            .get("originator")
            .expect("originator header missing");
        assert_eq!(originator_header.to_str().unwrap(), originator);

        // User-Agent matches the computed Codex UA for that originator
        let expected_ua = get_code_user_agent(Some(originator));
        let ua_header = headers
            .get("user-agent")
            .expect("user-agent header missing");
        assert_eq!(ua_header.to_str().unwrap(), expected_ua);
    }

    #[test]
    fn test_build_client_with_headers_no_proxy() {
        let mut headers = reqwest::header::HeaderMap::new();
        headers.insert(
            "originator",
            reqwest::header::HeaderValue::from_static("test_originator"),
        );

        let client = build_client_with_headers("test_agent/0.0.0", headers, true);
        assert!(client.is_ok());
    }

    #[test]
    #[cfg(target_os = "macos")]
    fn test_macos() {
        use regex_lite::Regex;
        let user_agent = get_code_user_agent(None);
        let re = Regex::new(
            r"^code_cli_rs/\d+\.\d+\.\d+ \(Mac OS \d+\.\d+\.\d+; (x86_64|arm64)\) (\S+)$",
        )
        .unwrap();
        assert!(re.is_match(&user_agent));
    }
}
