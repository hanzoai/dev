use crate::auth::{exchange_bootstrap_for_bearer_token, generate_random_hex};
use crate::backend::{resolve_backend_target, BackendSupervisor, ResolvedBackendTarget};
use crate::snapshot::{
    build_snapshot_state_payload, capture_screen_region_to_file, spawn_interactive_snapshot,
    unix_secs_to_iso8601, DesktopPendingSnapShotPayload, DesktopSnapShotPayload, SnapshotManager,
};
use crate::ssh::{SshEnvironmentTargetInput, SshTunnelSupervisor};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::cmp::Ordering;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use tauri::menu::{CheckMenuItem, Menu, MenuItem, PredefinedMenuItem, Submenu};
use tauri::{AppHandle, Emitter, LogicalPosition, Manager, Runtime, State, Theme};
use tauri_plugin_dialog::DialogExt;

const KEYCHAIN_SERVICE: &str = "ai.hanzo.dev";
const KEYCHAIN_ACCOUNT: &str = "iam-vault";
const GITHUB_RELEASES_API: &str = "https://api.github.com/repos/hanzoai/dev-desktop/releases";

#[derive(Debug, Clone)]
pub struct UpdateTrackerState {
    pub status: String,
    pub channel: String,
    pub available_version: Option<String>,
    pub downloaded_version: Option<String>,
    pub asset_download_url: Option<String>,
    pub asset_file_name: Option<String>,
    pub downloaded_file_path: Option<PathBuf>,
    pub release_html_url: Option<String>,
    pub release_notes: Vec<Value>,
    pub checked_at: Option<String>,
    pub message: Option<String>,
    pub error_context: Option<String>,
    pub can_retry: bool,
}

impl Default for UpdateTrackerState {
    fn default() -> Self {
        Self {
            status: "up-to-date".to_string(),
            channel: "latest".to_string(),
            available_version: None,
            downloaded_version: None,
            asset_download_url: None,
            asset_file_name: None,
            downloaded_file_path: None,
            release_html_url: None,
            release_notes: Vec::new(),
            checked_at: None,
            message: None,
            error_context: None,
            can_retry: false,
        }
    }
}

pub struct AppState {
    pub supervisor: BackendSupervisor,
    pub bearer_token: tokio::sync::Mutex<Option<String>>,
    pub local_env_enabled: Mutex<bool>,
    pub exposure_mode: Mutex<String>,
    pub update_tracker: Mutex<UpdateTrackerState>,
    pub snapshots: SnapshotManager,
    pub ssh_tunnels: SshTunnelSupervisor,
    pub pending_deep_links: Mutex<Vec<String>>,
    pub web_links_ready: Mutex<bool>,
    pub pending_context_menu: Mutex<Option<(String, tokio::sync::oneshot::Sender<Option<String>>)>>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SshHostRecord {
    pub alias: String,
    pub hostname: String,
    pub username: Option<String>,
    pub port: Option<u16>,
    pub source: &'static str,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ContextMenuItemInput {
    pub id: String,
    pub label: String,
    #[serde(default)]
    pub disabled: Option<bool>,
    #[serde(default)]
    pub header: Option<bool>,
    #[serde(default)]
    pub separator_before: Option<bool>,
    #[serde(default)]
    pub checked: Option<bool>,
    #[serde(default)]
    pub children: Option<Vec<ContextMenuItemInput>>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ContextMenuPosition {
    pub x: f64,
    pub y: f64,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PickFolderOptionsInput {
    pub initial_path: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct PickedThemeFileOutput {
    pub name: String,
    pub size: u64,
    pub text: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DiscoveredLanEngine {
    pub name: String,
    pub host: String,
    pub port: u16,
    pub base_url: String,
    pub kind: &'static str,
    pub models: Vec<String>,
    pub latency_ms: Option<u64>,
}

pub fn read_persisted_local_env_enabled(state_dir: &Path) -> bool {
    let path = state_dir.join("desktop-prefs.json");
    if let Ok(raw) = fs::read_to_string(path) {
        if let Ok(val) = serde_json::from_str::<Value>(&raw) {
            if let Some(enabled) = val.get("localEnvironmentEnabled").and_then(Value::as_bool) {
                return enabled;
            }
        }
    }
    true
}

fn write_persisted_local_env_enabled(state_dir: &Path, enabled: bool) {
    let _ = fs::create_dir_all(state_dir);
    let path = state_dir.join("desktop-prefs.json");
    let mut base = fs::read_to_string(&path)
        .ok()
        .and_then(|s| serde_json::from_str::<Value>(&s).ok())
        .unwrap_or_else(|| json!({}));
    if let Some(obj) = base.as_object_mut() {
        obj.insert("localEnvironmentEnabled".to_string(), json!(enabled));
    }
    let _ = fs::write(
        path,
        serde_json::to_string_pretty(&base).unwrap_or_default(),
    );
}

fn iam_vault_fallback_path(state_dir: &Path) -> PathBuf {
    state_dir.join("iam-vault.json")
}

fn now_iso8601() -> String {
    let secs = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);
    unix_secs_to_iso8601(secs)
}

pub fn compare_semver(left: &str, right: &str) -> Ordering {
    let normalize = |v: &str| -> (Vec<u64>, Option<String>) {
        let stripped = v.trim().trim_start_matches('v');
        let (core, pre) = match stripped.split_once('-') {
            Some((c, p)) => (c, Some(p.to_string())),
            None => (stripped, None),
        };
        let nums: Vec<u64> = core
            .split('.')
            .map(|seg| seg.parse::<u64>().unwrap_or(0))
            .collect();
        (nums, pre)
    };
    let (mut l_nums, l_pre) = normalize(left);
    let (mut r_nums, r_pre) = normalize(right);
    let max_len = l_nums.len().max(r_nums.len()).max(3);
    l_nums.resize(max_len, 0);
    r_nums.resize(max_len, 0);

    match l_nums.cmp(&r_nums) {
        Ordering::Equal => match (l_pre, r_pre) {
            (None, None) => Ordering::Equal,
            (None, Some(_)) => Ordering::Greater,
            (Some(_), None) => Ordering::Less,
            (Some(lp), Some(rp)) => lp.cmp(&rp),
        },
        ord => ord,
    }
}

fn runtime_arch_str() -> &'static str {
    if cfg!(target_arch = "aarch64") {
        "arm64"
    } else if cfg!(target_arch = "x86_64") {
        "x64"
    } else {
        "other"
    }
}

fn serialize_update_state(tracker: &UpdateTrackerState) -> Value {
    let arch = runtime_arch_str();
    json!({
        "enabled": true,
        "status": tracker.status,
        "channel": tracker.channel,
        "currentVersion": env!("CARGO_PKG_VERSION"),
        "hostArch": arch,
        "appArch": arch,
        "runningUnderArm64Translation": false,
        "availableVersion": tracker.available_version,
        "downloadedVersion": tracker.downloaded_version,
        "releaseNotes": tracker.release_notes,
        "omittedReleaseCount": 0,
        "downloadPercent": Value::Null,
        "checkedAt": tracker.checked_at,
        "message": tracker.message,
        "errorContext": tracker.error_context,
        "canRetry": tracker.can_retry,
    })
}

fn matches_platform_asset(name: &str) -> bool {
    let lower = name.to_ascii_lowercase();
    let arch_ok = if cfg!(target_arch = "aarch64") {
        lower.contains("aarch64") || lower.contains("arm64") || !lower.contains("x64")
    } else {
        lower.contains("x64") || lower.contains("x86_64") || !lower.contains("arm64")
    };
    if !arch_ok {
        return false;
    }
    if cfg!(target_os = "macos") {
        lower.ends_with(".dmg")
    } else if cfg!(target_os = "windows") {
        lower.ends_with(".exe") || lower.ends_with(".msi")
    } else {
        lower.ends_with(".appimage") || lower.ends_with(".deb")
    }
}

#[tauri::command]
pub async fn desktop_get_iam_vault(state: State<'_, AppState>) -> Result<Option<String>, String> {
    if let Ok(entry) = keyring::Entry::new(KEYCHAIN_SERVICE, KEYCHAIN_ACCOUNT) {
        if let Ok(secret) = entry.get_password() {
            if !secret.trim().is_empty() {
                return Ok(Some(secret));
            }
        }
    }
    let fallback = iam_vault_fallback_path(&state.supervisor.config.state_dir);
    match fs::read_to_string(fallback) {
        Ok(contents) if !contents.trim().is_empty() => Ok(Some(contents)),
        _ => Ok(None),
    }
}

#[tauri::command]
pub async fn desktop_set_iam_vault(
    state: State<'_, AppState>,
    vault_json: Option<String>,
) -> Result<bool, String> {
    let mut stored_in_keychain = false;
    if let Ok(entry) = keyring::Entry::new(KEYCHAIN_SERVICE, KEYCHAIN_ACCOUNT) {
        match &vault_json {
            Some(payload) if !payload.trim().is_empty() => {
                if entry.set_password(payload).is_ok() {
                    stored_in_keychain = true;
                }
            }
            _ => {
                let _ = entry.delete_credential();
                stored_in_keychain = true;
            }
        }
    }

    let fallback = iam_vault_fallback_path(&state.supervisor.config.state_dir);
    let _ = fs::create_dir_all(&state.supervisor.config.state_dir);
    match vault_json {
        Some(payload) if !payload.trim().is_empty() => {
            let _ = fs::write(&fallback, payload);
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                let _ = fs::set_permissions(&fallback, fs::Permissions::from_mode(0o600));
            }
        }
        _ => {
            let _ = fs::remove_file(&fallback);
        }
    }

    Ok(stored_in_keychain)
}

#[tauri::command]
pub async fn desktop_get_local_environment_bearer_token(
    state: State<'_, AppState>,
) -> Result<String, String> {
    let mut guard = state.bearer_token.lock().await;
    if let Some(existing) = guard.as_ref() {
        return Ok(existing.clone());
    }
    let token = exchange_bootstrap_for_bearer_token(
        &state.supervisor.config.http_base_url,
        &state.supervisor.config.bootstrap_secret,
    )
    .await?;
    *guard = Some(token.clone());
    Ok(token)
}

#[tauri::command]
pub async fn desktop_set_local_environment_enabled(
    state: State<'_, AppState>,
    enabled: bool,
) -> Result<(), String> {
    if let Ok(mut guard) = state.local_env_enabled.lock() {
        *guard = enabled;
    }
    write_persisted_local_env_enabled(&state.supervisor.config.state_dir, enabled);
    Ok(())
}

#[tauri::command]
pub async fn desktop_get_host_environment_id(
    state: State<'_, AppState>,
) -> Result<Option<String>, String> {
    let runtime_file = state
        .supervisor
        .config
        .state_dir
        .join("server-runtime.json");
    if let Ok(raw) = fs::read_to_string(runtime_file) {
        if let Ok(parsed) = serde_json::from_str::<Value>(&raw) {
            if let Some(id) = parsed.get("environmentId").and_then(Value::as_str) {
                return Ok(Some(id.to_string()));
            }
        }
    }
    Ok(None)
}

#[tauri::command]
pub async fn desktop_get_client_settings(
    state: State<'_, AppState>,
) -> Result<Option<Value>, String> {
    let path = state
        .supervisor
        .config
        .state_dir
        .join("client-settings.json");
    match fs::read_to_string(path) {
        Ok(raw) => Ok(serde_json::from_str::<Value>(&raw).ok()),
        Err(_) => Ok(None),
    }
}

#[tauri::command]
pub async fn desktop_set_client_settings(
    state: State<'_, AppState>,
    settings: Value,
) -> Result<(), String> {
    let _ = fs::create_dir_all(&state.supervisor.config.state_dir);
    let path = state
        .supervisor
        .config
        .state_dir
        .join("client-settings.json");
    let serialized = serde_json::to_string_pretty(&settings).map_err(|e| e.to_string())?;
    fs::write(path, serialized).map_err(|e| e.to_string())
}

#[tauri::command]
pub async fn desktop_get_connection_catalog(
    state: State<'_, AppState>,
) -> Result<Option<String>, String> {
    let path = state
        .supervisor
        .config
        .state_dir
        .join("connection-catalog.json");
    match fs::read_to_string(path) {
        Ok(raw) => Ok(Some(raw)),
        Err(_) => Ok(None),
    }
}

#[tauri::command]
pub async fn desktop_set_connection_catalog(
    state: State<'_, AppState>,
    catalog: String,
) -> Result<bool, String> {
    let _ = fs::create_dir_all(&state.supervisor.config.state_dir);
    let path = state
        .supervisor
        .config
        .state_dir
        .join("connection-catalog.json");
    fs::write(path, catalog).map_err(|e| e.to_string())?;
    Ok(true)
}

#[tauri::command]
pub async fn desktop_pick_folder(
    app: AppHandle,
    options: Option<PickFolderOptionsInput>,
) -> Result<Option<String>, String> {
    tokio::task::spawn_blocking(move || {
        let mut dialog = app.dialog().file();
        if let Some(initial) = options.and_then(|o| o.initial_path) {
            let p = PathBuf::from(initial);
            if p.is_dir() {
                dialog = dialog.set_directory(p);
            }
        }
        Ok(dialog
            .blocking_pick_folder()
            .and_then(|fp| fp.into_path().ok())
            .map(|p| p.to_string_lossy().into_owned()))
    })
    .await
    .map_err(|e| e.to_string())?
}

#[tauri::command]
pub async fn desktop_pick_project_favicon(
    app: AppHandle,
    initial_path: Option<String>,
) -> Result<Option<String>, String> {
    tokio::task::spawn_blocking(move || {
        let mut dialog = app
            .dialog()
            .file()
            .add_filter("Images", &["png", "jpg", "jpeg", "svg", "ico", "webp"]);
        if let Some(init) = initial_path {
            let p = PathBuf::from(init);
            if p.is_dir() {
                dialog = dialog.set_directory(p);
            } else if let Some(parent) = p.parent() {
                if parent.is_dir() {
                    dialog = dialog.set_directory(parent);
                }
            }
        }
        Ok(dialog
            .blocking_pick_file()
            .and_then(|fp| fp.into_path().ok())
            .map(|p| p.to_string_lossy().into_owned()))
    })
    .await
    .map_err(|e| e.to_string())?
}

#[tauri::command]
pub async fn desktop_pick_theme_files(
    app: AppHandle,
) -> Result<Option<Vec<PickedThemeFileOutput>>, String> {
    tokio::task::spawn_blocking(move || {
        let picked = app
            .dialog()
            .file()
            .add_filter("Theme JSON", &["json"])
            .blocking_pick_files();
        let Some(files) = picked else {
            return Ok(None);
        };
        let mut out = Vec::with_capacity(files.len());
        for f in files {
            if let Ok(path) = f.into_path() {
                let name = path
                    .file_name()
                    .map(|n| n.to_string_lossy().into_owned())
                    .unwrap_or_else(|| "theme.json".to_string());
                let size = fs::metadata(&path).map(|m| m.len()).unwrap_or(0);
                let text = if size <= 2 * 1024 * 1024 {
                    fs::read_to_string(&path).unwrap_or_default()
                } else {
                    String::new()
                };
                out.push(PickedThemeFileOutput { name, size, text });
            }
        }
        Ok(Some(out))
    })
    .await
    .map_err(|e| e.to_string())?
}

#[tauri::command]
pub async fn desktop_set_theme(app: AppHandle, theme: String) -> Result<(), String> {
    let resolved = match theme.as_str() {
        "dark" => Some(Theme::Dark),
        "light" => Some(Theme::Light),
        _ => None,
    };
    if let Some(window) = app.get_webview_window("main") {
        let _ = window.set_theme(resolved);
    }
    Ok(())
}

#[tauri::command]
pub async fn desktop_reveal_window(app: AppHandle) -> Result<(), String> {
    if let Some(window) = app.get_webview_window("main") {
        let _ = window.unminimize();
        let _ = window.show();
        let _ = window.set_focus();
    }
    Ok(())
}

#[tauri::command]
pub fn desktop_start_dragging(app: AppHandle) -> Result<(), String> {
    if let Some(window) = app.get_webview_window("main") {
        let _ = window.start_dragging();
    }
    Ok(())
}

#[tauri::command]
pub fn desktop_toggle_maximize(app: AppHandle) -> Result<(), String> {
    if let Some(window) = app.get_webview_window("main") {
        if window.is_maximized().unwrap_or(false) {
            let _ = window.unmaximize();
        } else {
            let _ = window.maximize();
        }
    }
    Ok(())
}

#[tauri::command]
pub async fn desktop_open_external(url: String) -> Result<bool, String> {
    Ok(opener::open(url).is_ok())
}

#[tauri::command]
pub async fn desktop_open_system_settings(pane: String) -> Result<bool, String> {
    #[cfg(target_os = "macos")]
    {
        let target = match pane.as_str() {
            "full-disk-access" => {
                "x-apple.systempreferences:com.apple.preference.security?Privacy_AllFiles"
            }
            "screen-recording" => {
                "x-apple.systempreferences:com.apple.preference.security?Privacy_ScreenCapture"
            }
            "accessibility" => {
                "x-apple.systempreferences:com.apple.preference.security?Privacy_Accessibility"
            }
            _ => "x-apple.systempreferences:com.apple.preference.security",
        };
        Ok(opener::open(target).is_ok())
    }
    #[cfg(not(target_os = "macos"))]
    {
        let _ = pane;
        Ok(false)
    }
}

#[tauri::command]
pub async fn desktop_read_media(url: String) -> Result<Option<Value>, String> {
    let client = reqwest::Client::builder()
        .timeout(Duration::from_secs(15))
        .build()
        .map_err(|e| e.to_string())?;
    let Ok(resp) = client.get(&url).send().await else {
        return Ok(None);
    };
    let status = resp.status().as_u16();
    let mime_type = resp
        .headers()
        .get(reqwest::header::CONTENT_TYPE)
        .and_then(|v| v.to_str().ok())
        .unwrap_or("application/octet-stream")
        .to_string();
    let Ok(bytes) = resp.bytes().await else {
        return Ok(None);
    };
    Ok(Some(json!({
        "status": status,
        "mimeType": mime_type,
        "data": bytes.to_vec(),
    })))
}

pub fn parse_ssh_config_contents(contents: &str) -> Vec<SshHostRecord> {
    let mut hosts = Vec::new();
    let mut current_aliases: Vec<String> = Vec::new();
    let mut current_hostname: Option<String> = None;
    let mut current_user: Option<String> = None;
    let mut current_port: Option<u16> = None;

    let flush = |aliases: &mut Vec<String>,
                 hostname: &mut Option<String>,
                 user: &mut Option<String>,
                 port: &mut Option<u16>,
                 out: &mut Vec<SshHostRecord>| {
        for alias in aliases.drain(..) {
            if alias.contains('*') || alias.contains('?') || alias.starts_with('!') {
                continue;
            }
            out.push(SshHostRecord {
                hostname: hostname.clone().unwrap_or_else(|| alias.clone()),
                alias,
                username: user.clone(),
                port: *port,
                source: "ssh-config",
            });
        }
        *hostname = None;
        *user = None;
        *port = None;
    };

    for line in contents.lines() {
        let trimmed = line.split('#').next().unwrap_or("").trim();
        if trimmed.is_empty() {
            continue;
        }
        let mut parts = trimmed.split_whitespace();
        let Some(key) = parts.next() else {
            continue;
        };
        let rest: Vec<&str> = parts.collect();
        if rest.is_empty() {
            continue;
        }
        match key.to_ascii_lowercase().as_str() {
            "host" => {
                flush(
                    &mut current_aliases,
                    &mut current_hostname,
                    &mut current_user,
                    &mut current_port,
                    &mut hosts,
                );
                current_aliases.extend(rest.into_iter().map(str::to_string));
            }
            "hostname" => {
                current_hostname = Some(rest[0].to_string());
            }
            "user" => {
                current_user = Some(rest[0].to_string());
            }
            "port" => {
                current_port = rest[0].parse::<u16>().ok();
            }
            _ => {}
        }
    }
    flush(
        &mut current_aliases,
        &mut current_hostname,
        &mut current_user,
        &mut current_port,
        &mut hosts,
    );
    hosts
}

#[tauri::command]
pub async fn desktop_discover_ssh_hosts() -> Result<Vec<SshHostRecord>, String> {
    let Some(home) = dirs::home_dir() else {
        return Ok(Vec::new());
    };
    let ssh_config = home.join(".ssh/config");
    match fs::read_to_string(ssh_config) {
        Ok(contents) => Ok(parse_ssh_config_contents(&contents)),
        Err(_) => Ok(Vec::new()),
    }
}

#[tauri::command]
pub async fn desktop_resolve_ssh_host(alias: String) -> Result<Value, String> {
    let hosts = desktop_discover_ssh_hosts().await?;
    if let Some(found) = hosts.into_iter().find(|h| h.alias == alias) {
        return Ok(json!({
            "alias": found.alias,
            "hostname": found.hostname,
            "username": found.username,
            "port": found.port,
        }));
    }
    Ok(json!({
        "alias": alias,
        "hostname": alias,
        "username": Value::Null,
        "port": Value::Null,
    }))
}

#[tauri::command]
pub async fn desktop_ensure_ssh_environment(
    state: State<'_, AppState>,
    target: SshEnvironmentTargetInput,
) -> Result<Value, String> {
    state.ssh_tunnels.ensure_environment(target).await
}

#[tauri::command]
pub async fn desktop_disconnect_ssh_environment(
    state: State<'_, AppState>,
    target: SshEnvironmentTargetInput,
) -> Result<(), String> {
    state.ssh_tunnels.disconnect(&target);
    Ok(())
}

#[tauri::command]
pub async fn desktop_get_snapshot_state(state: State<'_, AppState>) -> Result<Value, String> {
    Ok(build_snapshot_state_payload(
        &state.supervisor.config.state_dir,
        state.snapshots.is_shortcut_registered(),
    ))
}

#[tauri::command]
pub async fn desktop_request_snapshot_permissions(
    include_accessibility: bool,
) -> Result<(), String> {
    #[cfg(target_os = "macos")]
    {
        if include_accessibility {
            let _ = opener::open(
                "x-apple.systempreferences:com.apple.preference.security?Privacy_Accessibility",
            );
        }
    }
    #[cfg(not(target_os = "macos"))]
    {
        let _ = include_accessibility;
    }
    Ok(())
}

#[tauri::command]
pub async fn desktop_setup_snapshot(
    app: AppHandle,
    state: State<'_, AppState>,
    action: String,
) -> Result<(), String> {
    match action.as_str() {
        "test-mac-capture" => {
            spawn_interactive_snapshot(app);
        }
        "allow-screen-recording" => {
            let _ = desktop_open_system_settings("screen-recording".to_string()).await;
        }
        "allow-accessibility" => {
            let _ = desktop_open_system_settings("accessibility".to_string()).await;
        }
        "retry-shortcut" => {
            use tauri_plugin_global_shortcut::GlobalShortcutExt;
            let mgr = app.global_shortcut();
            let registered = mgr.is_registered(crate::SHORTCUT_SNAPSHOT)
                || mgr.register(crate::SHORTCUT_SNAPSHOT).is_ok();
            state.snapshots.set_shortcut_registered(registered);
            let _ = app.emit(
                "desktop:snapshot-event",
                json!({ "type": "shortcut-changed" }),
            );
        }
        _ => {}
    }
    Ok(())
}

#[tauri::command]
pub async fn desktop_check_snapshot_shortcut(shortcut: Value) -> Result<Value, String> {
    let _ = shortcut;
    Ok(json!({
        "available": true,
        "message": Value::Null,
    }))
}

#[tauri::command]
pub async fn desktop_set_snapshot_shortcut_suppressed(
    state: State<'_, AppState>,
    suppressed: bool,
) -> Result<(), String> {
    state.snapshots.set_suppressed(suppressed);
    Ok(())
}

#[tauri::command]
pub async fn desktop_list_pending_snapshots(
    state: State<'_, AppState>,
) -> Result<Vec<DesktopPendingSnapShotPayload>, String> {
    Ok(state.snapshots.list_pending())
}

#[tauri::command]
pub async fn desktop_read_snapshot(
    state: State<'_, AppState>,
    id: String,
) -> Result<DesktopSnapShotPayload, String> {
    state.snapshots.read_snapshot(&id)
}

#[tauri::command]
pub async fn desktop_acknowledge_snapshot(
    state: State<'_, AppState>,
    id: String,
) -> Result<(), String> {
    state.snapshots.acknowledge(&id);
    Ok(())
}

#[tauri::command]
pub async fn desktop_discover_lan_engines() -> Result<Vec<DiscoveredLanEngine>, String> {
    let client = reqwest::Client::builder()
        .timeout(Duration::from_millis(450))
        .build()
        .map_err(|e| e.to_string())?;

    let targets = [
        (
            "Embedded Hanzo Engine",
            "127.0.0.1",
            36900u16,
            "hanzo-engine",
            "/v1/models",
        ),
        (
            "Hanzo Node (hanzod)",
            "127.0.0.1",
            3690u16,
            "hanzo-engine",
            "/v1/models",
        ),
        (
            "Hanzo Validator Node (hanzod)",
            "127.0.0.1",
            9630u16,
            "hanzo-engine",
            "/v1/health",
        ),
        (
            "Hanzo Engine",
            "127.0.0.1",
            8000u16,
            "hanzo-engine",
            "/v1/models",
        ),
        ("Ollama Local", "127.0.0.1", 11434u16, "ollama", "/api/tags"),
    ];

    let mut out = Vec::new();
    for (name, host, port, kind, path) in targets {
        let base_url = format!("http://{host}:{port}");
        let url = format!("{base_url}{path}");
        let started = Instant::now();
        if let Ok(resp) = client.get(&url).send().await {
            if resp.status().is_success() {
                let latency_ms = Some(started.elapsed().as_millis() as u64);
                let mut models = Vec::new();
                if let Ok(body) = resp.json::<Value>().await {
                    if let Some(data) = body.get("data").and_then(Value::as_array) {
                        for m in data {
                            if let Some(id) = m.get("id").and_then(Value::as_str) {
                                models.push(id.to_string());
                            }
                        }
                    } else if let Some(list) = body.get("models").and_then(Value::as_array) {
                        for m in list {
                            if let Some(id) = m.get("name").and_then(Value::as_str) {
                                models.push(id.to_string());
                            }
                        }
                    }
                }
                out.push(DiscoveredLanEngine {
                    name: name.to_string(),
                    host: host.to_string(),
                    port,
                    base_url,
                    kind,
                    models,
                    latency_ms,
                });
            }
        }
    }
    Ok(out)
}

#[tauri::command]
pub async fn desktop_probe_remote_editors() -> Result<Vec<String>, String> {
    #[cfg(target_os = "macos")]
    {
        let mut found = Vec::new();
        let app_checks = [
            ("hanzo", "/Applications/Hanzo IDE.app"),
            ("cursor", "/Applications/Cursor.app"),
            ("vscode", "/Applications/Visual Studio Code.app"),
            ("zed", "/Applications/Zed.app"),
            ("antigravity", "/Applications/Antigravity.app"),
        ];
        for (id, bundle) in app_checks {
            if Path::new(bundle).exists() {
                found.push(id.to_string());
            }
        }
        Ok(found)
    }
    #[cfg(not(target_os = "macos"))]
    {
        Ok(Vec::new())
    }
}

#[tauri::command]
pub async fn desktop_get_server_exposure_state(
    state: State<'_, AppState>,
) -> Result<Value, String> {
    let mode = state
        .exposure_mode
        .lock()
        .map(|g| g.clone())
        .unwrap_or_else(|_| "local-only".to_string());
    Ok(json!({
        "mode": mode,
        "endpointUrl": state.supervisor.config.http_base_url,
        "advertisedHost": "127.0.0.1",
        "tailscaleServeEnabled": false,
        "tailscaleServePort": 443,
    }))
}

#[tauri::command]
pub async fn desktop_set_server_exposure_mode(
    state: State<'_, AppState>,
    mode: String,
) -> Result<Value, String> {
    if let Ok(mut guard) = state.exposure_mode.lock() {
        *guard = mode;
    }
    desktop_get_server_exposure_state(state).await
}

#[tauri::command]
pub async fn desktop_get_update_state(state: State<'_, AppState>) -> Result<Value, String> {
    let guard = state.update_tracker.lock().map_err(|e| e.to_string())?;
    Ok(serialize_update_state(&guard))
}

#[tauri::command]
pub async fn desktop_set_update_channel(
    app: AppHandle,
    state: State<'_, AppState>,
    channel: String,
) -> Result<Value, String> {
    let payload = {
        let mut guard = state.update_tracker.lock().map_err(|e| e.to_string())?;
        guard.channel = channel;
        serialize_update_state(&guard)
    };
    let _ = app.emit("desktop:update-state", payload.clone());
    Ok(payload)
}

#[tauri::command]
pub async fn desktop_check_for_update(
    app: AppHandle,
    state: State<'_, AppState>,
) -> Result<Value, String> {
    let channel = {
        let mut guard = state.update_tracker.lock().map_err(|e| e.to_string())?;
        guard.status = "checking".to_string();
        guard.error_context = None;
        guard.message = None;
        let checking_payload = serialize_update_state(&guard);
        let _ = app.emit("desktop:update-state", checking_payload);
        guard.channel.clone()
    };

    let client = reqwest::Client::builder()
        .timeout(Duration::from_secs(12))
        .user_agent(format!("Hanzo-Dev-Desktop/{}", env!("CARGO_PKG_VERSION")))
        .build()
        .map_err(|e| e.to_string())?;

    let checked_at = now_iso8601();
    let resp = client.get(GITHUB_RELEASES_API).send().await;

    let next_state = match resp {
        Ok(r) if r.status().is_success() => {
            let releases = r.json::<Vec<Value>>().await.unwrap_or_default();
            let candidate = releases.into_iter().find(|rel| {
                let draft = rel.get("draft").and_then(Value::as_bool).unwrap_or(false);
                let prerelease = rel
                    .get("prerelease")
                    .and_then(Value::as_bool)
                    .unwrap_or(false);
                !draft && (channel == "nightly" || !prerelease)
            });

            let mut guard = state.update_tracker.lock().map_err(|e| e.to_string())?;
            guard.checked_at = Some(checked_at);
            guard.can_retry = false;
            guard.error_context = None;
            guard.message = None;

            if let Some(rel) = candidate {
                let raw_tag = rel
                    .get("tag_name")
                    .and_then(Value::as_str)
                    .unwrap_or(env!("CARGO_PKG_VERSION"));
                let clean_ver = raw_tag.trim_start_matches('v').to_string();
                let current_ver = env!("CARGO_PKG_VERSION");

                if compare_semver(&clean_ver, current_ver) == Ordering::Greater {
                    let mut matched_url: Option<String> = None;
                    let mut matched_name: Option<String> = None;
                    if let Some(assets) = rel.get("assets").and_then(Value::as_array) {
                        for asset in assets {
                            let name = asset.get("name").and_then(Value::as_str).unwrap_or("");
                            let dl = asset
                                .get("browser_download_url")
                                .and_then(Value::as_str)
                                .unwrap_or("");
                            if !name.is_empty() && !dl.is_empty() && matches_platform_asset(name) {
                                matched_url = Some(dl.to_string());
                                matched_name = Some(name.to_string());
                                break;
                            }
                        }
                    }

                    let body_lines: Vec<String> = rel
                        .get("body")
                        .and_then(Value::as_str)
                        .unwrap_or("")
                        .lines()
                        .map(|l| {
                            l.trim()
                                .trim_start_matches("- ")
                                .trim_start_matches("* ")
                                .to_string()
                        })
                        .filter(|l| !l.is_empty() && !l.starts_with('#'))
                        .take(12)
                        .collect();
                    let released_at = rel
                        .get("published_at")
                        .and_then(Value::as_str)
                        .map(str::to_string);

                    guard.status = "available".to_string();
                    guard.available_version = Some(clean_ver.clone());
                    guard.asset_download_url = matched_url;
                    guard.asset_file_name = matched_name;
                    guard.release_html_url = rel
                        .get("html_url")
                        .and_then(Value::as_str)
                        .map(str::to_string);
                    let total_items = body_lines.len();
                    guard.release_notes = vec![json!({
                        "version": clean_ver,
                        "items": body_lines,
                        "totalItems": total_items,
                        "releasedAt": released_at,
                    })];
                } else {
                    guard.status = "up-to-date".to_string();
                    guard.available_version = None;
                    guard.release_notes.clear();
                }
            } else {
                guard.status = "up-to-date".to_string();
                guard.available_version = None;
            }
            serialize_update_state(&guard)
        }
        Ok(r) if r.status().as_u16() == 404 => {
            let mut guard = state.update_tracker.lock().map_err(|e| e.to_string())?;
            guard.status = "up-to-date".to_string();
            guard.checked_at = Some(checked_at);
            guard.available_version = None;
            serialize_update_state(&guard)
        }
        Ok(r) => {
            let mut guard = state.update_tracker.lock().map_err(|e| e.to_string())?;
            guard.status = "error".to_string();
            guard.checked_at = Some(checked_at);
            guard.error_context = Some("check".to_string());
            guard.message = Some(format!("Update server returned HTTP {}", r.status()));
            guard.can_retry = true;
            serialize_update_state(&guard)
        }
        Err(err) => {
            let mut guard = state.update_tracker.lock().map_err(|e| e.to_string())?;
            guard.status = "error".to_string();
            guard.checked_at = Some(checked_at);
            guard.error_context = Some("check".to_string());
            guard.message = Some(format!("Failed to check updates: {err}"));
            guard.can_retry = true;
            serialize_update_state(&guard)
        }
    };

    let _ = app.emit("desktop:update-state", next_state.clone());
    Ok(json!({
        "checked": true,
        "state": next_state,
    }))
}

#[tauri::command]
pub async fn desktop_download_update(
    app: AppHandle,
    state: State<'_, AppState>,
) -> Result<Value, String> {
    let (download_url, file_name, avail_ver) = {
        let mut guard = state.update_tracker.lock().map_err(|e| e.to_string())?;
        let Some(ver) = guard.available_version.clone() else {
            let s = serialize_update_state(&guard);
            return Ok(json!({ "accepted": false, "completed": false, "state": s }));
        };
        guard.status = "downloading".to_string();
        let s = serialize_update_state(&guard);
        let _ = app.emit("desktop:update-state", s);
        (
            guard.asset_download_url.clone(),
            guard.asset_file_name.clone(),
            ver,
        )
    };

    let Some(url) = download_url else {
        let mut guard = state.update_tracker.lock().map_err(|e| e.to_string())?;
        guard.status = "downloaded".to_string();
        guard.downloaded_version = Some(avail_ver);
        let s = serialize_update_state(&guard);
        let _ = app.emit("desktop:update-state", s.clone());
        return Ok(json!({ "accepted": true, "completed": true, "state": s }));
    };

    let updates_dir = state.supervisor.config.state_dir.join("updates");
    let _ = fs::create_dir_all(&updates_dir);
    let dest = updates_dir.join(file_name.unwrap_or_else(|| format!("hanzo-dev-{avail_ver}.dmg")));

    let client = reqwest::Client::builder()
        .timeout(Duration::from_secs(300))
        .user_agent(format!("Hanzo-Dev-Desktop/{}", env!("CARGO_PKG_VERSION")))
        .build()
        .map_err(|e| e.to_string())?;

    match client.get(&url).send().await {
        Ok(resp) if resp.status().is_success() => match resp.bytes().await {
            Ok(bytes) => {
                if let Err(err) = fs::write(&dest, &bytes) {
                    let mut guard = state.update_tracker.lock().map_err(|e| e.to_string())?;
                    guard.status = "error".to_string();
                    guard.error_context = Some("download".to_string());
                    guard.message = Some(format!("Failed writing update file: {err}"));
                    guard.can_retry = true;
                    let s = serialize_update_state(&guard);
                    let _ = app.emit("desktop:update-state", s.clone());
                    return Ok(json!({ "accepted": true, "completed": false, "state": s }));
                }
                let mut guard = state.update_tracker.lock().map_err(|e| e.to_string())?;
                guard.status = "downloaded".to_string();
                guard.downloaded_version = Some(avail_ver);
                guard.downloaded_file_path = Some(dest);
                let s = serialize_update_state(&guard);
                let _ = app.emit("desktop:update-state", s.clone());
                Ok(json!({ "accepted": true, "completed": true, "state": s }))
            }
            Err(err) => {
                let mut guard = state.update_tracker.lock().map_err(|e| e.to_string())?;
                guard.status = "error".to_string();
                guard.error_context = Some("download".to_string());
                guard.message = Some(format!("Update download interrupted: {err}"));
                guard.can_retry = true;
                let s = serialize_update_state(&guard);
                let _ = app.emit("desktop:update-state", s.clone());
                Ok(json!({ "accepted": true, "completed": false, "state": s }))
            }
        },
        _ => {
            let mut guard = state.update_tracker.lock().map_err(|e| e.to_string())?;
            guard.status = "error".to_string();
            guard.error_context = Some("download".to_string());
            guard.message = Some("Failed to download update package.".to_string());
            guard.can_retry = true;
            let s = serialize_update_state(&guard);
            let _ = app.emit("desktop:update-state", s.clone());
            Ok(json!({ "accepted": true, "completed": false, "state": s }))
        }
    }
}

#[tauri::command]
pub async fn desktop_install_update(state: State<'_, AppState>) -> Result<Value, String> {
    let (file_path, html_url, current_state) = {
        let guard = state.update_tracker.lock().map_err(|e| e.to_string())?;
        (
            guard.downloaded_file_path.clone(),
            guard.release_html_url.clone(),
            serialize_update_state(&guard),
        )
    };
    if let Some(path) = file_path {
        if path.exists() {
            let _ = opener::open(&path);
            return Ok(json!({
                "accepted": true,
                "completed": true,
                "state": current_state,
            }));
        }
    }
    let fallback_url =
        html_url.unwrap_or_else(|| "https://github.com/hanzoai/dev-desktop/releases".to_string());
    let _ = opener::open(fallback_url);
    Ok(json!({
        "accepted": true,
        "completed": true,
        "state": current_state,
    }))
}

pub fn resolve_shell_rc_path(home: &Path) -> PathBuf {
    let shell = std::env::var("SHELL").unwrap_or_default();
    if shell.ends_with("bash") {
        home.join(".bashrc")
    } else {
        home.join(".zshrc")
    }
}

fn compute_cli_state() -> Value {
    let home = dirs::home_dir().unwrap_or_else(|| PathBuf::from("."));
    let bin_dir = home.join(".local/bin");
    let installed = bin_dir.join("hanzo-dev-app");
    let exists = installed.exists();

    let sep = if cfg!(windows) { ';' } else { ':' };
    let bin_dir_str = bin_dir.to_string_lossy();
    let on_env_path = std::env::var("PATH")
        .unwrap_or_default()
        .split(sep)
        .any(|p| p.trim() == bin_dir_str.as_ref());

    let rc_file = resolve_shell_rc_path(&home);
    let rc_added = fs::read_to_string(&rc_file)
        .map(|contents| contents.contains(".local/bin"))
        .unwrap_or(false);

    let on_path = exists && (on_env_path || rc_added);

    json!({
        "supported": true,
        "installedPath": exists.then(|| installed.to_string_lossy().into_owned()),
        "onPath": on_path,
        "pathSetup": {
            "file": rc_file.to_string_lossy().into_owned(),
            "added": rc_added,
        },
        "launcherPath": std::env::current_exe().ok().map(|p| p.to_string_lossy().into_owned()),
    })
}

#[tauri::command]
pub async fn desktop_cli_get_state() -> Result<Value, String> {
    Ok(compute_cli_state())
}

#[tauri::command]
pub async fn desktop_cli_install() -> Result<Value, String> {
    let home = dirs::home_dir().ok_or_else(|| "Unable to locate home directory.".to_string())?;
    let bin_dir = home.join(".local/bin");
    fs::create_dir_all(&bin_dir).map_err(|e| e.to_string())?;

    // Install as `hanzo-dev-app` so we never shadow `~/.local/bin/dev` (the Rust coding agent CLI).
    let cli_path = bin_dir.join("hanzo-dev-app");
    let script = match resolve_backend_target() {
        Some(ResolvedBackendTarget::StandaloneBinary { executable, .. }) => {
            format!("#!/bin/sh\nexec \"{}\" \"$@\"\n", executable.display())
        }
        Some(ResolvedBackendTarget::NodeScript {
            node_bin,
            entry_mjs,
            ..
        }) => {
            format!(
                "#!/bin/sh\nexec \"{}\" \"{}\" \"$@\"\n",
                node_bin.display(),
                entry_mjs.display()
            )
        }
        None => {
            let exe = std::env::current_exe().map_err(|e| e.to_string())?;
            format!("#!/bin/sh\nexec \"{}\" \"$@\"\n", exe.display())
        }
    };

    if cli_path.symlink_metadata().is_ok() {
        let _ = fs::remove_file(&cli_path);
    }
    fs::write(&cli_path, script).map_err(|e| e.to_string())?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        fs::set_permissions(&cli_path, fs::Permissions::from_mode(0o755))
            .map_err(|e| e.to_string())?;
    }

    Ok(compute_cli_state())
}

#[tauri::command]
pub async fn desktop_cli_uninstall() -> Result<Value, String> {
    if let Some(home) = dirs::home_dir() {
        let cli_path = home.join(".local/bin/hanzo-dev-app");
        if cli_path.symlink_metadata().is_ok() {
            let _ = fs::remove_file(&cli_path);
        }
    }
    Ok(compute_cli_state())
}

#[tauri::command]
pub async fn desktop_cli_add_to_path() -> Result<Value, String> {
    let home = dirs::home_dir().ok_or_else(|| "Unable to locate home directory.".to_string())?;
    let rc_file = resolve_shell_rc_path(&home);
    let existing = fs::read_to_string(&rc_file).unwrap_or_default();
    if !existing.contains(".local/bin") {
        let snippet = format!(
            "{}# Added by Hanzo Dev\nexport PATH=\"$HOME/.local/bin:$PATH\"\n",
            if existing.is_empty() || existing.ends_with('\n') {
                ""
            } else {
                "\n"
            }
        );
        let mut updated = existing;
        updated.push_str(&snippet);
        fs::write(&rc_file, updated).map_err(|e| e.to_string())?;
    }
    Ok(compute_cli_state())
}

#[tauri::command]
pub async fn desktop_preview_open_devtools(app: AppHandle) -> Result<(), String> {
    if let Some(window) = app.get_webview_window("main") {
        window.open_devtools();
    }
    Ok(())
}

#[tauri::command]
pub async fn desktop_preview_capture_screenshot(
    state: State<'_, AppState>,
    tab_id: String,
) -> Result<Value, String> {
    let id = generate_random_hex(12);
    let dir = state.supervisor.config.state_dir.join("preview-artifacts");
    fs::create_dir_all(&dir).map_err(|e| e.to_string())?;
    let path = dir.join(format!("preview-{id}.png"));

    capture_screen_region_to_file(&path, false).await?;
    let size_bytes = fs::metadata(&path).map(|m| m.len()).unwrap_or(0);
    Ok(json!({
        "id": id,
        "tabId": tab_id,
        "path": path.to_string_lossy().into_owned(),
        "mimeType": "image/png",
        "sizeBytes": size_bytes,
        "createdAt": now_iso8601(),
    }))
}

#[tauri::command]
pub async fn desktop_preview_save_recording(
    state: State<'_, AppState>,
    tab_id: String,
    mime_type: String,
    data: Vec<u8>,
) -> Result<Value, String> {
    let id = generate_random_hex(12);
    let ext = if mime_type.contains("mp4") {
        "mp4"
    } else {
        "webm"
    };
    let dir = state.supervisor.config.state_dir.join("preview-artifacts");
    fs::create_dir_all(&dir).map_err(|e| e.to_string())?;
    let path = dir.join(format!("recording-{id}.{ext}"));
    let size_bytes = data.len();
    fs::write(&path, data).map_err(|e| e.to_string())?;
    Ok(json!({
        "id": id,
        "tabId": tab_id,
        "path": path.to_string_lossy().into_owned(),
        "mimeType": mime_type,
        "sizeBytes": size_bytes,
        "createdAt": now_iso8601(),
    }))
}

#[tauri::command]
pub async fn desktop_web_links_set_ready(
    app: AppHandle,
    state: State<'_, AppState>,
    ready: bool,
) -> Result<(), String> {
    let pending = {
        let mut ready_guard = state.web_links_ready.lock().map_err(|e| e.to_string())?;
        *ready_guard = ready;
        if ready {
            let mut links = state.pending_deep_links.lock().map_err(|e| e.to_string())?;
            std::mem::take(&mut *links)
        } else {
            Vec::new()
        }
    };
    for url in pending {
        let _ = app.emit("desktop:web-link-open", url);
    }
    Ok(())
}

fn append_context_menu_items<R: Runtime>(
    app: &AppHandle<R>,
    token: &str,
    items: &[ContextMenuItemInput],
    append_fn: &mut dyn FnMut(&dyn tauri::menu::IsMenuItem<R>) -> tauri::Result<()>,
) -> tauri::Result<()> {
    for item in items {
        if item.separator_before.unwrap_or(false) {
            let sep = PredefinedMenuItem::separator(app)?;
            append_fn(&sep)?;
        }
        let enabled = !item.disabled.unwrap_or(false) && !item.header.unwrap_or(false);
        let menu_id = format!("ctx:{token}:{}", item.id);
        if let Some(children) = &item.children {
            let submenu = Submenu::new(app, &item.label, enabled)?;
            let mut sub_append = |child: &dyn tauri::menu::IsMenuItem<R>| submenu.append(child);
            append_context_menu_items(app, token, children, &mut sub_append)?;
            append_fn(&submenu)?;
        } else if let Some(checked) = item.checked {
            let check =
                CheckMenuItem::with_id(app, menu_id, &item.label, enabled, checked, None::<&str>)?;
            append_fn(&check)?;
        } else {
            let leaf = MenuItem::with_id(app, menu_id, &item.label, enabled, None::<&str>)?;
            append_fn(&leaf)?;
        }
    }
    Ok(())
}

#[tauri::command]
pub async fn desktop_show_context_menu(
    app: AppHandle,
    state: State<'_, AppState>,
    items: Vec<ContextMenuItemInput>,
    position: Option<ContextMenuPosition>,
) -> Result<Option<String>, String> {
    let Some(window) = app.get_webview_window("main") else {
        return Ok(None);
    };
    let token = generate_random_hex(8);
    let (tx, rx) = tokio::sync::oneshot::channel::<Option<String>>();
    if let Ok(mut slot) = state.pending_context_menu.lock() {
        *slot = Some((token.clone(), tx));
    }

    let app_for_main = app.clone();
    let (done_tx, done_rx) = tokio::sync::oneshot::channel::<Result<(), String>>();
    app.run_on_main_thread(move || {
        let res = (|| -> tauri::Result<()> {
            let menu = Menu::new(&app_for_main)?;
            let mut append = |entry: &dyn tauri::menu::IsMenuItem<tauri::Wry>| menu.append(entry);
            append_context_menu_items(&app_for_main, &token, &items, &mut append)?;
            if let Some(pos) = position {
                window.popup_menu_at(&menu, LogicalPosition::new(pos.x, pos.y))?;
            } else {
                window.popup_menu(&menu)?;
            }
            Ok(())
        })()
        .map_err(|e| e.to_string());
        let _ = done_tx.send(res);
    })
    .map_err(|e| e.to_string())?;

    let _ = done_rx.await;

    #[cfg(target_os = "macos")]
    let wait_window = Duration::from_millis(120);
    #[cfg(not(target_os = "macos"))]
    let wait_window = Duration::from_secs(25);

    match tokio::time::timeout(wait_window, rx).await {
        Ok(Ok(selected)) => Ok(selected),
        _ => {
            if let Ok(mut slot) = state.pending_context_menu.lock() {
                *slot = None;
            }
            Ok(None)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_ssh_config_entries_and_ignores_wildcards() {
        let sample = r#"
            # Comment line
            Host *
              ServerAliveInterval 30

            Host dg1 prod-gpu
              HostName 10.0.1.42
              User ubuntu
              Port 2222

            Host jump
              HostName bastion.hanzo.ai
        "#;
        let parsed = parse_ssh_config_contents(sample);
        assert_eq!(parsed.len(), 3);
        assert_eq!(parsed[0].alias, "dg1");
        assert_eq!(parsed[0].hostname, "10.0.1.42");
        assert_eq!(parsed[0].username.as_deref(), Some("ubuntu"));
        assert_eq!(parsed[0].port, Some(2222));
        assert_eq!(parsed[1].alias, "prod-gpu");
        assert_eq!(parsed[2].alias, "jump");
        assert_eq!(parsed[2].hostname, "bastion.hanzo.ai");
    }

    #[test]
    fn compares_semver_versions_and_prereleases_accurately() {
        assert_eq!(compare_semver("0.1.11", "0.1.10"), Ordering::Greater);
        assert_eq!(compare_semver("v0.2.0", "0.1.99"), Ordering::Greater);
        assert_eq!(compare_semver("0.1.10", "0.1.10"), Ordering::Equal);
        assert_eq!(compare_semver("0.2.0-nightly.1", "0.2.0"), Ordering::Less);
        assert_eq!(
            compare_semver("0.2.0-nightly.2", "0.2.0-nightly.1"),
            Ordering::Greater
        );
    }
}
