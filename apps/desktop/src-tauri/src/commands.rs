use crate::auth::{exchange_bootstrap_for_bearer_token, generate_random_hex};
use crate::backend::BackendSupervisor;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::time::{Duration, Instant};
use tauri::menu::{CheckMenuItem, Menu, MenuItem, PredefinedMenuItem, Submenu};
use tauri::{AppHandle, Emitter, LogicalPosition, Manager, Runtime, State, Theme};
use tauri_plugin_dialog::DialogExt;

const KEYCHAIN_SERVICE: &str = "ai.hanzo.dev";
const KEYCHAIN_ACCOUNT: &str = "iam-vault";

pub struct AppState {
    pub supervisor: BackendSupervisor,
    pub bearer_token: tokio::sync::Mutex<Option<String>>,
    pub local_env_enabled: Mutex<bool>,
    pub exposure_mode: Mutex<String>,
    pub update_channel: Mutex<String>,
    pub pending_deep_links: Mutex<Vec<String>>,
    pub web_links_ready: Mutex<bool>,
    pub pending_context_menu:
        Mutex<Option<(String, tokio::sync::oneshot::Sender<Option<String>>)>>,
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
    let _ = fs::write(path, serde_json::to_string_pretty(&base).unwrap_or_default());
}

fn iam_vault_fallback_path(state_dir: &Path) -> PathBuf {
    state_dir.join("iam-vault.json")
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
    let runtime_file = state.supervisor.config.state_dir.join("server-runtime.json");
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
    let path = state.supervisor.config.state_dir.join("client-settings.json");
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
    let path = state.supervisor.config.state_dir.join("client-settings.json");
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
            _ => "x-apple.systempreferences:com.apple.preference.security",
        };
        return Ok(opener::open(target).is_ok());
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
pub async fn desktop_discover_lan_engines() -> Result<Vec<DiscoveredLanEngine>, String> {
    let client = reqwest::Client::builder()
        .timeout(Duration::from_millis(450))
        .build()
        .map_err(|e| e.to_string())?;

    let targets = [
        ("Embedded Hanzo Engine", "127.0.0.1", 36900u16, "hanzo-engine", "/v1/models"),
        ("Hanzo Node (hanzod)", "127.0.0.1", 3690u16, "hanzo-engine", "/v1/models"),
        ("Hanzo Validator Node (hanzod)", "127.0.0.1", 9630u16, "hanzo-engine", "/v1/health"),
        ("Hanzo Engine", "127.0.0.1", 8000u16, "hanzo-engine", "/v1/models"),
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
    let mut found = Vec::new();
    #[cfg(target_os = "macos")]
    {
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
    }
    Ok(found)
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
    let channel = state
        .update_channel
        .lock()
        .map(|g| g.clone())
        .unwrap_or_else(|_| "latest".to_string());
    let arch = if cfg!(target_arch = "aarch64") {
        "arm64"
    } else if cfg!(target_arch = "x86_64") {
        "x64"
    } else {
        "other"
    };
    Ok(json!({
        "enabled": true,
        "status": "up-to-date",
        "channel": channel,
        "currentVersion": env!("CARGO_PKG_VERSION"),
        "hostArch": arch,
        "appArch": arch,
        "runningUnderArm64Translation": false,
        "availableVersion": Value::Null,
        "downloadedVersion": Value::Null,
        "releaseNotes": [],
        "omittedReleaseCount": 0,
        "downloadPercent": Value::Null,
        "checkedAt": Value::Null,
        "message": Value::Null,
        "errorContext": Value::Null,
        "canRetry": false,
    }))
}

#[tauri::command]
pub async fn desktop_set_update_channel(
    state: State<'_, AppState>,
    channel: String,
) -> Result<Value, String> {
    if let Ok(mut guard) = state.update_channel.lock() {
        *guard = channel;
    }
    desktop_get_update_state(state).await
}

#[tauri::command]
pub async fn desktop_cli_get_state() -> Result<Value, String> {
    let home = dirs::home_dir().unwrap_or_else(|| PathBuf::from("."));
    let installed = home.join(".local/bin/hanzo-dev-app");
    let exists = installed.exists();
    Ok(json!({
        "supported": true,
        "installedPath": exists.then(|| installed.to_string_lossy().into_owned()),
        "onPath": exists,
        "launcherPath": std::env::current_exe().ok().map(|p| p.to_string_lossy().into_owned()),
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
            let mut links = state
                .pending_deep_links
                .lock()
                .map_err(|e| e.to_string())?;
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
            let check = CheckMenuItem::with_id(app, menu_id, &item.label, enabled, checked, None::<&str>)?;
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
}
