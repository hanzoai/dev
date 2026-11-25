use crate::auth::generate_random_hex;
use base64::Engine as _;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::fs;
use std::path::Path;
use std::sync::Mutex;
use std::time::{SystemTime, UNIX_EPOCH};
use tauri::{AppHandle, Emitter, Manager};

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SnapShotSourcePayload {
    pub kind: &'static str,
    pub captured_at: String,
    pub app_name: String,
    pub window_title: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DesktopPendingSnapShotPayload {
    pub id: String,
    pub name: String,
    pub mime_type: &'static str,
    pub size_bytes: usize,
    pub source: SnapShotSourcePayload,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DesktopSnapShotPayload {
    pub id: String,
    pub name: String,
    pub mime_type: &'static str,
    pub size_bytes: usize,
    pub source: SnapShotSourcePayload,
    pub data_url: String,
}

#[derive(Debug, Clone)]
struct StoredSnapShot {
    pending: DesktopPendingSnapShotPayload,
    data_url: String,
}

pub struct SnapshotManager {
    pending: Mutex<Vec<StoredSnapShot>>,
    suppressed: Mutex<bool>,
    shortcut_registered: Mutex<bool>,
}

impl SnapshotManager {
    pub fn new() -> Self {
        Self {
            pending: Mutex::new(Vec::new()),
            suppressed: Mutex::new(false),
            shortcut_registered: Mutex::new(true),
        }
    }

    pub fn set_suppressed(&self, value: bool) {
        if let Ok(mut guard) = self.suppressed.lock() {
            *guard = value;
        }
    }

    pub fn is_suppressed(&self) -> bool {
        self.suppressed.lock().map(|g| *g).unwrap_or(false)
    }

    pub fn is_shortcut_registered(&self) -> bool {
        self.shortcut_registered.lock().map(|g| *g).unwrap_or(true)
    }

    pub fn set_shortcut_registered(&self, value: bool) {
        if let Ok(mut guard) = self.shortcut_registered.lock() {
            *guard = value;
        }
    }

    pub fn record_snapshot(&self, id: String, png_bytes: &[u8], app_name: String) {
        let now_secs = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);
        let iso_now = unix_secs_to_iso8601(now_secs);
        let encoded = base64::engine::general_purpose::STANDARD.encode(png_bytes);
        let data_url = format!("data:image/png;base64,{encoded}");
        let name = format!("snapshot-{now_secs}.png");
        let resolved_app = if app_name.trim().is_empty() {
            "Screen Capture".to_string()
        } else {
            app_name.trim().chars().take(255).collect()
        };

        let pending = DesktopPendingSnapShotPayload {
            id,
            name,
            mime_type: "image/png",
            size_bytes: png_bytes.len(),
            source: SnapShotSourcePayload {
                kind: "snap-shot",
                captured_at: iso_now,
                app_name: resolved_app,
                window_title: String::new(),
            },
        };

        if let Ok(mut list) = self.pending.lock() {
            list.push(StoredSnapShot { pending, data_url });
            if list.len() > 25 {
                let excess = list.len() - 25;
                list.drain(0..excess);
            }
        }
    }

    pub fn list_pending(&self) -> Vec<DesktopPendingSnapShotPayload> {
        self.pending
            .lock()
            .map(|list| list.iter().map(|s| s.pending.clone()).collect())
            .unwrap_or_default()
    }

    pub fn read_snapshot(&self, id: &str) -> Result<DesktopSnapShotPayload, String> {
        let list = self.pending.lock().map_err(|e| e.to_string())?;
        let found = list
            .iter()
            .find(|item| item.pending.id == id)
            .ok_or_else(|| format!("SnapShot '{id}' not found."))?;
        Ok(DesktopSnapShotPayload {
            id: found.pending.id.clone(),
            name: found.pending.name.clone(),
            mime_type: found.pending.mime_type,
            size_bytes: found.pending.size_bytes,
            source: found.pending.source.clone(),
            data_url: found.data_url.clone(),
        })
    }

    pub fn acknowledge(&self, id: &str) {
        if let Ok(mut list) = self.pending.lock() {
            list.retain(|item| item.pending.id != id);
        }
    }
}

pub fn unix_secs_to_iso8601(secs: u64) -> String {
    let days = (secs / 86_400) as i64;
    let rem = secs % 86_400;
    let hour = rem / 3600;
    let min = (rem % 3600) / 60;
    let sec = rem % 60;

    // Howard Hinnant civil_from_days algorithm (UTC)
    let z = days + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = (z - era * 146_097) as u64;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146_096) / 365;
    let y = yoe as i64 + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let year = if m <= 2 { y + 1 } else { y };

    format!("{year:04}-{m:02}-{d:02}T{hour:02}:{min:02}:{sec:02}.000Z")
}

pub fn read_configured_snapshot_shortcut(state_dir: &Path) -> Value {
    let settings_path = state_dir.join("client-settings.json");
    if let Ok(raw) = fs::read_to_string(settings_path) {
        if let Ok(parsed) = serde_json::from_str::<Value>(&raw) {
            if let Some(shortcut) = parsed.get("snapShotShortcut") {
                if shortcut.is_object() {
                    return shortcut.clone();
                }
            }
        }
    }
    json!({ "kind": "both-shift-keys" })
}

pub fn build_snapshot_state_payload(state_dir: &Path, shortcut_registered: bool) -> Value {
    let shortcut = read_configured_snapshot_shortcut(state_dir);
    let mut payload = json!({
        "mode": "direct",
        "shortcut": shortcut,
        "shortcutRegistered": shortcut_registered,
        "shortcutCanRetry": true,
        "shortcutVerified": shortcut_registered,
        "shortcutMessage": Value::Null,
        "message": Value::Null,
    });

    #[cfg(target_os = "macos")]
    if let Some(obj) = payload.as_object_mut() {
        obj.insert(
            "macPermissions".to_string(),
            json!({
                "screenRecording": true,
                "accessibility": true,
            }),
        );
    }

    #[cfg(target_os = "windows")]
    if let Some(obj) = payload.as_object_mut() {
        obj.insert("windows".to_string(), json!(true));
    }

    payload
}

async fn detect_frontmost_app_name() -> String {
    #[cfg(target_os = "macos")]
    {
        if let Ok(output) = tokio::process::Command::new("osascript")
            .args([
                "-e",
                "tell application \"System Events\" to get name of first application process whose frontmost is true",
            ])
            .output()
            .await
        {
            if output.status.success() {
                let name = String::from_utf8_lossy(&output.stdout).trim().to_string();
                if !name.is_empty() {
                    return name;
                }
            }
        }
    }
    "Screen Capture".to_string()
}

pub async fn capture_screen_region_to_file(
    dest_path: &Path,
    interactive: bool,
) -> Result<bool, String> {
    #[cfg(target_os = "macos")]
    {
        let mut cmd = tokio::process::Command::new("screencapture");
        cmd.arg("-x");
        if interactive {
            cmd.arg("-i");
        }
        cmd.arg(dest_path);
        let status = cmd.status().await.map_err(|e| e.to_string())?;
        Ok(status.success() && dest_path.is_file())
    }

    #[cfg(target_os = "linux")]
    {
        let path_str = dest_path.to_string_lossy().into_owned();
        let candidates: Vec<(&str, Vec<String>)> = if interactive {
            vec![
                (
                    "gnome-screenshot",
                    vec!["-a".into(), "-f".into(), path_str.clone()],
                ),
                (
                    "spectacle",
                    vec![
                        "-r".into(),
                        "-b".into(),
                        "-n".into(),
                        "-o".into(),
                        path_str.clone(),
                    ],
                ),
                ("grim", vec![path_str]),
            ]
        } else {
            vec![
                ("gnome-screenshot", vec!["-f".into(), path_str.clone()]),
                (
                    "spectacle",
                    vec!["-b".into(), "-n".into(), "-o".into(), path_str.clone()],
                ),
                ("grim", vec![path_str]),
            ]
        };
        for (bin, args) in candidates {
            if let Ok(status) = tokio::process::Command::new(bin).args(&args).status().await {
                if status.success() && dest_path.is_file() {
                    return Ok(true);
                }
            }
        }
        Err(
            "No supported Linux screenshot utility found (gnome-screenshot, spectacle, or grim)."
                .to_string(),
        )
    }

    #[cfg(target_os = "windows")]
    {
        let _ = interactive;
        let escaped = dest_path.to_string_lossy().replace('\'', "''");
        let ps_script = format!(
            "Add-Type -AssemblyName System.Windows.Forms,System.Drawing; \
             $bounds = [System.Windows.Forms.Screen]::PrimaryScreen.Bounds; \
             $bmp = New-Object System.Drawing.Bitmap $bounds.Width, $bounds.Height; \
             $graphics = [System.Drawing.Graphics]::FromImage($bmp); \
             $graphics.CopyFromScreen($bounds.Location, [System.Drawing.Point]::Empty, $bounds.Size); \
             $bmp.Save('{escaped}', [System.Drawing.Imaging.ImageFormat]::Png);"
        );
        let mut ps_cmd = tokio::process::Command::new("powershell");
        ps_cmd.args([
            "-NoProfile",
            "-NonInteractive",
            "-WindowStyle",
            "Hidden",
            "-Command",
            &ps_script,
        ]);
        crate::hide_tokio_console(&mut ps_cmd);
        let status = ps_cmd.status().await.map_err(|e| e.to_string())?;
        Ok(status.success() && dest_path.is_file())
    }
}

pub fn spawn_interactive_snapshot(app: AppHandle) {
    tauri::async_runtime::spawn(async move {
        let Some(state) = app.try_state::<crate::commands::AppState>() else {
            return;
        };
        if state.snapshots.is_suppressed() {
            return;
        }

        let id = generate_random_hex(16);
        let _ = app.emit(
            "desktop:snapshot-event",
            json!({ "type": "requested", "id": id }),
        );
        let front_app = detect_frontmost_app_name().await;
        let _ = app.emit(
            "desktop:snapshot-event",
            json!({ "type": "started", "id": id }),
        );

        let temp_dir = state.supervisor.config.state_dir.join("snapshots");
        let _ = fs::create_dir_all(&temp_dir);
        let temp_file = temp_dir.join(format!("{id}.png"));

        match capture_screen_region_to_file(&temp_file, true).await {
            Ok(true) => match fs::read(&temp_file) {
                Ok(bytes) if !bytes.is_empty() => {
                    let _ = fs::remove_file(&temp_file);
                    state
                        .snapshots
                        .record_snapshot(id.clone(), &bytes, front_app);
                    if let Some(win) = app.get_webview_window("main") {
                        let _ = win.unminimize();
                        let _ = win.show();
                        let _ = win.set_focus();
                    }
                    let _ = app.emit(
                        "desktop:snapshot-event",
                        json!({ "type": "ready", "id": id }),
                    );
                }
                _ => {
                    let _ = fs::remove_file(&temp_file);
                    let _ = app.emit(
                        "desktop:snapshot-event",
                        json!({ "type": "failed", "id": id }),
                    );
                }
            },
            _ => {
                let _ = fs::remove_file(&temp_file);
                let _ = app.emit(
                    "desktop:snapshot-event",
                    json!({ "type": "failed", "id": id }),
                );
            }
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snapshot_manager_lifecycle_roundtrips_png_data_url() {
        let manager = SnapshotManager::new();
        let fake_png = b"\x89PNG\r\n\x1a\ntest";
        let id = "0123456789abcdef0123456789abcdef".to_string();

        manager.record_snapshot(id.clone(), fake_png, "Hanzo Terminal".to_string());
        let pending = manager.list_pending();
        assert_eq!(pending.len(), 1);
        assert_eq!(pending[0].id, id);
        assert_eq!(pending[0].mime_type, "image/png");
        assert_eq!(pending[0].size_bytes, fake_png.len());
        assert_eq!(pending[0].source.kind, "snap-shot");
        assert_eq!(pending[0].source.app_name, "Hanzo Terminal");

        let full = manager.read_snapshot(&id).expect("snapshot exists");
        assert!(full.data_url.starts_with("data:image/png;base64,"));

        manager.acknowledge(&id);
        assert!(manager.list_pending().is_empty());
        assert!(manager.read_snapshot(&id).is_err());
    }

    #[test]
    fn default_snapshot_state_matches_contract_schema() {
        let temp = std::env::temp_dir().join("hanzo-snapshot-test-empty");
        let state = build_snapshot_state_payload(&temp, true);
        assert_eq!(state.get("mode").and_then(Value::as_str), Some("direct"));
        assert_eq!(
            state.pointer("/shortcut/kind").and_then(Value::as_str),
            Some("both-shift-keys")
        );
        assert_eq!(
            state.get("shortcutRegistered").and_then(Value::as_bool),
            Some(true)
        );
    }
}
