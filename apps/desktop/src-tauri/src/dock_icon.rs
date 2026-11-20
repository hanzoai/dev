use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use tauri::{AppHandle, Emitter, Manager, Runtime};

pub const DOCK_ICON_CONFIG_FILE: &str = "dock-icon-mode.json";
pub const DOCK_ICON_CHANGED_EVENT: &str = "desktop:dock-icon-mode-changed";

const ICON_BRANDED_PNG: &[u8] = include_bytes!("../icons/icon-macos.png");
const ICON_MINIMAL_PNG: &[u8] = include_bytes!("../icons/icon-macos-minimal.png");

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum DockIconMode {
    #[default]
    Branded,
    Minimal,
}

impl DockIconMode {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Branded => "branded",
            Self::Minimal => "minimal",
        }
    }

    pub fn parse(raw: &str) -> Result<Self, String> {
        match raw.trim().to_ascii_lowercase().as_str() {
            "branded" => Ok(Self::Branded),
            "minimal" => Ok(Self::Minimal),
            other => Err(format!(
                "Invalid dock icon mode '{other}'; expected 'branded' or 'minimal'"
            )),
        }
    }

    pub fn icon_png_bytes(self) -> &'static [u8] {
        match self {
            Self::Branded => ICON_BRANDED_PNG,
            Self::Minimal => ICON_MINIMAL_PNG,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
struct PersistedDockIconConfig {
    mode: DockIconMode,
}

pub fn read_mode_from_path(path: &Path) -> DockIconMode {
    let raw = match fs::read_to_string(path) {
        Ok(content) => content,
        Err(_) => return DockIconMode::default(),
    };
    if let Ok(parsed) = serde_json::from_str::<PersistedDockIconConfig>(&raw) {
        return parsed.mode;
    }
    if let Ok(raw_mode) = serde_json::from_str::<String>(&raw) {
        if let Ok(mode) = DockIconMode::parse(&raw_mode) {
            return mode;
        }
    }
    DockIconMode::default()
}

pub fn write_mode_to_path(path: &Path, mode: DockIconMode) -> Result<(), String> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .map_err(|err| format!("Failed to create dock icon config directory: {err}"))?;
    }
    let payload = PersistedDockIconConfig { mode };
    let serialized = serde_json::to_string_pretty(&payload)
        .map_err(|err| format!("Failed to serialize dock icon mode: {err}"))?;
    fs::write(path, format!("{serialized}\n"))
        .map_err(|err| format!("Failed to persist dock icon mode: {err}"))
}

fn config_path<R: Runtime>(app: &AppHandle<R>) -> Option<PathBuf> {
    app.path()
        .app_config_dir()
        .ok()
        .map(|dir| dir.join(DOCK_ICON_CONFIG_FILE))
}

pub fn load_mode<R: Runtime>(app: &AppHandle<R>) -> DockIconMode {
    config_path(app)
        .map(|path| read_mode_from_path(&path))
        .unwrap_or_default()
}

#[cfg(target_os = "macos")]
fn apply_macos_dock_icon_on_main_thread(
    mtm: objc2::MainThreadMarker,
    mode: DockIconMode,
) -> Result<(), String> {
    use objc2::AnyThread;
    use objc2_app_kit::{NSApplication, NSImage};
    use objc2_foundation::NSData;

    let data = NSData::with_bytes(mode.icon_png_bytes());
    let image = NSImage::initWithData(NSImage::alloc(), &data)
        .ok_or_else(|| "Failed to decode embedded macOS Dock icon PNG".to_string())?;
    let ns_app = NSApplication::sharedApplication(mtm);
    // SAFETY: `image` is a valid non-null NSImage constructed on the main thread.
    unsafe {
        ns_app.setApplicationIconImage(Some(&image));
    }
    Ok(())
}

#[cfg(target_os = "macos")]
pub fn apply_mode<R: Runtime>(app: &AppHandle<R>, mode: DockIconMode) -> Result<(), String> {
    if let Some(mtm) = objc2::MainThreadMarker::new() {
        return apply_macos_dock_icon_on_main_thread(mtm, mode);
    }

    let (tx, rx) = std::sync::mpsc::sync_channel(1);
    app.run_on_main_thread(move || {
        let result = match objc2::MainThreadMarker::new() {
            Some(mtm) => apply_macos_dock_icon_on_main_thread(mtm, mode),
            None => Err("Dock icon update did not execute on the macOS main thread".to_string()),
        };
        let _ = tx.send(result);
    })
    .map_err(|err| format!("Failed to schedule Dock icon update on main thread: {err}"))?;

    rx.recv()
        .map_err(|err| format!("Dock icon main-thread task cancelled: {err}"))?
}

#[cfg(not(target_os = "macos"))]
pub fn apply_mode<R: Runtime>(_app: &AppHandle<R>, _mode: DockIconMode) -> Result<(), String> {
    Ok(())
}

pub fn apply_saved_mode_on_startup<R: Runtime>(app: &AppHandle<R>) -> DockIconMode {
    let mode = load_mode(app);
    if let Err(err) = apply_mode(app, mode) {
        log::warn!("Failed to apply saved Dock icon mode ({}): {err}", mode.as_str());
    }
    crate::menu::sync_dock_icon_menu(app, mode);
    mode
}

pub fn set_and_persist_mode<R: Runtime>(
    app: &AppHandle<R>,
    mode: DockIconMode,
) -> Result<DockIconMode, String> {
    apply_mode(app, mode)?;
    if let Some(path) = config_path(app) {
        write_mode_to_path(&path, mode)?;
    }
    crate::menu::sync_dock_icon_menu(app, mode);
    let _ = app.emit(DOCK_ICON_CHANGED_EVENT, mode.as_str());
    Ok(mode)
}

#[tauri::command]
pub fn get_dock_icon_mode(app: AppHandle) -> String {
    load_mode(&app).as_str().to_string()
}

#[tauri::command]
pub fn set_dock_icon_mode(app: AppHandle, mode: String) -> Result<String, String> {
    let parsed = DockIconMode::parse(&mode)?;
    let applied = set_and_persist_mode(&app, parsed)?;
    Ok(applied.as_str().to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn unique_temp_dir(label: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time after epoch")
            .as_nanos();
        std::env::temp_dir().join(format!(
            "hanzo-dev-dock-icon-{label}-{}-{nanos}",
            std::process::id()
        ))
    }

    #[test]
    fn parses_valid_modes_case_insensitively_and_rejects_unknown() {
        assert_eq!(DockIconMode::parse("branded"), Ok(DockIconMode::Branded));
        assert_eq!(DockIconMode::parse("minimal"), Ok(DockIconMode::Minimal));
        assert_eq!(DockIconMode::parse(" BRANDED "), Ok(DockIconMode::Branded));
        assert_eq!(DockIconMode::parse("Minimal\n"), Ok(DockIconMode::Minimal));
        assert!(DockIconMode::parse("").is_err());
        assert!(DockIconMode::parse("classic").is_err());
        assert_eq!(DockIconMode::default(), DockIconMode::Branded);
        assert_eq!(DockIconMode::Branded.as_str(), "branded");
        assert_eq!(DockIconMode::Minimal.as_str(), "minimal");
    }

    #[test]
    fn embedded_png_assets_are_valid_and_distinct() {
        const PNG_SIGNATURE: &[u8; 8] = b"\x89PNG\r\n\x1a\n";
        let branded = DockIconMode::Branded.icon_png_bytes();
        let minimal = DockIconMode::Minimal.icon_png_bytes();
        assert!(branded.starts_with(PNG_SIGNATURE));
        assert!(minimal.starts_with(PNG_SIGNATURE));
        assert_ne!(branded, minimal);
    }

    #[test]
    fn persists_and_reads_back_both_modes() {
        let dir = unique_temp_dir("roundtrip");
        let path = dir.join(DOCK_ICON_CONFIG_FILE);

        write_mode_to_path(&path, DockIconMode::Minimal).expect("write minimal mode");
        assert_eq!(read_mode_from_path(&path), DockIconMode::Minimal);
        let raw = fs::read_to_string(&path).expect("read config file");
        assert!(raw.contains("\"mode\": \"minimal\""));

        write_mode_to_path(&path, DockIconMode::Branded).expect("write branded mode");
        assert_eq!(read_mode_from_path(&path), DockIconMode::Branded);

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn defaults_to_branded_when_missing_or_invalid() {
        let dir = unique_temp_dir("fallback");
        let missing = dir.join(DOCK_ICON_CONFIG_FILE);
        assert_eq!(read_mode_from_path(&missing), DockIconMode::Branded);

        fs::create_dir_all(&dir).expect("create temp dir");
        fs::write(&missing, "{not-json").expect("write corrupt json");
        assert_eq!(read_mode_from_path(&missing), DockIconMode::Branded);

        fs::write(&missing, "{\"mode\": \"unknown\"}").expect("write unknown mode");
        assert_eq!(read_mode_from_path(&missing), DockIconMode::Branded);

        fs::write(&missing, "\"minimal\"").expect("write bare string fallback");
        assert_eq!(read_mode_from_path(&missing), DockIconMode::Minimal);

        let _ = fs::remove_dir_all(&dir);
    }
}
