use crate::commands::AppState;
use serde::{Deserialize, Serialize};
use std::path::Path;
use tauri::State;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct DesktopWslDistro {
    pub name: String,
    pub is_default: bool,
    pub version: u8,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct DesktopWslState {
    pub enabled: bool,
    pub distro: Option<String>,
    pub available: bool,
    pub wsl_only: bool,
    pub distros: Vec<DesktopWslDistro>,
    pub preflight_error: Option<String>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct PersistedWslSettings {
    enabled: bool,
    distro: Option<String>,
    wsl_only: bool,
}

#[cfg(target_os = "linux")]
pub fn is_running_in_wsl() -> bool {
    if std::env::var_os("WSL_DISTRO_NAME").is_some() || std::env::var_os("WSL_INTEROP").is_some() {
        return true;
    }
    std::fs::read_to_string("/proc/version")
        .map(|v| v.to_ascii_lowercase().contains("microsoft"))
        .unwrap_or(false)
}

#[cfg(not(target_os = "linux"))]
pub fn is_running_in_wsl() -> bool {
    false
}

#[cfg(target_os = "linux")]
pub fn configure_linux_and_wslg_env() {
    if std::env::var_os("WEBKIT_DISABLE_DMABUF_RENDERER").is_none() {
        std::env::set_var("WEBKIT_DISABLE_DMABUF_RENDERER", "1");
    }
}

fn read_persisted_wsl_settings(state_dir: &Path) -> PersistedWslSettings {
    let path = state_dir.join("wsl-settings.json");
    std::fs::read_to_string(path)
        .ok()
        .and_then(|raw| serde_json::from_str::<PersistedWslSettings>(&raw).ok())
        .unwrap_or_default()
}

fn write_persisted_wsl_settings(state_dir: &Path, settings: &PersistedWslSettings) {
    let _ = std::fs::create_dir_all(state_dir);
    if let Ok(json) = serde_json::to_string_pretty(settings) {
        let _ = std::fs::write(state_dir.join("wsl-settings.json"), json);
    }
}

pub fn decode_wsl_output(bytes: &[u8]) -> String {
    if bytes.len() >= 2 && (bytes[1] == 0 || (bytes[0] == 0xFF && bytes[1] == 0xFE)) {
        let offset = if bytes[0] == 0xFF && bytes[1] == 0xFE {
            2
        } else {
            0
        };
        let units: Vec<u16> = bytes[offset..]
            .chunks_exact(2)
            .map(|c| u16::from_le_bytes([c[0], c[1]]))
            .collect();
        return String::from_utf16_lossy(&units);
    }
    String::from_utf8_lossy(bytes).into_owned()
}

pub fn parse_wsl_list_verbose(raw: &str) -> Vec<DesktopWslDistro> {
    let mut distros = Vec::new();
    for line in raw.lines() {
        let trimmed = line.trim().trim_start_matches('\u{feff}');
        if trimmed.is_empty() {
            continue;
        }
        let is_default = trimmed.starts_with('*');
        let body = trimmed.trim_start_matches('*').trim();
        let tokens: Vec<&str> = body.split_whitespace().collect();
        if tokens.len() < 3 {
            continue;
        }
        if tokens[0].eq_ignore_ascii_case("NAME") && tokens[1].eq_ignore_ascii_case("STATE") {
            continue;
        }
        let Some(ver_raw) = tokens.last() else {
            continue;
        };
        let Ok(ver_num) = ver_raw.parse::<u8>() else {
            continue;
        };
        let version = if ver_num == 1 { 1 } else { 2 };
        let name = tokens[..tokens.len() - 2].join(" ");
        if !name.is_empty() {
            distros.push(DesktopWslDistro {
                name,
                is_default,
                version,
            });
        }
    }
    distros
}

#[cfg(target_os = "windows")]
fn discover_windows_wsl_distros() -> (Vec<DesktopWslDistro>, Option<String>) {
    use std::os::windows::process::CommandExt;
    const CREATE_NO_WINDOW: u32 = 0x08000000;
    match std::process::Command::new("wsl.exe")
        .args(["-l", "-v"])
        .creation_flags(CREATE_NO_WINDOW)
        .output()
    {
        Ok(out) if out.status.success() => {
            let text = decode_wsl_output(&out.stdout);
            (parse_wsl_list_verbose(&text), None)
        }
        Ok(out) => {
            let err = decode_wsl_output(&out.stderr);
            let msg = err.trim();
            (
                Vec::new(),
                if msg.is_empty() {
                    None
                } else {
                    Some(msg.to_string())
                },
            )
        }
        Err(_) => (Vec::new(), None),
    }
}

#[cfg(not(target_os = "windows"))]
fn discover_windows_wsl_distros() -> (Vec<DesktopWslDistro>, Option<String>) {
    if is_running_in_wsl() {
        let name = std::env::var("WSL_DISTRO_NAME").unwrap_or_else(|_| "Ubuntu".to_string());
        return (
            vec![DesktopWslDistro {
                name,
                is_default: true,
                version: 2,
            }],
            None,
        );
    }
    (Vec::new(), None)
}

fn build_wsl_state(state_dir: &Path) -> DesktopWslState {
    let persisted = read_persisted_wsl_settings(state_dir);
    let (distros, err) = discover_windows_wsl_distros();
    let available = !distros.is_empty();
    let preflight_error = if persisted.wsl_only { None } else { err };
    DesktopWslState {
        enabled: persisted.enabled && available,
        distro: persisted.distro,
        available,
        wsl_only: persisted.wsl_only,
        distros,
        preflight_error,
    }
}

pub fn convert_windows_path_for_wsl(input: &str) -> Option<String> {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return None;
    }
    if trimmed.starts_with('/') {
        return Some(trimmed.to_string());
    }
    let normalized = trimmed.replace('\\', "/");
    for prefix in ["//wsl.localhost/", "//wsl$/"] {
        if let Some(rest) = normalized.strip_prefix(prefix) {
            if let Some((_distro, subpath)) = rest.split_once('/') {
                return Some(format!("/{}", subpath.trim_start_matches('/')));
            }
            return Some("/".to_string());
        }
    }
    let bytes = normalized.as_bytes();
    if bytes.len() >= 2 && bytes[0].is_ascii_alphabetic() && bytes[1] == b':' {
        let drive = (bytes[0] as char).to_ascii_lowercase();
        let rest = normalized[2..].trim_start_matches('/');
        return if rest.is_empty() {
            Some(format!("/mnt/{drive}"))
        } else {
            Some(format!("/mnt/{drive}/{rest}"))
        };
    }
    None
}

pub fn active_windows_wsl_target(state_dir: &Path) -> Option<Option<String>> {
    if !cfg!(target_os = "windows") {
        return None;
    }
    let state = build_wsl_state(state_dir);
    if !state.enabled || !state.available {
        return None;
    }
    Some(state.distro)
}

#[tauri::command]
pub fn desktop_get_wsl_state(state: State<'_, AppState>) -> DesktopWslState {
    build_wsl_state(&state.supervisor.config.state_dir)
}

#[tauri::command]
pub fn desktop_set_wsl_backend_enabled(
    state: State<'_, AppState>,
    enabled: bool,
) -> DesktopWslState {
    let dir = &state.supervisor.config.state_dir;
    let mut settings = read_persisted_wsl_settings(dir);
    settings.enabled = enabled;
    write_persisted_wsl_settings(dir, &settings);
    let next = build_wsl_state(dir);
    state.supervisor.restart_active_worker();
    next
}

#[tauri::command]
pub fn desktop_set_wsl_distro(
    state: State<'_, AppState>,
    distro: Option<String>,
) -> DesktopWslState {
    let dir = &state.supervisor.config.state_dir;
    let mut settings = read_persisted_wsl_settings(dir);
    settings.distro = distro
        .map(|d| d.trim().to_string())
        .filter(|d| !d.is_empty());
    write_persisted_wsl_settings(dir, &settings);
    let next = build_wsl_state(dir);
    if next.enabled {
        state.supervisor.restart_active_worker();
    }
    next
}

#[tauri::command]
pub fn desktop_set_wsl_only(state: State<'_, AppState>, enabled: bool) -> DesktopWslState {
    let dir = &state.supervisor.config.state_dir;
    let mut settings = read_persisted_wsl_settings(dir);
    settings.wsl_only = enabled;
    if enabled {
        settings.enabled = true;
    }
    write_persisted_wsl_settings(dir, &settings);
    build_wsl_state(dir)
}

#[tauri::command]
pub fn desktop_to_local_backend_path(backend_id: String, windows_path: String) -> Option<String> {
    if backend_id == "wsl" || backend_id.starts_with("wsl:") || is_running_in_wsl() {
        return convert_windows_path_for_wsl(&windows_path);
    }
    Some(windows_path)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_wsl_list_verbose_utf8_and_utf16() {
        let sample = "  NAME            STATE           VERSION\r\n* Ubuntu-24.04    Running         2\r\n  Debian          Stopped         2\r\n";
        let utf16: Vec<u8> = sample
            .encode_utf16()
            .flat_map(|u| u.to_le_bytes())
            .collect();
        let decoded = decode_wsl_output(&utf16);
        let distros = parse_wsl_list_verbose(&decoded);
        assert_eq!(
            distros,
            vec![
                DesktopWslDistro {
                    name: "Ubuntu-24.04".to_string(),
                    is_default: true,
                    version: 2,
                },
                DesktopWslDistro {
                    name: "Debian".to_string(),
                    is_default: false,
                    version: 2,
                },
            ]
        );
    }

    #[test]
    fn converts_windows_and_unc_wsl_paths() {
        assert_eq!(
            convert_windows_path_for_wsl(r"C:\Users\z\work\app"),
            Some("/mnt/c/Users/z/work/app".to_string())
        );
        assert_eq!(
            convert_windows_path_for_wsl(r"\\wsl.localhost\Ubuntu-24.04\home\z\project"),
            Some("/home/z/project".to_string())
        );
        assert_eq!(
            convert_windows_path_for_wsl("/home/z/native"),
            Some("/home/z/native".to_string())
        );
    }
}
