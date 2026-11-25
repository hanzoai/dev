#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod auth;
mod backend;
mod bridge;
mod commands;
mod dock_icon;
mod menu;
mod menubar;
mod snapshot;
mod ssh;
mod tray;
mod wsl;

use crate::auth::{generate_random_hex, read_or_create_host_scope};
use crate::backend::{
    pick_available_port, resolve_hanzo_home, BackendRuntimeConfig, BackendSupervisor,
};
use crate::bridge::build_initialization_script;
use crate::commands::{
    purge_legacy_iam_vault_file, read_persisted_local_env_enabled, AppState, UpdateTrackerState,
};
use crate::snapshot::{spawn_interactive_snapshot, SnapshotManager};
use crate::ssh::SshTunnelSupervisor;
use std::env;
use std::sync::Mutex;
use tauri::{
    Emitter, LogicalSize, Manager, RunEvent, WebviewUrl, WebviewWindowBuilder, WindowEvent,
};
#[cfg(target_os = "macos")]
use tauri::{LogicalPosition, TitleBarStyle};
use tauri_plugin_deep_link::DeepLinkExt;
use tauri_plugin_global_shortcut::{GlobalShortcutExt, Shortcut, ShortcutState};

pub const SHORTCUT_NEW_THREAD: &str = "CmdOrCtrl+Shift+I";
pub const SHORTCUT_COMMAND_PALETTE: &str = "CmdOrCtrl+Shift+J";
pub const SHORTCUT_SNAPSHOT: &str = "CmdOrCtrl+Shift+2";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GlobalShortcutAction {
    NewThread,
    CommandPalette,
    SnapShot,
}

fn parse_shortcut(accelerator: &str) -> Option<Shortcut> {
    accelerator.parse::<Shortcut>().ok()
}

fn resolve_global_shortcut_action(shortcut: &Shortcut) -> Option<GlobalShortcutAction> {
    if parse_shortcut(SHORTCUT_NEW_THREAD).as_ref() == Some(shortcut) {
        Some(GlobalShortcutAction::NewThread)
    } else if parse_shortcut(SHORTCUT_COMMAND_PALETTE).as_ref() == Some(shortcut) {
        Some(GlobalShortcutAction::CommandPalette)
    } else if parse_shortcut(SHORTCUT_SNAPSHOT).as_ref() == Some(shortcut) {
        Some(GlobalShortcutAction::SnapShot)
    } else {
        None
    }
}

pub(crate) fn reveal_main_window(app: &tauri::AppHandle) {
    if let Some(window) = app.get_webview_window("main") {
        let _ = window.unminimize();
        let _ = window.show();
        let _ = window.set_focus();
    }
}

#[cfg(windows)]
pub(crate) fn hide_std_console(cmd: &mut std::process::Command) {
    use std::os::windows::process::CommandExt;
    const CREATE_NO_WINDOW: u32 = 0x0800_0000;
    cmd.creation_flags(CREATE_NO_WINDOW);
}

#[cfg(not(windows))]
pub(crate) fn hide_std_console(_cmd: &mut std::process::Command) {}

#[cfg(windows)]
pub(crate) fn hide_tokio_console(cmd: &mut tokio::process::Command) {
    const CREATE_NO_WINDOW: u32 = 0x0800_0000;
    cmd.creation_flags(CREATE_NO_WINDOW);
}

#[cfg(not(windows))]
pub(crate) fn hide_tokio_console(_cmd: &mut tokio::process::Command) {}

#[tauri::command]
fn desktop_set_window_mode(window: tauri::WebviewWindow, mode: String) -> Result<(), String> {
    if mode == "login" {
        let _ = window.set_min_size(Some(LogicalSize::new(400.0, 560.0)));
        window
            .set_size(LogicalSize::new(460.0, 640.0))
            .map_err(|e| e.to_string())?;
        let _ = window.center();
    } else {
        let _ = window.set_min_size(Some(LogicalSize::new(900.0, 600.0)));
        window
            .set_size(LogicalSize::new(1360.0, 860.0))
            .map_err(|e| e.to_string())?;
        let _ = window.center();
    }
    Ok(())
}

fn dispatch_global_shortcut(app: &tauri::AppHandle, shortcut: &Shortcut) {
    match resolve_global_shortcut_action(shortcut) {
        Some(GlobalShortcutAction::NewThread) => {
            reveal_main_window(app);
            let _ = app.emit("desktop:menu-action", "chat.new".to_string());
        }
        Some(GlobalShortcutAction::CommandPalette) => {
            reveal_main_window(app);
            let _ = app.emit("desktop:menu-action", "commandPalette.toggle".to_string());
        }
        Some(GlobalShortcutAction::SnapShot) => {
            spawn_interactive_snapshot(app.clone());
        }
        None => {}
    }
}

#[cfg(target_os = "windows")]
fn apply_windows_dwm_chrome(window: &tauri::WebviewWindow) {
    #[link(name = "dwmapi")]
    extern "system" {
        fn DwmSetWindowAttribute(
            hwnd: *mut std::ffi::c_void,
            dw_attribute: u32,
            pv_attribute: *const std::ffi::c_void,
            cb_attribute: u32,
        ) -> i32;
    }
    const DWMWA_USE_IMMERSIVE_DARK_MODE: u32 = 20;
    const DWMWA_WINDOW_CORNER_PREFERENCE: u32 = 33;
    const DWMWCP_ROUND: u32 = 2;

    if let Ok(hwnd) = window.hwnd() {
        let raw = hwnd.0 as *mut std::ffi::c_void;
        let dark: u32 = 1;
        let corner: u32 = DWMWCP_ROUND;
        unsafe {
            let _ = DwmSetWindowAttribute(
                raw,
                DWMWA_USE_IMMERSIVE_DARK_MODE,
                &dark as *const u32 as *const std::ffi::c_void,
                std::mem::size_of::<u32>() as u32,
            );
            let _ = DwmSetWindowAttribute(
                raw,
                DWMWA_WINDOW_CORNER_PREFERENCE,
                &corner as *const u32 as *const std::ffi::c_void,
                std::mem::size_of::<u32>() as u32,
            );
        }
    }
}

fn dispatch_deep_link(app: &tauri::AppHandle, url: String) {
    if let Some(state) = app.try_state::<AppState>() {
        let is_ready = state.web_links_ready.lock().map(|g| *g).unwrap_or(false);
        if is_ready {
            let _ = app.emit("desktop:web-link-open", url);
        } else if let Ok(mut pending) = state.pending_deep_links.lock() {
            pending.push(url);
        }
    }
    reveal_main_window(app);
}

fn main() {
    #[cfg(target_os = "linux")]
    wsl::configure_linux_and_wslg_env();

    let dev_vite_url = env::var("VITE_DEV_SERVER_URL")
        .ok()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty());
    let is_dev = dev_vite_url.is_some();

    let preferred_port = env::var("HANZO_DEV_PORT")
        .ok()
        .and_then(|raw| raw.trim().parse::<u16>().ok())
        .unwrap_or(if is_dev { 13773 } else { 3773 });
    let port = pick_available_port(preferred_port);

    let hanzo_home = resolve_hanzo_home();
    let state_dir = hanzo_home.join(if is_dev { "dev" } else { "userdata" });
    purge_legacy_iam_vault_file(&state_dir);
    let bootstrap_secret = generate_random_hex(32);
    let host_scope = read_or_create_host_scope(&state_dir);
    let local_enabled = read_persisted_local_env_enabled(&state_dir);

    let runtime_config = BackendRuntimeConfig {
        port,
        bind_host: "127.0.0.1".to_string(),
        http_base_url: format!("http://127.0.0.1:{port}"),
        ws_base_url: format!("ws://127.0.0.1:{port}"),
        hanzo_home,
        state_dir,
        bootstrap_secret,
        host_scope,
        is_dev,
    };

    let init_script = build_initialization_script(&runtime_config, local_enabled);

    let builder = tauri::Builder::default()
        .plugin(tauri_plugin_single_instance::init(|app, argv, _cwd| {
            for arg in argv {
                if arg.starts_with("hanzo://")
                    || arg.starts_with("hanzodev://")
                    || arg.starts_with("hanzo-dev://")
                {
                    dispatch_deep_link(app, arg);
                }
            }
            reveal_main_window(app);
        }))
        .plugin(tauri_plugin_dialog::init())
        .plugin(tauri_plugin_opener::init())
        .plugin(tauri_plugin_os::init())
        .plugin(tauri_plugin_process::init())
        .plugin(tauri_plugin_shell::init())
        .plugin(tauri_plugin_notification::init())
        .plugin(
            tauri_plugin_global_shortcut::Builder::new()
                .with_handler(|app, shortcut, event| {
                    if event.state == ShortcutState::Pressed {
                        dispatch_global_shortcut(app, shortcut);
                    }
                })
                .build(),
        )
        .plugin(tauri_plugin_deep_link::init());

    #[cfg(target_os = "macos")]
    let builder = builder
        .menu(menu::build_app_menu)
        .on_menu_event(|app, event| {
            menu::handle_menu_event(app, event.id().as_ref());
        });

    let builder = builder
        .setup(move |app| {
            dock_icon::apply_saved_mode_on_startup(app.handle());
            let supervisor = BackendSupervisor::start(runtime_config.clone(), local_enabled);
            let snapshots = SnapshotManager::new();
            let shortcut_manager = app.global_shortcut();
            let _ = shortcut_manager.register(SHORTCUT_NEW_THREAD);
            let _ = shortcut_manager.register(SHORTCUT_COMMAND_PALETTE);
            let snapshot_registered = shortcut_manager.register(SHORTCUT_SNAPSHOT).is_ok();
            snapshots.set_shortcut_registered(snapshot_registered);

            app.manage(AppState {
                supervisor,
                bearer_token: tokio::sync::Mutex::new(None),
                iam_vault: tokio::sync::Mutex::new(None),
                local_env_enabled: Mutex::new(local_enabled),
                exposure_mode: Mutex::new("local-only".to_string()),
                update_tracker: Mutex::new(UpdateTrackerState::default()),
                snapshots,
                ssh_tunnels: SshTunnelSupervisor::new(),
                pending_deep_links: Mutex::new(Vec::new()),
                web_links_ready: Mutex::new(false),
            });

            let app_handle = app.handle().clone();
            app.deep_link().on_open_url(move |event| {
                for url in event.urls() {
                    dispatch_deep_link(&app_handle, url.to_string());
                }
            });

            let webview_url = match &dev_vite_url {
                Some(raw) => WebviewUrl::External(raw.parse()?),
                None => WebviewUrl::App("index.html".into()),
            };

            let title = if is_dev {
                "Hanzo Dev (Dev)"
            } else {
                "Hanzo Dev"
            };
            let mut win_builder = WebviewWindowBuilder::new(app, "main", webview_url)
                .title(title)
                .inner_size(1360.0, 860.0)
                .min_inner_size(400.0, 560.0)
                .background_color(tauri::window::Color(0, 0, 0, 255))
                .initialization_script(&init_script);

            #[cfg(target_os = "macos")]
            {
                win_builder = win_builder
                    .title_bar_style(TitleBarStyle::Overlay)
                    .hidden_title(true)
                    .traffic_light_position(LogicalPosition::new(16.0, 22.0));
            }

            #[cfg(target_os = "linux")]
            {
                win_builder = win_builder
                    .decorations(false)
                    .transparent(true)
                    .background_color(tauri::window::Color(0, 0, 0, 0));
            }

            #[cfg(all(not(target_os = "macos"), not(target_os = "linux")))]
            {
                win_builder = win_builder.decorations(false);
            }

            let window = win_builder.build()?;
            #[cfg(target_os = "windows")]
            apply_windows_dwm_chrome(&window);
            menubar::start(app.handle());
            tray::start(app.handle());

            let win_clone = window.clone();
            let app_for_events = app.handle().clone();
            window.on_window_event(move |event| {
                if matches!(event, WindowEvent::Resized(_)) {
                    if let Ok(full) = win_clone.is_fullscreen() {
                        let _ = app_for_events.emit("desktop:window-fullscreen", full);
                    }
                }
            });

            Ok(())
        })
        .invoke_handler(tauri::generate_handler![
            commands::desktop_get_iam_vault,
            commands::desktop_set_iam_vault,
            commands::desktop_get_local_environment_bearer_token,
            commands::desktop_set_local_environment_enabled,
            commands::desktop_get_host_environment_id,
            commands::desktop_get_client_settings,
            commands::desktop_set_client_settings,
            commands::desktop_get_connection_catalog,
            commands::desktop_set_connection_catalog,
            commands::desktop_pick_folder,
            commands::desktop_pick_project_favicon,
            commands::desktop_pick_theme_files,
            commands::desktop_set_theme,
            commands::desktop_reveal_window,
            commands::desktop_start_dragging,
            commands::desktop_toggle_maximize,
            desktop_set_window_mode,
            commands::desktop_open_external,
            commands::desktop_open_system_settings,
            commands::desktop_read_media,
            commands::desktop_discover_ssh_hosts,
            commands::desktop_resolve_ssh_host,
            commands::desktop_ensure_ssh_environment,
            commands::desktop_disconnect_ssh_environment,
            commands::desktop_get_snapshot_state,
            commands::desktop_request_snapshot_permissions,
            commands::desktop_setup_snapshot,
            commands::desktop_capture_window,
            commands::desktop_check_snapshot_shortcut,
            commands::desktop_set_snapshot_shortcut_suppressed,
            commands::desktop_list_pending_snapshots,
            commands::desktop_read_snapshot,
            commands::desktop_acknowledge_snapshot,
            commands::desktop_discover_lan_engines,
            commands::desktop_probe_remote_editors,
            commands::desktop_get_server_exposure_state,
            commands::desktop_set_server_exposure_mode,
            commands::desktop_get_update_state,
            commands::desktop_set_update_channel,
            commands::desktop_check_for_update,
            commands::desktop_download_update,
            commands::desktop_install_update,
            commands::desktop_cli_get_state,
            commands::desktop_cli_install,
            commands::desktop_cli_uninstall,
            commands::desktop_cli_add_to_path,
            commands::desktop_preview_open_devtools,
            commands::desktop_preview_capture_screenshot,
            commands::desktop_preview_save_recording,
            commands::desktop_web_links_set_ready,
            wsl::desktop_get_wsl_state,
            wsl::desktop_set_wsl_backend_enabled,
            wsl::desktop_set_wsl_distro,
            wsl::desktop_set_wsl_only,
            wsl::desktop_to_local_backend_path,
            dock_icon::get_dock_icon_mode,
            dock_icon::set_dock_icon_mode,
        ]);

    let app = builder
        .build(tauri::generate_context!())
        .expect("error while building Hanzo Dev Tauri application");

    app.run(|app_handle, event| match event {
        RunEvent::Exit => {
            if let Some(state) = app_handle.try_state::<AppState>() {
                state.ssh_tunnels.shutdown_all();
                state.supervisor.shutdown();
            }
        }
        #[cfg(target_os = "macos")]
        RunEvent::Reopen {
            has_visible_windows,
            ..
        } => {
            if !has_visible_windows {
                reveal_main_window(app_handle);
            } else if let Some(window) = app_handle.get_webview_window("main") {
                let _ = window.unminimize();
                let _ = window.set_focus();
            }
        }
        _ => {}
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolves_all_registered_global_shortcuts() {
        let new_thread = parse_shortcut(SHORTCUT_NEW_THREAD).expect("parse new thread shortcut");
        let palette =
            parse_shortcut(SHORTCUT_COMMAND_PALETTE).expect("parse command palette shortcut");
        let snapshot = parse_shortcut(SHORTCUT_SNAPSHOT).expect("parse snapshot shortcut");
        let unrelated = parse_shortcut("CmdOrCtrl+Shift+9").expect("parse unrelated shortcut");

        assert_eq!(
            resolve_global_shortcut_action(&new_thread),
            Some(GlobalShortcutAction::NewThread)
        );
        assert_eq!(
            resolve_global_shortcut_action(&palette),
            Some(GlobalShortcutAction::CommandPalette)
        );
        assert_eq!(
            resolve_global_shortcut_action(&snapshot),
            Some(GlobalShortcutAction::SnapShot)
        );
        assert_eq!(resolve_global_shortcut_action(&unrelated), None);
    }

    #[test]
    fn default_capability_grants_frameless_window_controls() {
        let cap = include_str!("../capabilities/default.json");
        for perm in [
            "core:window:allow-start-dragging",
            "core:window:allow-minimize",
            "core:window:allow-toggle-maximize",
            "core:window:allow-is-maximized",
            "core:window:allow-close",
        ] {
            assert!(cap.contains(perm), "missing capability {perm}");
        }
    }
}
