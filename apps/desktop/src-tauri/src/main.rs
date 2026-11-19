#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod auth;
mod backend;
mod bridge;
mod commands;
mod menu;

use crate::auth::{generate_random_hex, read_or_create_host_scope};
use crate::backend::{pick_available_port, resolve_hanzo_home, BackendRuntimeConfig, BackendSupervisor};
use crate::bridge::build_initialization_script;
use crate::commands::{read_persisted_local_env_enabled, AppState};
use std::env;
use std::sync::Mutex;
use tauri::{Emitter, LogicalPosition, Manager, RunEvent, TitleBarStyle, WebviewUrl, WebviewWindowBuilder, WindowEvent};
use tauri_plugin_deep_link::DeepLinkExt;

fn dispatch_deep_link(app: &tauri::AppHandle, url: String) {
    if let Some(state) = app.try_state::<AppState>() {
        let is_ready = state
            .web_links_ready
            .lock()
            .map(|g| *g)
            .unwrap_or(false);
        if is_ready {
            let _ = app.emit("desktop:web-link-open", url);
        } else if let Ok(mut pending) = state.pending_deep_links.lock() {
            pending.push(url);
        }
    }
    if let Some(window) = app.get_webview_window("main") {
        let _ = window.unminimize();
        let _ = window.show();
        let _ = window.set_focus();
    }
}

fn main() {
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
            if let Some(window) = app.get_webview_window("main") {
                let _ = window.unminimize();
                let _ = window.show();
                let _ = window.set_focus();
            }
        }))
        .plugin(tauri_plugin_dialog::init())
        .plugin(tauri_plugin_opener::init())
        .plugin(tauri_plugin_os::init())
        .plugin(tauri_plugin_process::init())
        .plugin(tauri_plugin_shell::init())
        .plugin(tauri_plugin_notification::init())
        .plugin(tauri_plugin_global_shortcut::Builder::new().build())
        .plugin(tauri_plugin_deep_link::init())
        .menu(|app| menu::build_app_menu(app))
        .on_menu_event(|app, event| {
            menu::handle_menu_event(app, event.id().as_ref());
        })
        .setup(move |app| {
            let supervisor = BackendSupervisor::start(runtime_config.clone(), local_enabled);
            app.manage(AppState {
                supervisor,
                bearer_token: tokio::sync::Mutex::new(None),
                local_env_enabled: Mutex::new(local_enabled),
                exposure_mode: Mutex::new("local-only".to_string()),
                update_channel: Mutex::new("latest".to_string()),
                pending_deep_links: Mutex::new(Vec::new()),
                web_links_ready: Mutex::new(false),
                pending_context_menu: Mutex::new(None),
            });

            let app_handle = app.handle().clone();
            app.deep_link().on_open_url(move |event| {
                for url in event.urls() {
                    dispatch_deep_link(&app_handle, url.to_string());
                }
            });

            let webview_url = match &dev_vite_url {
                Some(raw) => WebviewUrl::External(raw.parse().expect("valid VITE_DEV_SERVER_URL")),
                None => WebviewUrl::App("index.html".into()),
            };

            let title = if is_dev { "Hanzo Dev (Dev)" } else { "Hanzo Dev" };
            let mut win_builder = WebviewWindowBuilder::new(app, "main", webview_url)
                .title(title)
                .inner_size(1400.0, 900.0)
                .min_inner_size(840.0, 560.0)
                .initialization_script(&init_script);

            #[cfg(target_os = "macos")]
            {
                win_builder = win_builder
                    .title_bar_style(TitleBarStyle::Overlay)
                    .hidden_title(true)
                    .traffic_light_position(LogicalPosition::new(18.0, 18.0));
            }

            let window = win_builder.build()?;

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
            commands::desktop_open_external,
            commands::desktop_open_system_settings,
            commands::desktop_read_media,
            commands::desktop_discover_ssh_hosts,
            commands::desktop_resolve_ssh_host,
            commands::desktop_discover_lan_engines,
            commands::desktop_probe_remote_editors,
            commands::desktop_get_server_exposure_state,
            commands::desktop_set_server_exposure_mode,
            commands::desktop_get_update_state,
            commands::desktop_set_update_channel,
            commands::desktop_cli_get_state,
            commands::desktop_web_links_set_ready,
            commands::desktop_show_context_menu,
        ]);

    let app = builder
        .build(tauri::generate_context!())
        .expect("error while building Hanzo Dev Tauri application");

    app.run(|app_handle, event| {
        if matches!(event, RunEvent::Exit) {
            if let Some(state) = app_handle.try_state::<AppState>() {
                state.supervisor.shutdown();
            }
        }
    });
}
