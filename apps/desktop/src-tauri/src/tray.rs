use crate::commands::AppState;
use tauri::menu::{Menu, MenuItem, PredefinedMenuItem};
use tauri::tray::{MouseButton, MouseButtonState, TrayIconBuilder, TrayIconEvent};
use tauri::{AppHandle, Manager};

const TRAY_ID_OPEN: &str = "tray:open";
const TRAY_ID_HANZOD_START: &str = "tray:hanzod-start";
const TRAY_ID_HANZOD_RESTART: &str = "tray:hanzod-restart";
const TRAY_ID_HANZOD_STOP: &str = "tray:hanzod-stop";
const TRAY_ID_QUIT: &str = "tray:quit";

pub fn start(app: &AppHandle) {
    // On macOS, HanzoMenubar.app already owns the native status-bar item when bundled;
    // on Windows and Linux (and macOS dev builds without HanzoMenubar), install the Tauri tray.
    #[cfg(target_os = "macos")]
    if std::env::current_exe()
        .ok()
        .and_then(|p| {
            p.parent()
                .map(|d| d.join("../Resources/resources/menubar/HanzoMenubar.app"))
        })
        .is_some_and(|p| p.is_dir())
    {
        return;
    }

    let Ok(open_item) = MenuItem::with_id(app, TRAY_ID_OPEN, "Open Hanzo Dev", true, None::<&str>)
    else {
        return;
    };
    let Ok(sep1) = PredefinedMenuItem::separator(app) else {
        return;
    };
    let Ok(start_item) = MenuItem::with_id(
        app,
        TRAY_ID_HANZOD_START,
        "Start hanzod",
        true,
        None::<&str>,
    ) else {
        return;
    };
    let Ok(restart_item) = MenuItem::with_id(
        app,
        TRAY_ID_HANZOD_RESTART,
        "Restart hanzod",
        true,
        None::<&str>,
    ) else {
        return;
    };
    let Ok(stop_item) =
        MenuItem::with_id(app, TRAY_ID_HANZOD_STOP, "Stop hanzod", true, None::<&str>)
    else {
        return;
    };
    let Ok(sep2) = PredefinedMenuItem::separator(app) else {
        return;
    };
    let Ok(quit_item) = MenuItem::with_id(app, TRAY_ID_QUIT, "Quit Hanzo Dev", true, None::<&str>)
    else {
        return;
    };

    let Ok(menu) = Menu::with_items(
        app,
        &[
            &open_item,
            &sep1,
            &start_item,
            &restart_item,
            &stop_item,
            &sep2,
            &quit_item,
        ],
    ) else {
        return;
    };

    let mut builder = TrayIconBuilder::with_id("hanzo-dev-tray")
        .tooltip("Hanzo Dev")
        .menu(&menu)
        .show_menu_on_left_click(false)
        .on_menu_event(|app_handle, event| match event.id().as_ref() {
            TRAY_ID_OPEN => crate::reveal_main_window(app_handle),
            TRAY_ID_HANZOD_START => {
                tauri::async_runtime::spawn(async {
                    let _ = crate::backend::start_hanzod_daemon().await;
                });
            }
            TRAY_ID_HANZOD_RESTART => {
                let handle = app_handle.clone();
                tauri::async_runtime::spawn(async move {
                    crate::backend::stop_hanzod_daemon();
                    let _ = crate::backend::start_hanzod_daemon().await;
                    if let Some(state) = handle.try_state::<AppState>() {
                        state.supervisor.restart_active_worker();
                    }
                });
            }
            TRAY_ID_HANZOD_STOP => {
                tauri::async_runtime::spawn(async {
                    crate::backend::stop_hanzod_daemon();
                });
            }
            TRAY_ID_QUIT => {
                app_handle.exit(0);
            }
            _ => {}
        })
        .on_tray_icon_event(|tray, event| {
            if let TrayIconEvent::Click {
                button: MouseButton::Left,
                button_state: MouseButtonState::Up,
                ..
            } = event
            {
                crate::reveal_main_window(tray.app_handle());
            }
        });

    if let Some(icon) = app.default_window_icon() {
        builder = builder.icon(icon.clone());
    }

    let _ = builder.build(app);
}
