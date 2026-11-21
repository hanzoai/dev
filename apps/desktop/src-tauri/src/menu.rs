use crate::dock_icon::{self, DockIconMode};
#[cfg(target_os = "macos")]
use tauri::menu::{CheckMenuItem, MenuItemKind};
use tauri::menu::{Menu, MenuItem, PredefinedMenuItem, Submenu};
use tauri::{AppHandle, Emitter, Runtime};

pub const MENU_ID_DOCK_ICON_BRANDED: &str = "dock-icon:branded";
pub const MENU_ID_DOCK_ICON_MINIMAL: &str = "dock-icon:minimal";

pub fn build_app_menu(app: &AppHandle) -> tauri::Result<Menu<tauri::Wry>> {
    let menu = Menu::new(app)?;

    #[cfg(target_os = "macos")]
    {
        let app_menu = Submenu::new(app, "Hanzo Dev", true)?;
        app_menu.append(&PredefinedMenuItem::about(
            app,
            Some("About Hanzo Dev"),
            None,
        )?)?;
        app_menu.append(&PredefinedMenuItem::separator(app)?)?;
        app_menu.append(&MenuItem::with_id(
            app,
            "menu:open-settings",
            "Settings...",
            true,
            Some("CmdOrCtrl+,"),
        )?)?;
        app_menu.append(&MenuItem::with_id(
            app,
            "menu:open-keybindings",
            "Keyboard Shortcuts",
            true,
            None::<&str>,
        )?)?;
        app_menu.append(&MenuItem::with_id(
            app,
            "menu:open-account",
            "Hanzo Account...",
            true,
            None::<&str>,
        )?)?;
        app_menu.append(&MenuItem::with_id(
            app,
            "menu:open-local-ai",
            "Local AI & Node...",
            true,
            Some("CmdOrCtrl+Shift+M"),
        )?)?;
        app_menu.append(&PredefinedMenuItem::separator(app)?)?;
        let active_dock_mode = dock_icon::load_mode(app);
        let dock_icon_menu = Submenu::new(app, "Dock Icon", true)?;
        dock_icon_menu.append(&CheckMenuItem::with_id(
            app,
            MENU_ID_DOCK_ICON_BRANDED,
            "Branded (H badge)",
            true,
            active_dock_mode == DockIconMode::Branded,
            None::<&str>,
        )?)?;
        dock_icon_menu.append(&CheckMenuItem::with_id(
            app,
            MENU_ID_DOCK_ICON_MINIMAL,
            "Minimal (No badge)",
            true,
            active_dock_mode == DockIconMode::Minimal,
            None::<&str>,
        )?)?;
        app_menu.append(&dock_icon_menu)?;
        app_menu.append(&PredefinedMenuItem::separator(app)?)?;
        app_menu.append(&PredefinedMenuItem::services(app, None)?)?;
        app_menu.append(&PredefinedMenuItem::separator(app)?)?;
        app_menu.append(&PredefinedMenuItem::hide(app, None)?)?;
        app_menu.append(&PredefinedMenuItem::hide_others(app, None)?)?;
        app_menu.append(&PredefinedMenuItem::show_all(app, None)?)?;
        app_menu.append(&PredefinedMenuItem::separator(app)?)?;
        app_menu.append(&PredefinedMenuItem::quit(app, None)?)?;
        menu.append(&app_menu)?;
    }

    let file_menu = Submenu::new(app, "File", true)?;
    file_menu.append(&MenuItem::with_id(
        app,
        "menu:chat.new",
        "New Thread",
        true,
        Some("CmdOrCtrl+N"),
    )?)?;
    #[cfg(not(target_os = "macos"))]
    {
        file_menu.append(&PredefinedMenuItem::separator(app)?)?;
        file_menu.append(&MenuItem::with_id(
            app,
            "menu:open-settings",
            "Settings...",
            true,
            Some("CmdOrCtrl+,"),
        )?)?;
        file_menu.append(&PredefinedMenuItem::quit(app, None)?)?;
    }
    menu.append(&file_menu)?;

    let edit_menu = Submenu::new(app, "Edit", true)?;
    edit_menu.append(&PredefinedMenuItem::undo(app, None)?)?;
    edit_menu.append(&PredefinedMenuItem::redo(app, None)?)?;
    edit_menu.append(&PredefinedMenuItem::separator(app)?)?;
    edit_menu.append(&PredefinedMenuItem::cut(app, None)?)?;
    edit_menu.append(&PredefinedMenuItem::copy(app, None)?)?;
    edit_menu.append(&PredefinedMenuItem::paste(app, None)?)?;
    edit_menu.append(&PredefinedMenuItem::select_all(app, None)?)?;
    menu.append(&edit_menu)?;

    let view_menu = Submenu::new(app, "View", true)?;
    view_menu.append(&MenuItem::with_id(
        app,
        "menu:commandPalette.toggle",
        "Command Palette...",
        true,
        Some("CmdOrCtrl+K"),
    )?)?;
    view_menu.append(&MenuItem::with_id(
        app,
        "menu:toggle-sidebar",
        "Toggle Sidebar",
        true,
        Some("CmdOrCtrl+B"),
    )?)?;
    view_menu.append(&MenuItem::with_id(
        app,
        "menu:toggle-voice-input",
        "Toggle Voice Input",
        true,
        Some("CmdOrCtrl+Shift+V"),
    )?)?;
    view_menu.append(&MenuItem::with_id(
        app,
        "snapshot:capture",
        "Take SnapShot",
        true,
        Some("CmdOrCtrl+Shift+2"),
    )?)?;
    view_menu.append(&PredefinedMenuItem::separator(app)?)?;
    view_menu.append(&PredefinedMenuItem::fullscreen(app, None)?)?;
    menu.append(&view_menu)?;

    let window_menu = Submenu::new(app, "Window", true)?;
    window_menu.append(&PredefinedMenuItem::minimize(app, None)?)?;
    window_menu.append(&PredefinedMenuItem::maximize(app, None)?)?;
    #[cfg(target_os = "macos")]
    {
        window_menu.append(&PredefinedMenuItem::separator(app)?)?;
        window_menu.append(&PredefinedMenuItem::close_window(app, None)?)?;
    }
    menu.append(&window_menu)?;

    Ok(menu)
}

#[cfg(target_os = "macos")]
fn sync_dock_icon_menu_items<R: Runtime>(items: &[MenuItemKind<R>], mode: DockIconMode) {
    for item in items {
        match item {
            MenuItemKind::Submenu(submenu) => {
                if let Ok(children) = submenu.items() {
                    sync_dock_icon_menu_items(&children, mode);
                }
            }
            MenuItemKind::Check(check) => {
                let id = check.id().as_ref();
                if id == MENU_ID_DOCK_ICON_BRANDED {
                    let _ = check.set_checked(mode == DockIconMode::Branded);
                } else if id == MENU_ID_DOCK_ICON_MINIMAL {
                    let _ = check.set_checked(mode == DockIconMode::Minimal);
                }
            }
            _ => {}
        }
    }
}

#[cfg(target_os = "macos")]
pub fn sync_dock_icon_menu<R: Runtime>(app: &AppHandle<R>, mode: DockIconMode) {
    if let Some(menu) = app.menu() {
        if let Ok(items) = menu.items() {
            sync_dock_icon_menu_items(&items, mode);
        }
    }
}

#[cfg(not(target_os = "macos"))]
pub fn sync_dock_icon_menu<R: Runtime>(_app: &AppHandle<R>, _mode: DockIconMode) {}

pub fn handle_menu_event(app: &AppHandle, raw_id: &str) {
    if raw_id == MENU_ID_DOCK_ICON_BRANDED {
        if let Err(err) = dock_icon::set_and_persist_mode(app, DockIconMode::Branded) {
            log::warn!("Failed to set Branded Dock icon from menu: {err}");
        }
        return;
    }

    if raw_id == MENU_ID_DOCK_ICON_MINIMAL {
        if let Err(err) = dock_icon::set_and_persist_mode(app, DockIconMode::Minimal) {
            log::warn!("Failed to set Minimal Dock icon from menu: {err}");
        }
        return;
    }

    if raw_id == "snapshot:capture" {
        crate::snapshot::spawn_interactive_snapshot(app.clone());
        return;
    }

    if let Some(action) = raw_id.strip_prefix("menu:") {
        let _ = app.emit("desktop:menu-action", action.to_string());
    }
}
