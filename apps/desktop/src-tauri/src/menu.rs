use crate::commands::AppState;
use tauri::menu::{Menu, MenuItem, PredefinedMenuItem, Submenu};
use tauri::{AppHandle, Emitter, Manager};

pub fn build_app_menu(app: &AppHandle) -> tauri::Result<Menu<tauri::Wry>> {
    let menu = Menu::new(app)?;

    #[cfg(target_os = "macos")]
    {
        let app_menu = Submenu::new(app, "Hanzo Dev", true)?;
        app_menu.append(&PredefinedMenuItem::about(app, Some("About Hanzo Dev"), None)?)?;
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

pub fn handle_menu_event(app: &AppHandle, raw_id: &str) {
    if let Some(rest) = raw_id.strip_prefix("ctx:") {
        if let Some((token, selected_id)) = rest.split_once(':') {
            if let Some(state) = app.try_state::<AppState>() {
                if let Ok(mut pending) = state.pending_context_menu.lock() {
                    if pending.as_ref().map(|(t, _)| t == token).unwrap_or(false) {
                        if let Some((_, tx)) = pending.take() {
                            let _ = tx.send(Some(selected_id.to_string()));
                        }
                    }
                }
            }
        }
        return;
    }

    if let Some(action) = raw_id.strip_prefix("menu:") {
        let _ = app.emit("desktop:menu-action", action.to_string());
    }
}
