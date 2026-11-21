//! Menu-bar item for provider usage. The server owns the data and the alert thresholds
//! (`GET /v1/usage/tray`); this module draws the icon, lists the readings and shows the
//! notifications the server drains.

use crate::auth::exchange_bootstrap_for_bearer_token;
use crate::commands::AppState;
use serde::Deserialize;
use std::time::Duration;
use tauri::image::Image;
use tauri::menu::{Menu, MenuItem, PredefinedMenuItem};
use tauri::tray::{TrayIcon, TrayIconBuilder};
use tauri::{AppHandle, Manager};
use tauri_plugin_notification::NotificationExt;

const TRAY_ID: &str = "usage";
const POLL_SECONDS: u64 = 60;
const ICON_SIZE: u32 = 22;

#[derive(Debug, Deserialize, PartialEq)]
pub struct TrayModel {
    pub title: String,
    pub fill: f64,
    pub lines: Vec<String>,
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct Alert {
    pub title: String,
    pub body: String,
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct TrayPayload {
    pub tray: TrayModel,
    pub alerts: Vec<Alert>,
}

pub fn parse_payload(raw: &str) -> Result<TrayPayload, serde_json::Error> {
    serde_json::from_str(raw)
}

/// A vertical gauge: outline plus a fill that rises with `fill` (0-100). Black on transparent,
/// so macOS renders it as a template image in the menu bar's own colour.
pub fn gauge_rgba(fill: f64) -> Vec<u8> {
    let size = ICON_SIZE as usize;
    let mut px = vec![0u8; size * size * 4];
    let (x0, x1, y0, y1) = (7usize, 14usize, 3usize, 18usize);
    let inner = (y1 - y0 - 1) as f64;
    let filled = ((fill.clamp(0.0, 100.0) / 100.0) * inner).round() as usize;
    for y in y0..=y1 {
        for x in x0..=x1 {
            let edge = y == y0 || y == y1 || x == x0 || x == x1;
            let in_fill = y > y1 - 1 - filled && y < y1;
            if edge || in_fill {
                let i = (y * size + x) * 4;
                px[i + 3] = 255;
            }
        }
    }
    px
}

fn icon(fill: f64) -> Image<'static> {
    Image::new_owned(gauge_rgba(fill), ICON_SIZE, ICON_SIZE)
}

fn build_menu(app: &AppHandle, lines: &[String]) -> tauri::Result<Menu<tauri::Wry>> {
    let menu = Menu::new(app)?;
    if lines.is_empty() {
        menu.append(&MenuItem::with_id(
            app,
            "usage:none",
            "No usage data yet",
            false,
            None::<&str>,
        )?)?;
    }
    for (i, line) in lines.iter().enumerate() {
        menu.append(&MenuItem::with_id(
            app,
            format!("usage:line:{i}"),
            line,
            false,
            None::<&str>,
        )?)?;
    }
    menu.append(&PredefinedMenuItem::separator(app)?)?;
    menu.append(&MenuItem::with_id(
        app,
        "usage:open",
        "Open Usage",
        true,
        None::<&str>,
    )?)?;
    menu.append(&MenuItem::with_id(
        app,
        "usage:quit",
        "Quit Hanzo Dev",
        true,
        None::<&str>,
    )?)?;
    Ok(menu)
}

async fn fetch_payload(app: &AppHandle) -> Result<TrayPayload, String> {
    let state = app.state::<AppState>();
    let token = {
        let mut guard = state.bearer_token.lock().await;
        match guard.as_ref() {
            Some(t) => t.clone(),
            None => {
                let t = exchange_bootstrap_for_bearer_token(
                    &state.supervisor.config.http_base_url,
                    &state.supervisor.config.bootstrap_secret,
                )
                .await?;
                *guard = Some(t.clone());
                t
            }
        }
    };
    let url = format!("{}/v1/usage/tray", state.supervisor.config.http_base_url);
    let raw = reqwest::Client::new()
        .get(url)
        .bearer_auth(token)
        .timeout(Duration::from_secs(30))
        .send()
        .await
        .map_err(|e| e.to_string())?
        .error_for_status()
        .map_err(|e| e.to_string())?
        .text()
        .await
        .map_err(|e| e.to_string())?;
    parse_payload(&raw).map_err(|e| e.to_string())
}

fn apply(app: &AppHandle, tray: &TrayIcon, payload: &TrayPayload) {
    let _ = tray.set_icon(Some(icon(payload.tray.fill)));
    #[cfg(target_os = "macos")]
    {
        let _ = tray.set_icon_as_template(true);
        let _ = tray.set_title(if payload.tray.title.is_empty() {
            None
        } else {
            Some(payload.tray.title.as_str())
        });
    }
    let _ = tray.set_tooltip(Some(payload.tray.lines.join("\n")));
    if let Ok(menu) = build_menu(app, &payload.tray.lines) {
        let _ = tray.set_menu(Some(menu));
    }
    for alert in &payload.alerts {
        let _ = app
            .notification()
            .builder()
            .title(&alert.title)
            .body(&alert.body)
            .show();
    }
}

pub fn start(app: &AppHandle) -> tauri::Result<()> {
    let menu = build_menu(app, &[])?;
    let tray = TrayIconBuilder::with_id(TRAY_ID)
        .icon(icon(0.0))
        .icon_as_template(true)
        .tooltip("Hanzo Dev usage")
        .menu(&menu)
        .on_menu_event(|app, event| match event.id().as_ref() {
            "usage:open" => {
                if let Some(window) = app.get_webview_window("main") {
                    let _ = window.unminimize();
                    let _ = window.show();
                    let _ = window.set_focus();
                }
                let _ = tauri::Emitter::emit(
                    app,
                    "desktop:menu-action",
                    "open-usage".to_string(),
                );
            }
            "usage:quit" => app.exit(0),
            _ => {}
        })
        .build(app)?;

    let handle = app.clone();
    tauri::async_runtime::spawn(async move {
        // The backend takes a moment to come up; a failed read just waits for the next tick.
        loop {
            if let Ok(payload) = fetch_payload(&handle).await {
                apply(&handle, &tray, &payload);
            }
            tokio::time::sleep(Duration::from_secs(POLL_SECONDS)).await;
        }
    });
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_the_server_payload() {
        let raw = r#"{"tray":{"title":"73%","fill":73,"lines":["Claude: Session 73%"]},
                      "alerts":[{"title":"Claude: Session at 82%","body":"Resets in 2h 05m."}]}"#;
        let p = parse_payload(raw).unwrap();
        assert_eq!(p.tray.title, "73%");
        assert_eq!(p.alerts.len(), 1);
    }

    #[test]
    fn gauge_fill_rises_with_usage() {
        let lit = |fill: f64| gauge_rgba(fill).chunks(4).filter(|p| p[3] == 255).count();
        assert!(lit(0.0) < lit(50.0));
        assert!(lit(50.0) < lit(100.0));
        assert_eq!(gauge_rgba(10.0).len(), (ICON_SIZE * ICON_SIZE * 4) as usize);
    }
}
