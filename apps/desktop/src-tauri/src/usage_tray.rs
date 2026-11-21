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
/// 18pt at @2x. Strokes are 3px (1.5pt), the weight of the neighbouring system icons.
const ICON_SIZE: u32 = 36;

#[derive(Debug, Deserialize, PartialEq)]
pub struct TrayModel {
    pub title: String,
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

/// The Dev mark `</>`, black on transparent so macOS renders it as a template image in the menu
/// bar's own colour. The reading itself is the title text beside it.
pub fn glyph_rgba() -> Vec<u8> {
    let size = ICON_SIZE as usize;
    let mut alpha = vec![0f32; size * size];
    let mut stroke = |a: (f32, f32), b: (f32, f32), width: f32, level: f32| {
        let (dx, dy) = (b.0 - a.0, b.1 - a.1);
        let len2 = dx * dx + dy * dy;
        for y in 0..size {
            for x in 0..size {
                let (px, py) = (x as f32 + 0.5, y as f32 + 0.5);
                let t = (((px - a.0) * dx + (py - a.1) * dy) / len2).clamp(0.0, 1.0);
                let dist = ((px - (a.0 + t * dx)).powi(2) + (py - (a.1 + t * dy)).powi(2)).sqrt();
                let cover = (width / 2.0 + 0.5 - dist).clamp(0.0, 1.0) * level;
                let cell = &mut alpha[y * size + x];
                *cell = cell.max(cover);
            }
        }
    };
    // Centred on the 36px grid (x and y mid-point 18); 3px strokes on half-pixel coordinates
    // keep the horizontal extents on whole pixels.
    let p = |x: f32, y: f32| (x + 0.5, y + 0.5);
    let w = 3.0;
    stroke(p(10.0, 9.0), p(3.0, 17.0), w, 1.0);
    stroke(p(3.0, 17.0), p(10.0, 25.0), w, 1.0);
    stroke(p(20.0, 7.0), p(14.0, 27.0), w, 1.0);
    stroke(p(24.0, 9.0), p(31.0, 17.0), w, 1.0);
    stroke(p(31.0, 17.0), p(24.0, 25.0), w, 1.0);
    let mut px = vec![0u8; size * size * 4];
    for (i, a) in alpha.iter().enumerate() {
        px[i * 4 + 3] = (a * 255.0).round() as u8;
    }
    px
}

fn icon() -> Image<'static> {
    Image::new_owned(glyph_rgba(), ICON_SIZE, ICON_SIZE)
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
    let _ = tray.set_icon(Some(icon()));
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
        .icon(icon())
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
        let raw = r#"{"tray":{"title":"73%","lines":["Claude: Session 73%"]},
                      "alerts":[{"title":"Claude: Session at 82%","body":"Resets in 2h 05m."}]}"#;
        let p = parse_payload(raw).unwrap();
        assert_eq!(p.tray.title, "73%");
        assert_eq!(p.alerts.len(), 1);
    }

    #[test]
    fn glyph_is_alpha_only_and_drawn() {
        let px = glyph_rgba();
        assert_eq!(px.len(), (ICON_SIZE * ICON_SIZE * 4) as usize);
        assert!(px.chunks(4).filter(|p| p[3] > 200).count() > 100);
        assert!(px.chunks(4).all(|p| p[0] == 0 && p[1] == 0 && p[2] == 0));
        // Vertically centred: as much empty space above the glyph as below it.
        let size = ICON_SIZE as usize;
        let rows: Vec<usize> = (0..size)
            .filter(|y| (0..size).any(|x| px[(y * size + x) * 4 + 3] > 40))
            .collect();
        let (top, bottom) = (rows[0], size - 1 - rows[rows.len() - 1]);
        assert!(top.abs_diff(bottom) <= 1, "top {top} bottom {bottom}");
    }
}
