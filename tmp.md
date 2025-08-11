# Proposed Changes: Reliable image sizing for ratatui-image

This approach avoids `Picker::from_query_stdio()` races by measuring terminal cell size via `/dev/tty` once and feeding that to `Picker::from_fontsize`. It removes the startup timing constraint and yields accurate image scaling.

## 1) codex-rs/tui/src/terminal_info.rs

Add a helper that returns the exact cell size in pixels (width, height). Place it near the bottom, above `DEFAULT_CELL_ASPECT_RATIO` so it can reuse internal helpers.

```rust
pub fn get_cell_size_pixels() -> Option<(u16, u16)> {
    // Open /dev/tty for reading and writing
    let mut tty_w = OpenOptions::new().write(true).open("/dev/tty").ok()?;
    let mut tty_r = OpenOptions::new().read(true).open("/dev/tty").ok()?;

    // Try direct cell size query (CSI 16 t) -> expect: CSI 6;height;width t
    tty_w.write_all(b"\x1b[16t").ok()?;
    tty_w.flush().ok()?;
    if let Some(reply) = read_reply(&mut tty_r, Duration::from_millis(100)) {
        if let Some((kind, height, width)) = parse_three_nums(&reply) {
            if kind == 6 && width > 0 && height > 0 {
                return Some((width as u16, height as u16));
            }
        }
    }

    // Fallback: window size in pixels (CSI 14 t) -> CSI 4;win_h;win_w t
    tty_w.write_all(b"\x1b[14t").ok()?;
    tty_w.flush().ok()?;
    let (mut win_h, mut win_w) = (0u32, 0u32);
    if let Some(reply) = read_reply(&mut tty_r, Duration::from_millis(100)) {
        if let Some((kind, h, w)) = parse_three_nums(&reply) {
            if kind == 4 {
                win_h = h;
                win_w = w;
            }
        }
    }

    // Text area in characters (CSI 18 t) -> CSI 8;rows;cols t
    tty_w.write_all(b"\x1b[18t").ok()?;
    tty_w.flush().ok()?;
    let (mut rows, mut cols) = (0u32, 0u32);
    if let Some(reply) = read_reply(&mut tty_r, Duration::from_millis(100)) {
        if let Some((kind, r, c)) = parse_three_nums(&reply) {
            if kind == 8 {
                rows = r;
                cols = c;
            }
        }
    }

    if win_h > 0 && win_w > 0 && rows > 0 && cols > 0 {
        let cell_w = (win_w as f32 / cols as f32).round() as u16;
        let cell_h = (win_h as f32 / rows as f32).round() as u16;
        if cell_w > 0 && cell_h > 0 {
            return Some((cell_w, cell_h));
        }
    }
    None
}
```

## 2) codex-rs/tui/src/chatwidget.rs

- Add a cached cell-size field to `ChatWidget`:

```rust
// Cached cell size (width,height) in pixels
cached_cell_size: std::cell::OnceCell<(u16, u16)>,
```

- Add a helper to compute the picker font size once, with fallback:

```rust
impl ChatWidget<'_> {
    fn measured_font_size(&self) -> (u16, u16) {
        let default_guess = if std::env::var("TERM_PROGRAM").unwrap_or_default() == "iTerm.app" {
            (7, 15)
        } else {
            (8, 16)
        };
        *self.cached_cell_size.get_or_init(|| {
            crate::terminal_info::get_cell_size_pixels().unwrap_or(default_guess)
        })
    }
    // ...
}
```

- Use `measured_font_size()` when creating the `Picker` (two places):

1) In the branch where we create a new picker (around the first `Picker::from_fontsize(...)`):

```rust
// Replace the conditional guessed font size block with
let picker = Picker::from_fontsize(self.measured_font_size());
```

2) In `render_screenshot_highlevel`, when `cached_picker` is `None`:

```rust
// Replace the guessed font size block with
*cached_picker = Some(Picker::from_fontsize(self.measured_font_size()));
```

Notes
- No changes to the event thread ordering are required.
- This approach avoids races with `stdin` and makes screenshot images fill the intended area using `Resize::Fit`.
- Keep the existing aspect‑ratio calculation in `popup_area_top_right`; it remains correct.
