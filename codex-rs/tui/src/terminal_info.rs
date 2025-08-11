use std::fs::OpenOptions;
use std::io::{Read, Write};
use std::time::{Duration, Instant};

/// Query the terminal for actual cell dimensions and return the aspect ratio (height/width)
/// Returns None if the terminal doesn't support the query or on timeout
pub fn get_cell_aspect_ratio() -> Option<f32> {
    cell_aspect_ratio(100) // 100ms timeout
}

fn read_reply(tty: &mut std::fs::File, timeout: Duration) -> Option<String> {
    let start = Instant::now();
    let mut buf = [0u8; 256];
    let mut s = String::new();
    
    // Set non-blocking mode
    #[cfg(unix)]
    {
        use std::os::unix::io::AsRawFd;
        let fd = tty.as_raw_fd();
        let flags = unsafe { libc::fcntl(fd, libc::F_GETFL, 0) };
        if flags != -1 {
            unsafe { libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK) };
        }
    }
    
    while start.elapsed() < timeout {
        match tty.read(&mut buf) {
            Ok(n) if n > 0 => {
                s.push_str(&String::from_utf8_lossy(&buf[..n]));
                // Check if we got a complete response (ends with 't')
                if s.contains('t') && s.contains("\x1b[") { 
                    break; 
                }
            }
            _ => {
                // Small sleep to avoid busy waiting
                std::thread::sleep(Duration::from_millis(10));
            }
        }
    }
    
    (!s.is_empty()).then_some(s)
}

fn parse_three_nums(s: &str) -> Option<(u32, u32, u32)> {
    // Parse response like "\x1b[6;20;10t" (kind;height;width)
    // Use simple parsing to avoid regex dependency
    if let Some(start) = s.find("\x1b[") {
        let s = &s[start + 2..]; // Skip "\x1b["
        if let Some(end) = s.find('t') {
            let nums_str = &s[..end];
            let parts: Vec<&str> = nums_str.split(';').collect();
            if parts.len() == 3 {
                if let (Ok(a), Ok(b), Ok(c)) = (
                    parts[0].parse::<u32>(),
                    parts[1].parse::<u32>(),
                    parts[2].parse::<u32>()
                ) {
                    return Some((a, b, c));
                }
            }
        }
    }
    None
}

fn cell_aspect_ratio(timeout_ms: u64) -> Option<f32> {
    // Open /dev/tty for reading and writing
    let mut tty_w = OpenOptions::new()
        .write(true)
        .open("/dev/tty")
        .ok()?;
    let mut tty_r = OpenOptions::new()
        .read(true)
        .open("/dev/tty")
        .ok()?;
    
    // Method 1: Try direct cell size query (CSI 16 t)
    // Terminal should respond with CSI 6;height;width t
    tty_w.write_all(b"\x1b[16t").ok()?;
    tty_w.flush().ok()?;
    
    if let Some(reply) = read_reply(&mut tty_r, Duration::from_millis(timeout_ms)) {
        if let Some((kind, height, width)) = parse_three_nums(&reply) {
            if kind == 6 && width > 0 {
                return Some(height as f32 / width as f32);
            }
        }
    }
    
    // Method 2: Fallback - query window size in pixels and text area in characters
    // Get window size in pixels (CSI 14 t) -> response: CSI 4;height;width t
    tty_w.write_all(b"\x1b[14t").ok()?;
    tty_w.flush().ok()?;
    
    let (mut win_h, mut win_w) = (0u32, 0u32);
    if let Some(reply) = read_reply(&mut tty_r, Duration::from_millis(timeout_ms)) {
        if let Some((kind, h, w)) = parse_three_nums(&reply) {
            if kind == 4 {
                win_h = h;
                win_w = w;
            }
        }
    }
    
    // Get text area size in characters (CSI 18 t) -> response: CSI 8;rows;cols t
    tty_w.write_all(b"\x1b[18t").ok()?;
    tty_w.flush().ok()?;
    
    let (mut rows, mut cols) = (0u32, 0u32);
    if let Some(reply) = read_reply(&mut tty_r, Duration::from_millis(timeout_ms)) {
        if let Some((kind, r, c)) = parse_three_nums(&reply) {
            if kind == 8 {
                rows = r;
                cols = c;
            }
        }
    }
    
    // Calculate cell dimensions from window and text area sizes
    if win_h > 0 && win_w > 0 && rows > 0 && cols > 0 {
        let cell_h = win_h as f32 / rows as f32;
        let cell_w = win_w as f32 / cols as f32;
        return Some(cell_h / cell_w);
    }
    
    None
}

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

/// Default aspect ratio for terminals (most terminals use roughly 2:1)
pub const DEFAULT_CELL_ASPECT_RATIO: f32 = 2.0;