//! Dependency-free, terminal-resolution 3D renderer for the canonical Hanzo mark.
//! Only one rigid Y-axis transform; the actual 2D outline is hanzo::MARK.
//!
//! Each Braille cell encodes eight surface samples. The logo never exceeds
//! the caller-supplied bounds, even at 80 columns or on terminal resize.

use std::f64::consts::TAU;
use std::sync::LazyLock;

pub(crate) const MAX_WIDTH: u16 = 60;
pub(crate) const MAX_HEIGHT: u16 = 21;
const GRID: usize = 160;
const HALF_DEPTH: f64 = 0.33;
const DOTS: [u8; 8] = [1, 8, 2, 16, 4, 32, 64, 128];
const CUBE_PITCH: f64 = 2.0 / 12.0;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Cell {
    pub(crate) glyph: char,
    pub(crate) shade: u8,
}

impl Default for Cell {
    fn default() -> Self {
        Self {
            glyph: ' ',
            shade: 0,
        }
    }
}

fn number(path: &str, index: &mut usize) -> f64 {
    let bytes = path.as_bytes();
    while *index < bytes.len() && bytes[*index].is_ascii_whitespace() {
        *index += 1;
    }
    let start = *index;
    while *index < bytes.len()
        && (bytes[*index].is_ascii_digit() || bytes[*index] == b'.' || bytes[*index] == b'-')
    {
        *index += 1;
    }
    path[start..*index].parse().unwrap_or(0.0)
}

fn parse_mark() -> Vec<Vec<[f64; 2]>> {
    let path = hanzo::MARK.0;
    let mut index = 0;
    let mut current = [0.0, 0.0];
    let mut shape = Vec::new();
    let mut polygons = Vec::new();
    while index < path.len() {
        let command = path.as_bytes()[index] as char;
        index += 1;
        match command {
            'M' => {
                if !shape.is_empty() {
                    polygons.push(std::mem::take(&mut shape));
                }
                current = [number(path, &mut index), number(path, &mut index)];
                shape.push(current);
            }
            'L' => {
                current = [number(path, &mut index), number(path, &mut index)];
                shape.push(current);
            }
            'H' => {
                current[0] = number(path, &mut index);
                shape.push(current);
            }
            'V' => {
                current[1] = number(path, &mut index);
                shape.push(current);
            }
            'Z' => {
                if !shape.is_empty() {
                    polygons.push(std::mem::take(&mut shape));
                }
            }
            _ => {}
        }
    }
    if !shape.is_empty() {
        polygons.push(shape);
    }
    polygons
}

fn inside_polygon(point: [f64; 2], polygon: &[[f64; 2]]) -> bool {
    let mut inside = false;
    let mut prev = polygon[polygon.len() - 1];
    for &next in polygon {
        if (next[1] > point[1]) != (prev[1] > point[1]) {
            let intercept =
                (prev[0] - next[0]) * (point[1] - next[1]) / (prev[1] - next[1]) + next[0];
            if point[0] < intercept {
                inside = !inside;
            }
        }
        prev = next;
    }
    inside
}

static MASK: LazyLock<Vec<bool>> = LazyLock::new(|| {
    let polygons = parse_mark();
    let mut mask = vec![false; GRID * GRID];
    for iy in 0..GRID {
        for ix in 0..GRID {
            let point = [
                (ix as f64 + 0.5) / GRID as f64 * 67.0,
                (iy as f64 + 0.5) / GRID as f64 * 67.0,
            ];
            mask[iy * GRID + ix] = polygons.iter().any(|p| inside_polygon(point, p));
        }
    }
    mask
});

fn filled(x: f64, y: f64) -> bool {
    let fx = (x + 1.0) * 0.5;
    let fy = (y + 1.0) * 0.5;
    if !(0.0..1.0).contains(&fx) || !(0.0..1.0).contains(&fy) {
        return false;
    }
    let ix = (fx * GRID as f64) as usize;
    let iy = (fy * GRID as f64) as usize;
    MASK[iy * GRID + ix]
}

fn face_brightness(normal: [f64; 3]) -> f64 {
    (0.27 + 0.72 * (normal[0] * -0.45 + normal[1] * 0.55 + normal[2] * 0.70).max(0.0)).min(1.0)
}

fn ray(x: f64, y: f64, yaw: f64) -> Option<f64> {
    let (s, c) = yaw.sin_cos();
    let origin = [c * x - s * 4.5, y, s * x + c * 4.5];
    let direction = [s, 0.0, -c];
    let bounds = [(-1.0, 1.0), (-1.0, 1.0), (-HALF_DEPTH, HALF_DEPTH)];
    let (mut enter, mut exit) = (0.0_f64, 20.0_f64);
    for axis in 0..3 {
        if direction[axis].abs() < 1e-10 {
            if origin[axis] < bounds[axis].0 || origin[axis] > bounds[axis].1 {
                return None;
            }
            continue;
        }
        let a = (bounds[axis].0 - origin[axis]) / direction[axis];
        let b = (bounds[axis].1 - origin[axis]) / direction[axis];
        enter = enter.max(a.min(b));
        exit = exit.min(a.max(b));
        if enter > exit {
            return None;
        }
    }
    let mut t = enter + 0.004;
    while t <= exit && t < 20.0 {
        let px = origin[0] + t * direction[0];
        let py = origin[1];
        let pz = origin[2] + t * direction[2];
        if filled(px, py) {
            let local_normal = if (pz.abs() - HALF_DEPTH).abs() < 0.018 {
                [0.0, 0.0, if pz > 0.0 { 1.0 } else { -1.0 }]
            } else {
                let dx = (filled(px - 0.026, py) as i8) - (filled(px + 0.026, py) as i8);
                let dy = (filled(px, py - 0.026) as i8) - (filled(px, py + 0.026) as i8);
                let len = (f64::from(dx).hypot(f64::from(dy))).max(1e-9);
                if len < 0.5 {
                    [-direction[0], 0.0, -direction[2]]
                } else {
                    [f64::from(dx) / len, f64::from(dy) / len, 0.0]
                }
            };
            let normal = [
                local_normal[0] * c + local_normal[2] * s,
                local_normal[1],
                -local_normal[0] * s + local_normal[2] * c,
            ];
            let edge = |v: f64| {
                let frac = ((v + 1.0) / CUBE_PITCH).rem_euclid(1.0);
                frac.min(1.0 - frac)
            };
            let seam = if local_normal[2].abs() > 0.5 {
                edge(px).min(edge(py))
            } else if local_normal[0].abs() > local_normal[1].abs() {
                edge(py).min(edge(pz))
            } else {
                edge(px).min(edge(pz))
            };
            let surface = if seam < 0.035 { 0.81 } else { 1.0 };
            return Some(face_brightness(normal) * surface);
        }
        t += 0.013;
    }
    None
}

pub(crate) fn render(columns: u16, rows: u16, phase: f64) -> Vec<Vec<Cell>> {
    let columns = columns.min(MAX_WIDTH);
    let rows = rows.min(MAX_HEIGHT);
    if columns == 0 || rows == 0 {
        return Vec::new();
    }
    let dot_cols = usize::from(columns) * 2;
    let dot_rows = usize::from(rows) * 4;
    let pitch = 2.65 / dot_rows as f64;
    let yaw = phase.rem_euclid(1.0) * TAU;
    let mut result = vec![vec![Cell::default(); usize::from(columns)]; usize::from(rows)];
    for row in 0..usize::from(rows) {
        for col in 0..usize::from(columns) {
            let mut dots = 0_u8;
            let mut total = 0.0;
            let mut count = 0_u32;
            for (sub, bit) in DOTS.into_iter().enumerate() {
                let dx = col * 2 + sub % 2;
                let dy = row * 4 + sub / 2;
                let x = (dx as f64 + 0.5 - dot_cols as f64 * 0.5) * pitch;
                // SVG Y increases downward; the 2D face must not flip.
                let y = (dy as f64 + 0.5 - dot_rows as f64 * 0.5) * pitch;
                if let Some(value) = ray(x, y, yaw) {
                    dots |= bit;
                    total += value;
                    count += 1;
                }
            }
            if count > 0 {
                result[row][col] = Cell {
                    glyph: char::from_u32(0x2800 + u32::from(dots)).unwrap_or(' '),
                    shade: ((total / f64::from(count) * 23.0).round() as u8).min(23),
                };
            }
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn geometry_comes_from_hanzo_brand() {
        assert_eq!(parse_mark().len(), 5);
        assert!(filled(-0.67, -0.67));
        assert!(!filled(0.0, -0.83));
    }

    #[test]
    fn exact_loop_and_80_column_bounds() {
        let first = render(60, 21, 0.0);
        assert_eq!(first, render(60, 21, 1.0));
        assert_eq!(first.len(), 21);
        assert!(first.iter().all(|row| row.len() == 60));
        assert!(first.iter().flatten().any(|cell| cell.glyph != ' '));
        assert!(render(80, 24, 0.0).iter().all(|row| row.len() <= 60));
    }
}
