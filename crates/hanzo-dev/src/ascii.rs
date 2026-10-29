//! Native idle adapter. Geometry, lighting, sampling and spin live in hanzo::logo,
//! exactly as in the default welcome TUI.
pub(crate) const MAX_WIDTH: u16 = hanzo::logo::MAX_COLUMNS;
pub(crate) const MAX_HEIGHT: u16 = 21;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Cell {
    pub(crate) glyph: char,
    pub(crate) rgb: u32,
}

pub(crate) fn render(columns: u16, rows: u16, phase: f64) -> Vec<Vec<Cell>> {
    let columns = columns.min(MAX_WIDTH);
    let rows = rows.min(MAX_HEIGHT);
    if columns == 0 || rows == 0 {
        return Vec::new();
    }
    let mut renderer = hanzo::logo::Renderer::default();
    let light = hanzo::logo::Lighting::terminal((210, 221, 235), (15, 20, 37));
    renderer
        .frame(columns, rows, phase, &light)
        .chunks(usize::from(columns))
        .map(|row| {
            row.iter()
                .map(|cell| Cell {
                    glyph: cell.glyph(),
                    rgb: cell.rgb,
                })
                .collect()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn native_and_welcome_frames_are_identical() {
        let light = hanzo::logo::Lighting::terminal((210, 221, 235), (15, 20, 37));
        let mut shared = hanzo::logo::Renderer::default();
        for (width, height) in [(40, 14), (60, 21)] {
            for phase in [0.0, 0.125, 0.25, 0.5, 1.0] {
                let native = render(width, height, phase);
                for (a, b) in native
                    .iter()
                    .flatten()
                    .zip(shared.frame(width, height, phase, &light))
                {
                    assert_eq!(a.rgb, b.rgb);
                    assert_eq!(a.glyph, b.glyph());
                }
            }
        }
        assert!(render(0, 0, 0.0).is_empty());
    }
}
