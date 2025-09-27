use super::*;
use crate::history::state::{HistoryId, ImageRecord};
use sha2::{Digest, Sha256};

#[derive(Clone)]
pub(crate) struct ImageCellState {
    pub width: u32,
    pub height: u32,
    pub rgba: Vec<u8>,
}

impl ImageCellState {
    pub(crate) fn from_dynamic(image: DynamicImage) -> Self {
        let width = image.width();
        let height = image.height();
        let rgba = image.to_rgba8().into_raw();
        Self { width, height, rgba }
    }
}

pub(crate) struct ImageOutputCell {
    state: ImageCellState,
    record: ImageRecord,
}

impl ImageOutputCell {
    pub(crate) fn new(image: DynamicImage) -> Self {
        let state = ImageCellState::from_dynamic(image);
        let width_u16 = state.width.min(u16::MAX as u32) as u16;
        let height_u16 = state.height.min(u16::MAX as u32) as u16;
        let sha = Sha256::digest(&state.rgba);
        let sha_hex = format!("{:x}", sha);
        let record = ImageRecord {
            id: HistoryId::ZERO,
            source_path: None,
            alt_text: None,
            width: width_u16,
            height: height_u16,
            sha256: Some(sha_hex),
        };
        Self { state, record }
    }

    pub(crate) fn record(&self) -> &ImageRecord {
        &self.record
    }

    pub(crate) fn record_mut(&mut self) -> &mut ImageRecord {
        &mut self.record
    }
}

impl HistoryCell for ImageOutputCell {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn kind(&self) -> HistoryCellType {
        HistoryCellType::Image
    }

    fn display_lines(&self) -> Vec<Line<'static>> {
        let description = format!(
            "tool result (image {}x{} RGBA, {} bytes)",
            self.state.width,
            self.state.height,
            self.state.rgba.len()
        );
        vec![
            Line::from(description),
            Line::from(""),
        ]
    }
}
