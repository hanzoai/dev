//! Streaming-related helpers for `ChatWidget`.

use super::ChatWidget;
use crate::height_manager::HeightEvent;
use crate::streaming::controller::AppEventHistorySink;
use crate::streaming::StreamKind;

pub(super) fn on_commit_tick(chat: &mut ChatWidget<'_>) {
    let sink = AppEventHistorySink(chat.app_event_tx.clone());
    let _finished = chat.stream.on_commit_tick(&sink);
}

pub(super) fn is_write_cycle_active(chat: &ChatWidget<'_>) -> bool {
    chat.stream.is_write_cycle_active()
}

pub(super) fn handle_streaming_delta(
    chat: &mut ChatWidget<'_>,
    kind: StreamKind,
    id: String,
    delta: String,
) {
    if chat.stream_state.drop_streaming {
        tracing::debug!(
            "dropping streaming delta after cancel (kind={:?}, id={})",
            kind, id
        );
        return;
    }
    tracing::debug!("handle_streaming_delta kind={:?}, delta={:?}", kind, delta);
    chat.stream_state.current_kind = Some(kind);
    let sink = AppEventHistorySink(chat.app_event_tx.clone());
    chat.stream.begin_with_id(kind, Some(id), &sink);
    chat.stream.push_and_maybe_commit(&delta, &sink);
    chat.mark_needs_redraw();
}

pub(super) fn finalize_active_stream(chat: &mut ChatWidget<'_>) {
    let sink = AppEventHistorySink(chat.app_event_tx.clone());
    if chat.stream.is_write_cycle_active() {
        chat.stream_state.current_kind = Some(StreamKind::Reasoning);
        chat.stream.finalize(StreamKind::Reasoning, true, &sink);
        chat.stream_state.current_kind = Some(StreamKind::Answer);
        chat.stream.finalize(StreamKind::Answer, true, &sink);
        chat.height_manager
            .borrow_mut()
            .record_event(HeightEvent::HistoryFinalize);
    }
    chat.stream_state.current_kind = None;
}
