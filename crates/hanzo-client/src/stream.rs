use std::sync::Arc;
use futures::Stream;

pub fn chat_stream(
    _inner: Arc<crate::ClientInner>,
    _request: crate::chat::ChatRequest,
) -> impl Stream<Item = crate::Result<hanzo_protocol::Event>> {
    futures::stream::empty()
}
