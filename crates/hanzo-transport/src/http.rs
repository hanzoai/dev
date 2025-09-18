//! HTTP transport implementation

use async_trait::async_trait;
use super::Transport;

pub struct HttpTransport {
    client: reqwest::Client,
    base_url: String,
}

impl HttpTransport {
    pub fn new(base_url: String) -> Self {
        Self {
            client: reqwest::Client::new(),
            base_url,
        }
    }
}

#[async_trait]
impl Transport for HttpTransport {
    type Error = reqwest::Error;

    async fn send(&self, request: Vec<u8>) -> Result<Vec<u8>, Self::Error> {
        let response = self.client
            .post(&self.base_url)
            .body(request)
            .send()
            .await?;

        Ok(response.bytes().await?.to_vec())
    }
}