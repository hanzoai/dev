//! Simplified LLM interface - matches Python's llm_client.py

use crate::{Result, client::Hanzo, Message};

/// Simplified LLM client interface - matches Python's LLMClient
#[derive(Clone)]
pub struct LLMClient {
    client: Hanzo,
    model: String,
}

impl LLMClient {
    /// Create new LLM client
    pub fn new(api_key: impl Into<String>) -> Result<Self> {
        Ok(Self {
            client: Hanzo::new(api_key)?,
            model: "gpt-5".to_string(),
        })
    }

    /// Set the model to use
    pub fn with_model(mut self, model: impl Into<String>) -> Self {
        self.model = model.into();
        self
    }

    /// Quick completion - matches Python: client.complete("prompt")
    pub async fn complete(&self, prompt: impl Into<String>) -> Result<String> {
        let response = self.client
            .chat()
            .create_with_model(&self.model, vec![Message::user(prompt)])
            .await?;

        Ok(response.choices[0].message.content.clone())
    }

    /// Chat with messages
    pub async fn chat(&self, messages: Vec<Message>) -> Result<String> {
        let response = self.client
            .chat()
            .create_with_model(&self.model, messages)
            .await?;

        Ok(response.choices[0].message.content.clone())
    }

    /// Stream completion
    pub async fn stream(&self, prompt: impl Into<String>) -> Result<impl futures::Stream<Item = Result<String>>> {
        use futures::StreamExt;

        let stream = self.client
            .chat()
            .stream_with_model(&self.model, vec![Message::user(prompt)])
            .await?;

        Ok(stream.filter_map(|event| async move {
            match event {
                Ok(e) => e.delta.map(Ok),
                Err(e) => Some(Err(e)),
            }
        }))
    }
}