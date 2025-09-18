//! Development utilities

use hanzoai::{Hanzo, Message, Result};
use std::time::Instant;

/// Development helper for testing and debugging
pub struct DevHelper {
    client: Hanzo,
    log_requests: bool,
    measure_time: bool,
}

impl DevHelper {
    pub fn new(client: Hanzo) -> Self {
        Self {
            client,
            log_requests: false,
            measure_time: false,
        }
    }

    pub fn with_logging(mut self, enable: bool) -> Self {
        self.log_requests = enable;
        self
    }

    pub fn with_timing(mut self, enable: bool) -> Self {
        self.measure_time = enable;
        self
    }

    /// Test a prompt with timing and logging
    pub async fn test_prompt(&self, prompt: &str) -> Result<String> {
        if self.log_requests {
            println!("📤 Request: {}", prompt);
        }

        let start = if self.measure_time {
            Some(Instant::now())
        } else {
            None
        };

        let response = self.client.chat().create(vec![
            Message::user(prompt)
        ]).await?;

        if let Some(start) = start {
            println!("⏱️  Time: {:?}", start.elapsed());
        }

        let text = response.text().to_string();

        if self.log_requests {
            println!("📥 Response: {}", text);
        }

        Ok(text)
    }

    /// Benchmark a prompt with multiple runs
    pub async fn benchmark(&self, prompt: &str, runs: usize) -> Result<BenchmarkResult> {
        let mut times = Vec::new();
        let mut tokens = Vec::new();

        for i in 0..runs {
            println!("Run {}/{}", i + 1, runs);

            let start = Instant::now();
            let response = self.client.chat().create(vec![
                Message::user(prompt)
            ]).await?;
            let duration = start.elapsed();

            times.push(duration.as_millis());

            if let Some(usage) = response.usage {
                tokens.push(usage.total_tokens as u64);
            }
        }

        let avg_time = times.iter().sum::<u128>() / times.len() as u128;
        let avg_tokens = if !tokens.is_empty() {
            tokens.iter().sum::<u64>() / tokens.len() as u64
        } else {
            0
        };

        Ok(BenchmarkResult {
            runs,
            avg_time_ms: avg_time,
            avg_tokens,
            min_time_ms: *times.iter().min().unwrap_or(&0),
            max_time_ms: *times.iter().max().unwrap_or(&0),
        })
    }

    /// Test conversation flow
    pub async fn test_conversation(&self, messages: Vec<(&str, &str)>) -> Result<Vec<String>> {
        let mut conversation = Vec::new();
        let mut responses = Vec::new();

        for (role, content) in messages {
            let msg = match role {
                "user" => Message::user(content),
                "assistant" => Message::assistant(content),
                "system" => Message::system(content),
                _ => Message::user(content),
            };

            conversation.push(msg);

            if role == "user" {
                let response = self.client.chat().create(conversation.clone()).await?;
                let text = response.text().to_string();
                responses.push(text.clone());
                conversation.push(Message::assistant(text));
            }
        }

        Ok(responses)
    }
}

#[derive(Debug)]
pub struct BenchmarkResult {
    pub runs: usize,
    pub avg_time_ms: u128,
    pub avg_tokens: u64,
    pub min_time_ms: u128,
    pub max_time_ms: u128,
}

impl std::fmt::Display for BenchmarkResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Benchmark Results:")?;
        writeln!(f, "  Runs: {}", self.runs)?;
        writeln!(f, "  Average Time: {}ms", self.avg_time_ms)?;
        writeln!(f, "  Min Time: {}ms", self.min_time_ms)?;
        writeln!(f, "  Max Time: {}ms", self.max_time_ms)?;
        if self.avg_tokens > 0 {
            writeln!(f, "  Average Tokens: {}", self.avg_tokens)?;
        }
        Ok(())
    }
}