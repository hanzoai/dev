//! Hanzo AI SDK - Meta package with dev tools
//!
//! This crate re-exports the core hanzoai functionality and adds
//! development tools, CLI, and extended features.

// Re-export everything from core hanzoai
pub use hanzoai::*;

// Dev tools modules
pub mod dev;
pub mod streaming;
pub mod tools;

// Optional features
#[cfg(feature = "hanzo-batch-orchestrator")]
pub mod batch_orchestrator {
    pub use hanzo_batch_orchestrator::*;
}

#[cfg(feature = "hanzo-memory-manager")]
pub mod memory_manager {
    pub use hanzo_memory_manager::*;
}

#[cfg(feature = "hanzo-model-registry")]
pub mod model_registry {
    pub use hanzo_model_registry::*;
}

/// Dev client implementation - matches Python's dev.py
pub struct DevClient {
    client: hanzoai::Hanzo,
    debug: bool,
    trace_requests: bool,
}

impl DevClient {
    pub fn new() -> hanzoai::Result<Self> {
        Ok(Self {
            client: hanzoai::Hanzo::from_env()?,
            debug: false,
            trace_requests: false,
        })
    }

    pub fn with_debug(mut self, debug: bool) -> Self {
        self.debug = debug;
        self
    }

    pub fn with_tracing(mut self, trace: bool) -> Self {
        self.trace_requests = trace;
        self
    }

    pub async fn chat(&self, message: &str) -> hanzoai::Result<String> {
        if self.debug {
            eprintln!("🔍 Debug: Sending message: {}", message);
        }

        let response = self.client.chat().create(vec![
            hanzoai::Message::user(message)
        ]).await?;

        if self.debug {
            eprintln!("✅ Debug: Received response with {} tokens",
                response.usage.as_ref().map(|u| u.total_tokens).unwrap_or(0));
        }

        Ok(response.text().to_string())
    }

    /// Interactive REPL mode
    pub async fn repl(&self) -> hanzoai::Result<()> {
        use std::io::{self, Write};

        println!("🤖 Hanzo Dev Client - Interactive Mode");
        println!("Type 'exit' to quit, 'help' for commands\n");

        let mut messages = Vec::new();

        loop {
            print!("> ");
            io::stdout().flush().unwrap();

            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();
            let input = input.trim();

            match input {
                "exit" | "quit" => break,
                "help" => {
                    println!("Commands:");
                    println!("  exit/quit - Exit the REPL");
                    println!("  clear     - Clear conversation history");
                    println!("  model     - Show current model");
                    println!("  debug     - Toggle debug mode");
                    continue;
                }
                "clear" => {
                    messages.clear();
                    println!("Conversation cleared.");
                    continue;
                }
                "debug" => {
                    let debug = !self.debug;
                    println!("Debug mode: {}", if debug { "ON" } else { "OFF" });
                    continue;
                }
                "" => continue,
                _ => {}
            }

            messages.push(hanzoai::Message::user(input));

            match self.client.chat().create(messages.clone()).await {
                Ok(response) => {
                    let text = response.text();
                    println!("\n{}\n", text);
                    messages.push(hanzoai::Message::assistant(text));
                }
                Err(e) => {
                    eprintln!("Error: {}", e);
                }
            }
        }

        Ok(())
    }
}