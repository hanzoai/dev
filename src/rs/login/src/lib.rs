mod pkce;
mod server;

pub use server::LoginServer;
pub use server::ServerOptions;
pub use server::ShutdownHandle;
pub use server::run_login_server;

// Re-export commonly used auth types and helpers from codex-core for compatibility
pub use dev_core::AuthManager;
pub use dev_core::CodexAuth;
pub use dev_core::auth::AuthDotJson;
pub use dev_core::auth::CLIENT_ID;
pub use dev_core::auth::OPENAI_API_KEY_ENV_VAR;
pub use dev_core::auth::get_auth_file;
pub use dev_core::auth::login_with_api_key;
pub use dev_core::auth::logout;
pub use dev_core::auth::try_read_auth_json;
pub use dev_core::auth::write_auth_json;
pub use dev_core::token_data::TokenData;
pub use dev_protocol::mcp_protocol::AuthMode;
