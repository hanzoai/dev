mod pkce;
mod server;

pub use server::LoginServer;
pub use server::ServerOptions;
pub use server::ShutdownHandle;
pub use server::run_login_server;

// Re-export commonly used auth types and helpers from codex-core for compatibility
pub use hanzo_dev::AuthManager;
pub use hanzo_dev::CodexAuth;
pub use hanzo_dev::auth::AuthDotJson;
pub use hanzo_dev::auth::CLIENT_ID;
pub use hanzo_dev::auth::OPENAI_API_KEY_ENV_VAR;
pub use hanzo_dev::auth::get_auth_file;
pub use hanzo_dev::auth::login_with_api_key;
pub use hanzo_dev::auth::logout;
pub use hanzo_dev::auth::try_read_auth_json;
pub use hanzo_dev::auth::write_auth_json;
pub use hanzo_dev::token_data::TokenData;
pub use dev_protocol::mcp_protocol::AuthMode;
