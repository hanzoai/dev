//! gRPC server implementation
//!
//! Implements the NodeService defined in hanzo.node.v1.proto

mod generated;
mod service;

pub use service::RpcServer;

// Re-export generated types for convenience
pub use generated::hanzo::node::v1::*;
