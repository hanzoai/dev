//! Hanzod library exports

pub mod cli;
pub mod runtime_detection;
pub mod docker_provider;
#[cfg(feature = "sled")]
pub mod workload_manager;
pub mod sandboxer;

// Export additional modules if they exist
#[cfg(feature = "chain")]
pub mod blockchain;

// Re-export KeyManager for easy access
#[cfg(feature = "chain")]
pub use blockchain::KeyManager;

#[cfg(feature = "market")]
pub mod marketplace;

#[cfg(feature = "staking")]
pub mod staking;

// Database and Chain modules - always included
pub mod database;
mod ledger;
mod vector_store;
pub mod chain;
pub mod api_gateway;

// AI Model support
pub mod qwen3_models;

// RPC and consensus modules
pub mod lux_consensus;
pub mod rpc_server;
pub mod warp_ffi;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_exports() {
        // Ensure all public modules are accessible
        use crate::lux_consensus::LuxConsensusConfig;
        use crate::warp_ffi::UnsignedMessage;
        
        let _ = LuxConsensusConfig::default();
        assert!(true, "Modules are accessible");
    }
}