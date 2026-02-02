//! Policy Engine for Transaction Approvals
//!
//! Provides rule-based approval logic for MPC wallet operations:
//! - Value thresholds (auto-approve below certain amounts)
//! - Whitelist/blacklist destination addresses
//! - Rate limiting
//! - Multi-approver requirements
//! - Time-based restrictions

use crate::error::WalletResult;
use crate::signing::SigningMetadata;

use chrono::Timelike;
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::{Duration, Instant};

/// A single policy rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PolicyRule {
    /// Auto-approve transactions below this value
    MaxAutoApproveValue { value: u64 },

    /// Require manual approval for transactions above this value
    ManualApprovalThreshold { value: u64 },

    /// Require N approvers for high-value transactions
    RequireApprovers {
        count: usize,
        for_value_above: Option<u64>,
    },

    /// Only allow transactions to whitelisted addresses
    WhitelistOnly { addresses: Vec<String> },

    /// Block transactions to blacklisted addresses
    Blacklist { addresses: Vec<String> },

    /// Rate limit: max transactions per time window
    RateLimit {
        max_count: u32,
        window_seconds: u64,
    },

    /// Daily spending limit
    DailySpendingLimit { max_value: u64 },

    /// Time-based restriction (e.g., no transactions outside business hours)
    TimeRestriction {
        allowed_hours_start: u8, // 0-23
        allowed_hours_end: u8,   // 0-23
        timezone_offset_hours: i8,
    },

    /// Require specific chain ID
    AllowedChains { chain_ids: Vec<u64> },

    /// Custom rule (evaluated by external system)
    Custom { rule_id: String, params: HashMap<String, String> },
}

/// Result of policy evaluation
#[derive(Debug, Clone)]
pub enum PolicyResult {
    /// Transaction is approved
    Approved,

    /// Transaction requires manual approval
    RequiresApproval { reason: String, required_approvers: usize },

    /// Transaction is denied
    Denied { reason: String },

    /// Transaction is pending (waiting for additional approvals)
    Pending { received: usize, required: usize },
}

impl PolicyResult {
    pub fn is_approved(&self) -> bool {
        matches!(self, PolicyResult::Approved)
    }

    pub fn is_denied(&self) -> bool {
        matches!(self, PolicyResult::Denied { .. })
    }
}

/// An approval policy consisting of multiple rules
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApprovalPolicy {
    /// Policy name
    pub name: String,

    /// Policy description
    pub description: Option<String>,

    /// Rules to evaluate (all must pass)
    pub rules: Vec<PolicyRule>,

    /// Whether policy is enabled
    pub enabled: bool,

    /// Priority (higher = evaluated first)
    pub priority: i32,
}

impl Default for ApprovalPolicy {
    fn default() -> Self {
        Self {
            name: "default".to_string(),
            description: Some("Default policy".to_string()),
            rules: vec![],
            enabled: true,
            priority: 0,
        }
    }
}

/// Tracks rate limiting and spending data
struct PolicyState {
    /// Transaction timestamps for rate limiting
    transaction_times: Vec<Instant>,

    /// Daily spending by date (YYYY-MM-DD -> total spent)
    daily_spending: HashMap<String, u64>,

    /// Pending approvals: session_id -> (received_approvers, required_approvers)
    pending_approvals: HashMap<String, (HashSet<String>, usize)>,
}

impl PolicyState {
    fn new() -> Self {
        Self {
            transaction_times: Vec::new(),
            daily_spending: HashMap::new(),
            pending_approvals: HashMap::new(),
        }
    }

    fn cleanup_old_transactions(&mut self, window: Duration) {
        let cutoff = Instant::now() - window;
        self.transaction_times.retain(|t| *t > cutoff);
    }

    fn today_key() -> String {
        let now = chrono::Utc::now();
        now.format("%Y-%m-%d").to_string()
    }

    fn get_daily_spent(&self) -> u64 {
        let today = Self::today_key();
        *self.daily_spending.get(&today).unwrap_or(&0)
    }

    fn add_spending(&mut self, amount: u64) {
        let today = Self::today_key();
        *self.daily_spending.entry(today).or_insert(0) += amount;
    }

    fn record_transaction(&mut self) {
        self.transaction_times.push(Instant::now());
    }
}

/// Policy engine for evaluating transaction approvals
pub struct PolicyEngine {
    /// Policies to evaluate
    policies: Arc<RwLock<Vec<ApprovalPolicy>>>,

    /// Policy state (rate limits, spending, etc.)
    state: Arc<RwLock<PolicyState>>,
}

impl PolicyEngine {
    /// Create a new policy engine
    pub fn new() -> Self {
        Self {
            policies: Arc::new(RwLock::new(Vec::new())),
            state: Arc::new(RwLock::new(PolicyState::new())),
        }
    }

    /// Create a policy engine with initial policies
    pub fn with_policies(policies: Vec<ApprovalPolicy>) -> Self {
        let engine = Self::new();
        *engine.policies.write() = policies;
        engine
    }

    /// Add a policy
    pub fn add_policy(&self, policy: ApprovalPolicy) {
        let mut policies = self.policies.write();
        policies.push(policy);
        policies.sort_by(|a, b| b.priority.cmp(&a.priority));
    }

    /// Remove a policy by name
    pub fn remove_policy(&self, name: &str) {
        self.policies.write().retain(|p| p.name != name);
    }

    /// Get all policies
    pub fn get_policies(&self) -> Vec<ApprovalPolicy> {
        self.policies.read().clone()
    }

    /// Enable or disable a policy
    pub fn set_policy_enabled(&self, name: &str, enabled: bool) {
        let mut policies = self.policies.write();
        if let Some(policy) = policies.iter_mut().find(|p| p.name == name) {
            policy.enabled = enabled;
        }
    }

    /// Evaluate a transaction against all policies
    pub fn evaluate(&self, metadata: &SigningMetadata) -> WalletResult<PolicyResult> {
        let policies = self.policies.read();
        let mut state = self.state.write();

        for policy in policies.iter().filter(|p| p.enabled) {
            match self.evaluate_policy(policy, metadata, &mut state)? {
                PolicyResult::Approved => continue,
                result => return Ok(result),
            }
        }

        Ok(PolicyResult::Approved)
    }

    /// Evaluate a single policy
    fn evaluate_policy(
        &self,
        policy: &ApprovalPolicy,
        metadata: &SigningMetadata,
        state: &mut PolicyState,
    ) -> WalletResult<PolicyResult> {
        for rule in &policy.rules {
            match self.evaluate_rule(rule, metadata, state)? {
                PolicyResult::Approved => continue,
                result => return Ok(result),
            }
        }
        Ok(PolicyResult::Approved)
    }

    /// Evaluate a single rule
    fn evaluate_rule(
        &self,
        rule: &PolicyRule,
        metadata: &SigningMetadata,
        state: &mut PolicyState,
    ) -> WalletResult<PolicyResult> {
        match rule {
            PolicyRule::MaxAutoApproveValue { value } => {
                if let Some(tx_value) = metadata.value {
                    if tx_value <= *value {
                        return Ok(PolicyResult::Approved);
                    } else {
                        return Ok(PolicyResult::RequiresApproval {
                            reason: format!(
                                "Transaction value {tx_value} exceeds auto-approve threshold {value}"
                            ),
                            required_approvers: 1,
                        });
                    }
                }
                Ok(PolicyResult::Approved)
            }

            PolicyRule::ManualApprovalThreshold { value } => {
                if let Some(tx_value) = metadata.value {
                    if tx_value > *value {
                        return Ok(PolicyResult::RequiresApproval {
                            reason: format!(
                                "Transaction value {tx_value} exceeds manual approval threshold {value}"
                            ),
                            required_approvers: 1,
                        });
                    }
                }
                Ok(PolicyResult::Approved)
            }

            PolicyRule::RequireApprovers {
                count,
                for_value_above,
            } => {
                let requires_multi = match (metadata.value, for_value_above) {
                    (Some(tx_value), Some(threshold)) => tx_value > *threshold,
                    (_, None) => true, // Always require if no threshold
                    _ => false,
                };

                if requires_multi && *count > 1 {
                    return Ok(PolicyResult::RequiresApproval {
                        reason: format!("Transaction requires {count} approvers"),
                        required_approvers: *count,
                    });
                }
                Ok(PolicyResult::Approved)
            }

            PolicyRule::WhitelistOnly { addresses } => {
                if let Some(dest) = &metadata.destination {
                    let normalized_dest = dest.to_lowercase();
                    let normalized_whitelist: Vec<_> =
                        addresses.iter().map(|a| a.to_lowercase()).collect();

                    if !normalized_whitelist.contains(&normalized_dest) {
                        return Ok(PolicyResult::Denied {
                            reason: format!("Address {dest} is not whitelisted"),
                        });
                    }
                }
                Ok(PolicyResult::Approved)
            }

            PolicyRule::Blacklist { addresses } => {
                if let Some(dest) = &metadata.destination {
                    let normalized_dest = dest.to_lowercase();
                    let normalized_blacklist: Vec<_> =
                        addresses.iter().map(|a| a.to_lowercase()).collect();

                    if normalized_blacklist.contains(&normalized_dest) {
                        return Ok(PolicyResult::Denied {
                            reason: format!("Address {dest} is blacklisted"),
                        });
                    }
                }
                Ok(PolicyResult::Approved)
            }

            PolicyRule::RateLimit {
                max_count,
                window_seconds,
            } => {
                let window = Duration::from_secs(*window_seconds);
                state.cleanup_old_transactions(window);

                if state.transaction_times.len() >= *max_count as usize {
                    return Ok(PolicyResult::Denied {
                        reason: format!(
                            "Rate limit exceeded: {max_count} transactions in {window_seconds} seconds"
                        ),
                    });
                }
                Ok(PolicyResult::Approved)
            }

            PolicyRule::DailySpendingLimit { max_value } => {
                let spent = state.get_daily_spent();
                let tx_value = metadata.value.unwrap_or(0);

                if spent + tx_value > *max_value {
                    return Ok(PolicyResult::Denied {
                        reason: format!(
                            "Daily spending limit exceeded: spent {} + {} = {} > {}",
                            spent,
                            tx_value,
                            spent + tx_value,
                            max_value
                        ),
                    });
                }
                Ok(PolicyResult::Approved)
            }

            PolicyRule::TimeRestriction {
                allowed_hours_start,
                allowed_hours_end,
                timezone_offset_hours,
            } => {
                let now = chrono::Utc::now();
                let local_hour =
                    (now.hour() as i32 + *timezone_offset_hours as i32).rem_euclid(24) as u8;

                let in_window = if allowed_hours_start <= allowed_hours_end {
                    local_hour >= *allowed_hours_start && local_hour < *allowed_hours_end
                } else {
                    // Window crosses midnight
                    local_hour >= *allowed_hours_start || local_hour < *allowed_hours_end
                };

                if !in_window {
                    return Ok(PolicyResult::Denied {
                        reason: format!(
                            "Transaction outside allowed hours ({}-{} UTC{}{})",
                            allowed_hours_start,
                            allowed_hours_end,
                            if *timezone_offset_hours >= 0 { "+" } else { "" },
                            timezone_offset_hours
                        ),
                    });
                }
                Ok(PolicyResult::Approved)
            }

            PolicyRule::AllowedChains { chain_ids } => {
                if let Some(chain_id) = metadata.chain_id {
                    if !chain_ids.contains(&chain_id) {
                        return Ok(PolicyResult::Denied {
                            reason: format!("Chain ID {chain_id} is not allowed"),
                        });
                    }
                }
                Ok(PolicyResult::Approved)
            }

            PolicyRule::Custom { rule_id, params: _ } => {
                // Custom rules would be evaluated by an external system
                tracing::debug!("Custom rule {} - allowing by default", rule_id);
                Ok(PolicyResult::Approved)
            }
        }
    }

    /// Record a transaction for rate limiting and spending tracking
    pub fn record_transaction(&self, metadata: &SigningMetadata) {
        let mut state = self.state.write();
        state.record_transaction();

        if let Some(value) = metadata.value {
            state.add_spending(value);
        }
    }

    /// Submit an approval for a pending transaction
    pub fn submit_approval(
        &self,
        session_id: &str,
        approver_id: &str,
        required: usize,
    ) -> PolicyResult {
        let mut state = self.state.write();

        let (approvers, required_count) = state
            .pending_approvals
            .entry(session_id.to_string())
            .or_insert_with(|| (HashSet::new(), required));

        approvers.insert(approver_id.to_string());

        if approvers.len() >= *required_count {
            state.pending_approvals.remove(session_id);
            PolicyResult::Approved
        } else {
            PolicyResult::Pending {
                received: approvers.len(),
                required: *required_count,
            }
        }
    }

    /// Check approval status for a session
    pub fn check_approval_status(&self, session_id: &str) -> Option<PolicyResult> {
        let state = self.state.read();

        state.pending_approvals.get(session_id).map(|(approvers, required)| {
            if approvers.len() >= *required {
                PolicyResult::Approved
            } else {
                PolicyResult::Pending {
                    received: approvers.len(),
                    required: *required,
                }
            }
        })
    }

    /// Cancel a pending approval
    pub fn cancel_approval(&self, session_id: &str) {
        self.state.write().pending_approvals.remove(session_id);
    }
}

impl Default for PolicyEngine {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for creating policies
pub struct PolicyBuilder {
    policy: ApprovalPolicy,
}

impl PolicyBuilder {
    pub fn new(name: &str) -> Self {
        Self {
            policy: ApprovalPolicy {
                name: name.to_string(),
                ..Default::default()
            },
        }
    }

    pub fn description(mut self, desc: &str) -> Self {
        self.policy.description = Some(desc.to_string());
        self
    }

    pub fn priority(mut self, priority: i32) -> Self {
        self.policy.priority = priority;
        self
    }

    pub fn max_auto_approve(mut self, value: u64) -> Self {
        self.policy
            .rules
            .push(PolicyRule::MaxAutoApproveValue { value });
        self
    }

    pub fn require_approvers(mut self, count: usize, for_value_above: Option<u64>) -> Self {
        self.policy.rules.push(PolicyRule::RequireApprovers {
            count,
            for_value_above,
        });
        self
    }

    pub fn whitelist(mut self, addresses: Vec<String>) -> Self {
        self.policy
            .rules
            .push(PolicyRule::WhitelistOnly { addresses });
        self
    }

    pub fn blacklist(mut self, addresses: Vec<String>) -> Self {
        self.policy.rules.push(PolicyRule::Blacklist { addresses });
        self
    }

    pub fn rate_limit(mut self, max_count: u32, window_seconds: u64) -> Self {
        self.policy.rules.push(PolicyRule::RateLimit {
            max_count,
            window_seconds,
        });
        self
    }

    pub fn daily_spending_limit(mut self, max_value: u64) -> Self {
        self.policy
            .rules
            .push(PolicyRule::DailySpendingLimit { max_value });
        self
    }

    pub fn allowed_chains(mut self, chain_ids: Vec<u64>) -> Self {
        self.policy
            .rules
            .push(PolicyRule::AllowedChains { chain_ids });
        self
    }

    pub fn build(self) -> ApprovalPolicy {
        self.policy
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_metadata(value: Option<u64>, destination: Option<&str>) -> SigningMetadata {
        SigningMetadata {
            value,
            destination: destination.map(|s| s.to_string()),
            chain_id: Some(1),
            tx_type: Some("transfer".to_string()),
        }
    }

    #[test]
    fn test_auto_approve_below_threshold() {
        let engine = PolicyEngine::new();

        let policy = PolicyBuilder::new("test")
            .max_auto_approve(1000)
            .build();

        engine.add_policy(policy);

        // Below threshold - approved
        let result = engine.evaluate(&test_metadata(Some(500), None)).unwrap();
        assert!(result.is_approved());

        // Above threshold - requires approval
        let result = engine.evaluate(&test_metadata(Some(1500), None)).unwrap();
        assert!(matches!(result, PolicyResult::RequiresApproval { .. }));
    }

    #[test]
    fn test_whitelist() {
        let engine = PolicyEngine::new();

        let policy = PolicyBuilder::new("whitelist")
            .whitelist(vec![
                "0x1234567890abcdef".to_string(),
                "0xfedcba0987654321".to_string(),
            ])
            .build();

        engine.add_policy(policy);

        // Whitelisted address
        let result = engine
            .evaluate(&test_metadata(None, Some("0x1234567890abcdef")))
            .unwrap();
        assert!(result.is_approved());

        // Non-whitelisted address
        let result = engine
            .evaluate(&test_metadata(None, Some("0xbadaddress")))
            .unwrap();
        assert!(result.is_denied());
    }

    #[test]
    fn test_blacklist() {
        let engine = PolicyEngine::new();

        let policy = PolicyBuilder::new("blacklist")
            .blacklist(vec!["0xbadaddress".to_string()])
            .build();

        engine.add_policy(policy);

        // Good address
        let result = engine
            .evaluate(&test_metadata(None, Some("0xgoodaddress")))
            .unwrap();
        assert!(result.is_approved());

        // Blacklisted address
        let result = engine
            .evaluate(&test_metadata(None, Some("0xbadaddress")))
            .unwrap();
        assert!(result.is_denied());
    }

    #[test]
    fn test_rate_limit() {
        let engine = PolicyEngine::new();

        let policy = PolicyBuilder::new("rate-limit")
            .rate_limit(3, 3600) // 3 per hour
            .build();

        engine.add_policy(policy);

        let metadata = test_metadata(Some(100), None);

        // First 3 should pass
        for _ in 0..3 {
            let result = engine.evaluate(&metadata).unwrap();
            assert!(result.is_approved());
            engine.record_transaction(&metadata);
        }

        // 4th should fail
        let result = engine.evaluate(&metadata).unwrap();
        assert!(result.is_denied());
    }

    #[test]
    fn test_multi_approver() {
        let engine = PolicyEngine::new();

        let policy = PolicyBuilder::new("multi-sign")
            .require_approvers(2, Some(10000))
            .build();

        engine.add_policy(policy);

        // Low value - no multi-sig needed
        let result = engine.evaluate(&test_metadata(Some(5000), None)).unwrap();
        assert!(result.is_approved());

        // High value - requires 2 approvers
        let result = engine.evaluate(&test_metadata(Some(15000), None)).unwrap();
        assert!(matches!(
            result,
            PolicyResult::RequiresApproval {
                required_approvers: 2,
                ..
            }
        ));
    }

    #[test]
    fn test_approval_flow() {
        let engine = PolicyEngine::new();

        // Submit first approval
        let result = engine.submit_approval("session-1", "approver-1", 2);
        assert!(matches!(
            result,
            PolicyResult::Pending {
                received: 1,
                required: 2
            }
        ));

        // Submit second approval
        let result = engine.submit_approval("session-1", "approver-2", 2);
        assert!(result.is_approved());

        // Session should be cleared
        assert!(engine.check_approval_status("session-1").is_none());
    }

    #[test]
    fn test_allowed_chains() {
        let engine = PolicyEngine::new();

        let policy = PolicyBuilder::new("chains")
            .allowed_chains(vec![1, 137, 42161]) // Mainnet, Polygon, Arbitrum
            .build();

        engine.add_policy(policy);

        // Allowed chain
        let mut metadata = test_metadata(None, None);
        metadata.chain_id = Some(1);
        let result = engine.evaluate(&metadata).unwrap();
        assert!(result.is_approved());

        // Disallowed chain
        metadata.chain_id = Some(56); // BSC
        let result = engine.evaluate(&metadata).unwrap();
        assert!(result.is_denied());
    }

    #[test]
    fn test_policy_builder() {
        let policy = PolicyBuilder::new("comprehensive")
            .description("A comprehensive policy")
            .priority(10)
            .max_auto_approve(1000)
            .daily_spending_limit(100000)
            .whitelist(vec!["0xsafe".to_string()])
            .rate_limit(10, 3600)
            .build();

        assert_eq!(policy.name, "comprehensive");
        assert_eq!(policy.priority, 10);
        assert_eq!(policy.rules.len(), 4);
    }
}
