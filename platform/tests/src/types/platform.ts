/**
 * Platform Types - Derived from database schema
 * /Users/z/work/hanzo/dev/platform/db/migrations/001_initial_schema.sql
 */

// Resource types supported by the platform
export type ResourceType =
  | 'CPU'
  | 'GPU'
  | 'Memory'
  | 'Storage'
  | 'Bandwidth'
  | 'WASM'
  | 'Docker'
  | 'K8S';

// Pool status for lifecycle management
export type PoolStatus = 'active' | 'paused' | 'deprecated' | 'closed';

// Node status for compute providers
export type NodeStatus =
  | 'pending'
  | 'active'
  | 'suspended'
  | 'offline'
  | 'maintenance'
  | 'decommissioned';

// Offer status for compute offers
export type OfferStatus =
  | 'active'
  | 'partially_filled'
  | 'filled'
  | 'expired'
  | 'cancelled';

// Challenge status for SLA verification
export type ChallengeStatus =
  | 'pending'
  | 'passed'
  | 'failed'
  | 'timeout'
  | 'disputed';

// Membership role in pool governance
export type MembershipRole = 'provider' | 'liquidity' | 'consumer' | 'operator';

// Challenge types from QoS system design
export type ChallengeType =
  | 'availability'
  | 'performance'
  | 'attestation'
  | 'compute'
  | 'latency'
  | 'bandwidth'
  | 'model';

// Reward types
export type RewardType = 'trading_fee' | 'liquidity_mining' | 'sla_bonus';

/**
 * Compute Pool entity
 */
export interface ComputePool {
  id: string;
  name: string;
  slug: string;
  description?: string;
  resource_type: ResourceType;
  resource_amount: bigint;
  token_amount: bigint;
  total_liquidity: bigint;
  last_price: bigint;
  fee_rate: number;
  volume_24h: bigint;
  volume_7d: bigint;
  volume_total: bigint;
  trade_count_24h: number;
  trade_count_total: number;
  price_change_1h: number;
  price_change_24h: number;
  price_change_7d: number;
  utilization: number;
  avg_sla_score: number;
  apr: number;
  status: PoolStatus;
  min_stake: bigint;
  created_at: Date;
  updated_at: Date;
}

/**
 * Compute Node entity
 */
export interface ComputeNode {
  id: string;
  owner_address: string;
  name?: string;
  description?: string;
  cpu_cores: number;
  cpu_model?: string;
  memory_mb: number;
  storage_gb: number;
  gpu_count: number;
  gpu_model?: string;
  bandwidth_mbps: number;
  supports_docker: boolean;
  supports_k8s: boolean;
  supports_wasm: boolean;
  region?: string;
  availability_zone?: string;
  ip_address?: string;
  p2p_address?: string;
  tee_type?: string;
  attestation_quote?: Uint8Array;
  attestation_verified_at?: Date;
  sla_score: number;
  uptime_percent: number;
  avg_response_time_ms: number;
  total_tasks_completed: number;
  total_tasks_failed: number;
  stake_amount: bigint;
  stake_locked_until?: Date;
  status: NodeStatus;
  last_heartbeat_at?: Date;
  created_at: Date;
  updated_at: Date;
}

/**
 * Compute Offer entity
 */
export interface ComputeOffer {
  id: string;
  node_id: string;
  pool_id?: string;
  resource_type: ResourceType;
  amount: bigint;
  amount_remaining: bigint;
  price_per_unit: bigint;
  min_duration_hours: number;
  max_duration_hours: number;
  guaranteed_uptime: number;
  max_latency_ms?: number;
  status: OfferStatus;
  expires_at: Date;
  created_at: Date;
  updated_at: Date;
}

/**
 * Pool Membership entity
 */
export interface PoolMembership {
  id: string;
  pool_id: string;
  node_id?: string;
  user_address: string;
  role: MembershipRole;
  liquidity_shares: bigint;
  resource_deposited: bigint;
  token_deposited: bigint;
  entry_price: bigint;
  pending_rewards: bigint;
  claimed_rewards: bigint;
  last_reward_claim_at?: Date;
  committed_resources: bigint;
  utilized_resources: bigint;
  member_sla_score: number;
  is_active: boolean;
  joined_at: Date;
  left_at?: Date;
  created_at: Date;
  updated_at: Date;
}

/**
 * Challenge entity
 */
export interface Challenge {
  id: string;
  node_id: string;
  pool_id?: string;
  membership_id?: string;
  challenge_type: ChallengeType;
  challenge_data: ChallengeData;
  response_data?: ChallengeResponse;
  response_at?: Date;
  verified_by?: string;
  verification_proof?: Uint8Array;
  verified_at?: Date;
  status: ChallengeStatus;
  score?: number;
  penalty_amount: bigint;
  deadline: Date;
  created_at: Date;
  updated_at: Date;
}

/**
 * Challenge data structure
 */
export interface ChallengeData {
  seed?: string;
  difficulty?: number;
  nonce?: string;
  rounds?: number;
  expected_latency_ms?: number;
  expected_throughput_mbps?: number;
}

/**
 * Challenge response structure
 */
export interface ChallengeResponse {
  result_hash?: string;
  intermediate_hashes?: string[];
  actual_flops?: number;
  latency_ms?: number;
  throughput_mbps?: number;
  signed_nonce?: string;
}

/**
 * Reward entity
 */
export interface Reward {
  id: string;
  pool_id: string;
  membership_id: string;
  user_address: string;
  reward_type: RewardType;
  amount: bigint;
  source_tx_hash?: string;
  source_trade_id?: string;
  epoch_number?: number;
  epoch_start?: Date;
  epoch_end?: Date;
  is_claimed: boolean;
  claimed_at?: Date;
  claim_tx_hash?: string;
  created_at: Date;
}

/**
 * Node Registration Request
 */
export interface NodeRegistrationRequest {
  owner_address: string;
  name?: string;
  description?: string;
  cpu_cores: number;
  cpu_model?: string;
  memory_mb: number;
  storage_gb: number;
  gpu_count?: number;
  gpu_model?: string;
  bandwidth_mbps: number;
  supports_docker?: boolean;
  supports_k8s?: boolean;
  supports_wasm?: boolean;
  region?: string;
  availability_zone?: string;
  p2p_address?: string;
  tee_type?: string;
  initial_stake: bigint;
}

/**
 * Pool Creation Request
 */
export interface PoolCreationRequest {
  name: string;
  slug: string;
  description?: string;
  resource_type: ResourceType;
  initial_resource_amount: bigint;
  initial_token_amount: bigint;
  fee_rate?: number;
  min_stake?: bigint;
}

/**
 * QoS Score structure (from QOS_CHALLENGE_SYSTEM_DESIGN.md)
 */
export interface QoSScore {
  composite: number;
  compute_score: number;
  latency_score: number;
  bandwidth_score: number;
  availability_score: number;
  consistency_score: number;
  confidence: number;
  updated_at: Date;
  trend: number;
}

/**
 * Reward Distribution Request
 */
export interface RewardDistributionRequest {
  pool_id: string;
  epoch_number: number;
  epoch_start: Date;
  epoch_end: Date;
  total_trading_fees: bigint;
  total_liquidity_rewards: bigint;
  total_sla_bonuses: bigint;
}

/**
 * Verification Result
 */
export interface VerificationResult {
  challenge_id: string;
  passed: boolean;
  score: number;
  bonus?: number;
  penalty_amount?: bigint;
  failure_reason?: string;
}
