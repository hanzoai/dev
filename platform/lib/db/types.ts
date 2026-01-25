/**
 * Database Types for Hanzo Compute Pools Platform
 *
 * These TypeScript types mirror the PostgreSQL schema defined in:
 * /platform/db/migrations/001_initial_schema.sql
 * /platform/db/migrations/002_trades_and_history.sql
 *
 * All numeric fields use string representation for BigInt compatibility.
 */

// =============================================================================
// ENUMS
// =============================================================================

export enum ResourceType {
  CPU = 'CPU',
  GPU = 'GPU',
  Memory = 'Memory',
  Storage = 'Storage',
  Bandwidth = 'Bandwidth',
  WASM = 'WASM',
  Docker = 'Docker',
  K8S = 'K8S',
}

export enum PoolStatus {
  Active = 'active',
  Paused = 'paused',
  Deprecated = 'deprecated',
  Closed = 'closed',
}

export enum NodeStatus {
  Pending = 'pending',
  Active = 'active',
  Suspended = 'suspended',
  Offline = 'offline',
  Maintenance = 'maintenance',
  Decommissioned = 'decommissioned',
}

export enum OfferStatus {
  Active = 'active',
  PartiallyFilled = 'partially_filled',
  Filled = 'filled',
  Expired = 'expired',
  Cancelled = 'cancelled',
}

export enum ChallengeStatus {
  Pending = 'pending',
  Passed = 'passed',
  Failed = 'failed',
  Timeout = 'timeout',
  Disputed = 'disputed',
}

export enum MembershipRole {
  Provider = 'provider',
  Liquidity = 'liquidity',
  Consumer = 'consumer',
  Operator = 'operator',
}

export enum TradeDirection {
  Buy = 'buy',
  Sell = 'sell',
}

export enum TradeRoute {
  AMM = 'amm',
  OrderBook = 'orderbook',
  Hybrid = 'hybrid',
}

// =============================================================================
// CORE TABLES
// =============================================================================

/**
 * Compute Pool - AMM liquidity pool for compute resources
 */
export interface ComputePool {
  id: string;

  // Identity
  name: string;
  slug: string;
  description: string | null;
  resourceType: ResourceType;

  // Balances (wei precision as string)
  resourceAmount: string;
  tokenAmount: string;
  totalLiquidity: string;

  // Pricing
  lastPrice: string;
  feeRate: number;

  // Metrics
  volume24h: string;
  volume7d: string;
  volumeTotal: string;
  tradeCount24h: number;
  tradeCountTotal: number;

  // Price changes (decimal, e.g., 0.05 = 5%)
  priceChange1h: number;
  priceChange24h: number;
  priceChange7d: number;

  // Pool health
  utilization: number;
  avgSlaScore: number;
  apr: number;

  // Governance
  status: PoolStatus;
  minStake: string;

  // Timestamps
  createdAt: Date;
  updatedAt: Date;
}

/**
 * Compute Node - Individual compute provider
 */
export interface ComputeNode {
  id: string;

  // Identity
  ownerAddress: string;
  name: string | null;
  description: string | null;

  // Capabilities
  cpuCores: number;
  cpuModel: string | null;
  memoryMb: number;
  storageGb: number;
  gpuCount: number;
  gpuModel: string | null;
  bandwidthMbps: number;

  // Container support
  supportsDocker: boolean;
  supportsK8s: boolean;
  supportsWasm: boolean;

  // Location
  region: string | null;
  availabilityZone: string | null;
  ipAddress: string | null;
  p2pAddress: string | null;

  // Attestation
  teeType: string | null;
  attestationQuote: Uint8Array | null;
  attestationVerifiedAt: Date | null;

  // Performance
  slaScore: number;
  uptimePercent: number;
  avgResponseTimeMs: number;
  totalTasksCompleted: number;
  totalTasksFailed: number;

  // Staking
  stakeAmount: string;
  stakeLockedUntil: Date | null;

  // Status
  status: NodeStatus;
  lastHeartbeatAt: Date | null;

  // Timestamps
  createdAt: Date;
  updatedAt: Date;
}

/**
 * Compute Offer - Order book entry from provider
 */
export interface ComputeOffer {
  id: string;

  // Relationships
  nodeId: string;
  poolId: string | null;

  // Offer details
  resourceType: ResourceType;
  amount: string;
  amountRemaining: string;
  pricePerUnit: string;

  // Duration
  minDurationHours: number;
  maxDurationHours: number;

  // SLA guarantees
  guaranteedUptime: number;
  maxLatencyMs: number | null;

  // Lifecycle
  status: OfferStatus;
  expiresAt: Date;

  // Timestamps
  createdAt: Date;
  updatedAt: Date;
}

/**
 * Pool Membership - LP position or provider stake
 */
export interface PoolMembership {
  id: string;

  // Relationships
  poolId: string;
  nodeId: string | null;
  userAddress: string;

  // Type
  role: MembershipRole;

  // Liquidity position
  liquidityShares: string;
  resourceDeposited: string;
  tokenDeposited: string;
  entryPrice: string;

  // Rewards
  pendingRewards: string;
  claimedRewards: string;
  lastRewardClaimAt: Date | null;

  // Resource commitment
  committedResources: string;
  utilizedResources: string;

  // Performance
  memberSlaScore: number;

  // Status
  isActive: boolean;
  joinedAt: Date;
  leftAt: Date | null;

  // Timestamps
  createdAt: Date;
  updatedAt: Date;
}

/**
 * Challenge - SLA verification challenge
 */
export interface Challenge {
  id: string;

  // Relationships
  nodeId: string;
  poolId: string | null;
  membershipId: string | null;

  // Challenge details
  challengeType: string;
  challengeData: Record<string, unknown>;

  // Response
  responseData: Record<string, unknown> | null;
  responseAt: Date | null;

  // Verification
  verifiedBy: string | null;
  verificationProof: Uint8Array | null;
  verifiedAt: Date | null;

  // Result
  status: ChallengeStatus;
  score: number | null;
  penaltyAmount: string;

  // Timing
  deadline: Date;

  // Timestamps
  createdAt: Date;
  updatedAt: Date;
}

/**
 * Reward - Distribution to participant
 */
export interface Reward {
  id: string;

  // Relationships
  poolId: string;
  membershipId: string;
  userAddress: string;

  // Details
  rewardType: string;
  amount: string;

  // Source
  sourceTxHash: string | null;
  sourceTradeId: string | null;

  // Epoch
  epochNumber: number | null;
  epochStart: Date | null;
  epochEnd: Date | null;

  // Claim
  isClaimed: boolean;
  claimedAt: Date | null;
  claimTxHash: string | null;

  // Timestamps
  createdAt: Date;
}

// =============================================================================
// TRADING TABLES
// =============================================================================

/**
 * Trade - Executed trade on the platform
 */
export interface Trade {
  id: string;

  // Relationships
  poolId: string;
  userAddress: string;

  // Trade details
  direction: TradeDirection;
  route: TradeRoute;
  resourceType: ResourceType;

  // Amounts
  amountIn: string;
  amountOut: string;
  effectivePrice: string;
  priceImpact: number;

  // Fees
  tradingFee: string;
  protocolFee: string;

  // Slippage
  slippageTolerance: number;
  actualSlippage: number;

  // Execution
  offerIds: string[] | null;
  txHash: string | null;

  // Timestamps
  createdAt: Date;
}

/**
 * Price Snapshot - OHLCV candle data
 */
export interface PriceSnapshot {
  id: number;
  poolId: string;
  bucketTime: Date;
  bucketInterval: string;
  openPrice: string;
  highPrice: string;
  lowPrice: string;
  closePrice: string;
  volume: string;
  tradeCount: number;
  liquidityAtClose: string | null;
  utilizationAtClose: number | null;
}

/**
 * Liquidity Event - Add/remove liquidity
 */
export interface LiquidityEvent {
  id: string;
  poolId: string;
  membershipId: string;
  userAddress: string;
  eventType: 'add' | 'remove' | 'claim_rewards';
  resourceAmount: string;
  tokenAmount: string;
  lpTokensMinted: string;
  lpTokensBurned: string;
  totalSharesAfter: string;
  poolSharePercent: number;
  txHash: string | null;
  createdAt: Date;
}

/**
 * Resource Allocation - Active compute allocation
 */
export interface ResourceAllocation {
  id: string;
  poolId: string;
  nodeId: string;
  offerId: string | null;
  consumerAddress: string;
  resourceType: ResourceType;
  allocatedAmount: string;
  pricePerUnitHour: string;
  startTime: Date;
  endTime: Date;
  actualEndTime: Date | null;
  totalCost: string;
  paidAmount: string;
  escrowAmount: string;
  status: 'pending' | 'active' | 'completed' | 'cancelled' | 'disputed';
  uptimeAchieved: number | null;
  slaMet: boolean | null;
  createdAt: Date;
  updatedAt: Date;
}

// =============================================================================
// VIEW TYPES
// =============================================================================

/**
 * Active Pool with computed metrics (v_active_pools)
 */
export interface ActivePool extends ComputePool {
  providerCount: number;
  lpCount: number;
  trades24h: number;
  uniqueTraders24h: number;
}

/**
 * User Portfolio position (v_user_portfolio)
 */
export interface UserPortfolioPosition {
  userAddress: string;
  poolId: string;
  poolName: string;
  resourceType: ResourceType;
  role: MembershipRole;
  liquidityShares: string;
  resourceDeposited: string;
  tokenDeposited: string;
  entryPrice: string;
  pendingRewards: string;
  claimedRewards: string;
  memberSlaScore: number;
  poolSharePercent: number;
  positionOpenedAt: Date;
}

/**
 * Node Performance summary (v_node_performance)
 */
export interface NodePerformance {
  nodeId: string;
  ownerAddress: string;
  name: string | null;
  status: NodeStatus;
  slaScore: number;
  uptimePercent: number;
  totalTasksCompleted: number;
  totalTasksFailed: number;
  activeOffers: number;
  totalCapacityOffered: string;
  activeAllocations: number;
  pendingChallenges: number;
  failedChallenges30d: number;
}

/**
 * Pool Leaderboard entry (v_pool_leaderboard)
 */
export interface PoolLeaderboardEntry {
  id: string;
  name: string;
  resourceType: ResourceType;
  tvl: string;
  volume24h: string;
  apr: number;
  utilization: number;
  avgSlaScore: number;
  volumeRank: number;
  tvlRank: number;
  aprRank: number;
}

// =============================================================================
// FUNCTION RETURN TYPES
// =============================================================================

/**
 * Swap output calculation result
 */
export interface SwapOutput {
  amountOut: string;
  priceImpact: number;
  tradingFee: string;
  newPrice: string;
}

/**
 * Liquidity removal calculation result
 */
export interface LiquidityRemoval {
  resourceAmount: string;
  tokenAmount: string;
  sharePercent: number;
}

/**
 * Best price routing result
 */
export interface BestPriceResult {
  poolId: string;
  expectedOutput: string;
  effectivePrice: string;
  priceImpact: number;
  useAmm: boolean;
  routeDetails: {
    source: 'amm' | 'orderbook';
    poolId: string;
    tradingFee?: string;
  };
}

// =============================================================================
// API REQUEST/RESPONSE TYPES
// =============================================================================

export interface CreatePoolRequest {
  name: string;
  slug: string;
  description?: string;
  resourceType: ResourceType;
  initialResourceAmount: string;
  initialTokenAmount: string;
  feeRate?: number;
}

export interface SwapRequest {
  poolId: string;
  direction: TradeDirection;
  amountIn: string;
  minAmountOut: string;
  slippageTolerance: number;
}

export interface AddLiquidityRequest {
  poolId: string;
  resourceAmount: string;
  tokenAmount: string;
}

export interface RemoveLiquidityRequest {
  poolId: string;
  lpTokens: string;
  minResourceAmount?: string;
  minTokenAmount?: string;
}

export interface CreateOfferRequest {
  nodeId: string;
  poolId?: string;
  resourceType: ResourceType;
  amount: string;
  pricePerUnit: string;
  minDurationHours: number;
  maxDurationHours: number;
  guaranteedUptime: number;
  maxLatencyMs?: number;
  expiresAt: Date;
}

export interface AllocateResourcesRequest {
  poolId: string;
  resourceType: ResourceType;
  amount: string;
  durationHours: number;
  maxPricePerUnit: string;
}
